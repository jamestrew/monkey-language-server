mod ast;
mod diagnostics;
mod eval;
mod lexer;
mod parser;
mod spanned;

#[cfg(test)]
mod test_util;

use std::collections::HashMap;
use std::sync::Arc;

use eval::{Env as EvalEnv, Eval, Object};
use parser::Parser;
use spanned::Spanned;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{Diagnostic, *};
use tower_lsp::{Client, LanguageServer};
use tracing::info;

#[allow(unused)]
struct Env {
    store: HashMap<String, Arc<Spanned<Object>>>,
    refs: Vec<Arc<Spanned<String>>>,
    children: Vec<Env>,
}

impl Env {}

impl From<EvalEnv> for Env {
    fn from(env: EvalEnv) -> Self {
        let env = env.take_environment();
        Self {
            store: env.store,
            refs: env.refs,
            children: env
                .children
                .into_iter()
                .map(|child| child.into())
                .collect::<Vec<_>>(),
        }
    }
}

fn analyze_source(source: &str) -> (Vec<Diagnostic>, Env) {
    let program = Parser::from_source(source).parse_program();
    let (eval_env, diags) = Eval::eval_program(program.nodes);

    let diags = diags
        .iter()
        .map(|diag| {
            Diagnostic::new(
                diag.lsp_range(),
                Some(diag.severity()),
                None,
                None,
                diag.to_string(),
                None,
                None,
            )
        })
        .collect::<Vec<_>>();

    (diags, eval_env.into())
}

pub struct Backend {
    client: Client,
    env: Arc<Mutex<Option<Env>>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            env: Default::default(),
        }
    }

    async fn on_change(&self, uri: Url, text: String, version: i32) {
        info!("on_change");
        let (diagnostics, new_env) = analyze_source(&text);
        let mut old_env = self.env.lock().await;
        *old_env = Some(new_env);

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            std::mem::take(&mut params.content_changes[0].text),
            params.text_document.version,
        )
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let _ = params;

        // get cursor position
        let _pos = params.text_document_position_params.position;
        // get eval'ed env (should be in our backend struct)
        // check env refs for under cursor ident name
        // find correct ident in our env store

        todo!()
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }
}

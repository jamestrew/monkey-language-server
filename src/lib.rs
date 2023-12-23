mod ast;
mod diagnostics;
mod eval;
mod lexer;
mod parser;
mod spanned;

#[cfg(test)]
mod test_util;

use std::sync::Arc;

use eval::{Env, Eval};
use parser::Parser;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::info;

fn analyze_source(source: &str) -> (Vec<Diagnostic>, Env) {
    let program = Parser::from_source(source).parse_program();
    let (env, diags) = Eval::eval_program(program);

    let diags = diags
        .iter()
        .map(|diag| {
            Diagnostic::new(
                diag.into(),
                Some(diag.severity()),
                None,
                None,
                diag.to_string(),
                None,
                None,
            )
        })
        .collect::<Vec<_>>();

    (diags, env)
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
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
        let pos = params.text_document_position_params.position;
        let uri = params.text_document_position_params.text_document.uri;
        let env_lock = self.env.lock().await;

        if let Some(env) = &*env_lock {
            if let Some(range) = env.find_pos_def(&pos) {
                let location = Location::new(uri, range);
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let pos = params.text_document_position.position;
        let uri = params.text_document_position.text_document.uri;
        let env_lock = self.env.lock().await;

        if let Some(env) = &*env_lock {
            if let Some(refs) = env.find_references(&pos) {
                let locations = refs
                    .iter()
                    .map(|ref_range| Location::new(uri.clone(), *ref_range))
                    .collect::<Vec<_>>();
                return Ok(Some(locations));
            }
        }
        Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let pos = params.text_document_position.position;
        let env_lock = self.env.lock().await;
        if let Some(env) = &*env_lock {
            let comps = CompletionResponse::Array(env.get_completions(&pos));
            return Ok(Some(comps));
        }
        Ok(None)
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }
}

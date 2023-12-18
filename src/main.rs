use monkey_language_server::Backend;
use tower_lsp::{LspService, Server};
use tracing::info;

#[tokio::main]
async fn main() {
    let file_appender = tracing_appender::rolling::daily("/tmp", "monkeylsp.log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

    tracing_subscriber::fmt().with_writer(non_blocking).init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    info!("starting server");
    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

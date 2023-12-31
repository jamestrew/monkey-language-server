use tower_lsp::lsp_types::Range;

use crate::diagnostics::PosDiagnostic;

pub fn input_diagnostics(input: &str, diagnostics: Vec<PosDiagnostic>) -> String {
    let errors = diagnostics
        .iter()
        .map(|err| (Range::from(err), err.to_string(), err.severity()))
        .collect::<Vec<_>>();

    let mut ret_lines: Vec<String> = Vec::new();
    for (row, line) in input.lines().enumerate() {
        ret_lines.push(line.to_string());
        for (rng, err, severity) in &errors {
            if rng.start.line as usize != row {
                continue;
            }

            let start_col = rng.start.character;
            let start = " ".repeat(start_col as usize);
            let end_at = rng.end.character - start_col - 1;
            let end = if end_at == 0 {
                "".to_string()
            } else {
                format!("{}^", "-".repeat(end_at as usize))
            };
            ret_lines.push(format!("{start}^{end} [{severity:?}] {}", err))
        }
    }

    ret_lines.join("\n")
}

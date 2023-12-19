use tower_lsp::lsp_types::Range;

use crate::diagnostics::SpannedDiagnostic;

pub fn input_diagnostics(input: &str, diagnostics: Vec<SpannedDiagnostic>) -> String {
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

            let col = rng.start.character;
            let start = " ".repeat(col as usize);
            ret_lines.push(format!("{start}^ [{severity:?}] {}", err))
        }
    }

    ret_lines.join("\n")
}

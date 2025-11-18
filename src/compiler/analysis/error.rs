use crate::compiler::tokens::DisplaySpan;

crate::define_errors! {
    AnalysisError, phase: Analysis {
        #[msg = "Type mismatch: expected {expected}, got {got}"]
        #[help = "Ensure the types match or add an explicit conversion"]
        TypeMismatch {
            expected: String,
            got: String,
            span: DisplaySpan,
        },

        #[msg = "Unresolved {what}"]
        #[help = "This identifier or symbol was not found in the current scope"]
        Unresolved {
            what: String,
            span: DisplaySpan,
        },

        #[msg = "Invalid {what}: {reason}"]
        Invalid {
            what: String,
            reason: String,
            span: DisplaySpan,
        },

        #[msg = "Unsupported {what}"]
        #[help = "This feature is not yet supported by the analyzer"]
        Unsupported {
            what: String,
            span: DisplaySpan,
        },
    }
}

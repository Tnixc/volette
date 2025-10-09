use crate::compiler::tokens::DisplaySpan;

crate::define_errors! {
    LexError, phase: Lexing {
        #[msg = "Unexpected {what}"]
        #[help = "This character or sequence is not valid at this location"]
        Unexpected {
            what: String,
            context: String,
            span: DisplaySpan,
        },

        #[msg = "Invalid {what}: {reason}"]
        #[help = "Check the syntax of this token"]
        Invalid {
            what: String,
            reason: String,
            span: DisplaySpan,
        },

        #[msg = "Unterminated {what}"]
        #[help = "This token was not properly closed"]
        Unterminated {
            what: String,
            span: DisplaySpan,
        },
    }
}

use crate::compiler::tokens::DisplaySpan;

crate::define_errors! {
    ParserError, phase: Parsing {
        #[msg = "Unexpected {what}"]
        #[help = "Check the syntax at this location"]
        Unexpected {
            what: String,
            context: String,
            span: DisplaySpan,
        },

        #[msg = "Expected {what}, got {got}"]
        #[help = "Check the syntax - the parser expected something different here"]
        Expected {
            what: String,
            got: String,
            span: DisplaySpan,
        },

        #[msg = "Invalid {what}: {reason}"]
        #[help = "This construct is not valid in the current context"]
        Invalid {
            what: String,
            reason: String,
            span: DisplaySpan,
        },

        #[msg = "Node not found: {what}"]
        #[help = "This is likely a parser bug - please report it"]
        NotFound {
            what: String,
            span: DisplaySpan,
        },
    }
}

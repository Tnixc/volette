/// # Syntax
/// ```ignore
/// define_errors! {
///     ErrorType, phase: PhaseName {
///         #[msg = "Error message with {field} interpolation"]
///         #[help = "Optional help text with {field} interpolation"]
///         #[note = "Optional note text"]
///         VariantName {
///             field1: Type1,
///             field2: Type2,
///             span: DisplaySpan,  // required for all variants
///         },
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_errors {
    (
        $error_name:ident, phase: $phase:ident {
            $(
                #[msg = $msg:expr]
                $(#[help = $help:expr])?
                $(#[note = $note:expr])?
                $variant:ident {
                    $($field:ident: $field_ty:ty),* $(,)?
                }
            ),* $(,)?
        }
    ) => {
        #[derive(Debug, Clone)]
        pub enum $error_name {
            $(
                $variant {
                    $($field: $field_ty),*
                }
            ),*
        }

        impl $error_name {
            pub fn span(&self) -> &$crate::compiler::tokens::DisplaySpan {
                match self {
                    $(
                        Self::$variant { span, .. } => span,
                    )*
                }
            }
        }

        impl std::fmt::Display for $error_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant { $($field),* } => {
                            let mut result = $msg.to_string();
                            $(
                                result = result.replace(
                                    &format!("{{{}}}", stringify!($field)),
                                    &format!("{}", $field)
                                );
                            )*
                            write!(f, "{}", result)
                        }
                    )*
                }
            }
        }

        impl std::error::Error for $error_name {}

        #[allow(unused_mut)]
        impl From<$error_name> for $crate::compiler::error::Diagnostic {
            fn from(err: $error_name) -> Self {
                use $crate::compiler::error::{Diagnostic, CompilerPhase};

                match &err {
                    $(
                        $error_name::$variant { $($field),* } => {
                            let span = err.span();

                            let mut message = $msg.to_string();
                            $(
                                message = message.replace(
                                    &format!("{{{}}}", stringify!($field)),
                                    &format!("{}", $field)
                                );
                            )*

                            let mut diagnostic = Diagnostic::error(
                                message,
                                CompilerPhase::$phase
                            ).with_span(span.clone());

                            // add help if provided
                            $(
                                diagnostic = diagnostic.with_help($help);
                            )?

                            // add note if provided
                            $(
                                diagnostic = diagnostic.with_note($note);
                            )?

                            diagnostic
                        }
                    )*
                }
            }
        }
    };
}

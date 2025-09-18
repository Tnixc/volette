pub mod compiler;

trait SafeConvert<T> {
    fn safe(self) -> T;
}

impl<T> SafeConvert<T> for Option<T> {
    fn safe(self) -> T {
        match self {
            Some(value) => value,
            None => panic!("Called safe() on None value"),
        }
    }
}

impl<T, E: std::fmt::Debug> SafeConvert<T> for Result<T, E> {
    fn safe(self) -> T {
        match self {
            Ok(value) => value,
            Err(error) => panic!("Called safe() on Err value: {:?}", error),
        }
    }
}

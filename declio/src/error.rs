use std::fmt;

/// Encoding and decoding errors.
pub struct Error {
    message: String,
    source: Option<Box<dyn std::error::Error + 'static>>,
}

impl Error {
    /// `Creates a new `Error` with the given message.
    pub fn new<S>(message: S) -> Self
    where
        S: ToString,
    {
        Self {
            message: message.to_string(),
            source: None,
        }
    }

    /// Creates a new `Error` with the given error value as the source.
    pub fn wrap<E>(error: E) -> Self
    where
        E: std::error::Error + 'static,
    {
        Self {
            message: error.to_string(),
            source: Some(Box::new(error)),
        }
    }

    /// Creates a new `Error` with a custom message and a source error value.
    pub fn with_context<S, E>(message: S, error: E) -> Self
    where
        S: ToString,
        E: std::error::Error + 'static,
    {
        Self {
            message: message.to_string(),
            source: Some(Box::new(error)),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // defer to Display
        write!(f, "{}", self)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(Box::as_ref)
    }
}

macro_rules! convert_error {
    ($($t:ty,)*) => {$(
        impl From<$t> for Error {
            fn from(error: $t) -> Self {
                Self::wrap(error)
            }
        }
    )*}
}

convert_error! {
    std::convert::Infallible,
    std::array::TryFromSliceError,
    std::char::CharTryFromError,
    std::char::DecodeUtf16Error,
    std::io::Error,
    std::num::TryFromIntError,
    std::str::Utf8Error,
    std::string::FromUtf8Error,
    std::string::FromUtf16Error,
}

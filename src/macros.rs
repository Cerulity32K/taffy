#[rustfmt::skip]
#[macro_export]
macro_rules! Tok {
    (~) => { $crate::tok::punct::Tilde };
    (!) => { $crate::tok::punct::Exclamation };
    (@) => { $crate::tok::punct::At };
    ($) => { $crate::tok::punct::Dollar };
    (%) => { $crate::tok::punct::Percent };
    (^) => { $crate::tok::punct::Caret };
    (&) => { $crate::tok::punct::Ampersand };
    (*) => { $crate::tok::punct::Asterisk };
    (**) => { $crate::tok::punct::Asterisk2 };
    (-) => { $crate::tok::punct::Hyphen };
    (=) => { $crate::tok::punct::Equal };
    (+) => { $crate::tok::punct::Plus };
    (;) => { $crate::tok::punct::Semicolon };
    (:) => { $crate::tok::punct::Colon };
    (b/) => { $crate::tok::punct::Backslash };
    (|) => { $crate::tok::punct::Pipe };
    (,) => { $crate::tok::punct::Comma };
    (.) => { $crate::tok::punct::Period };
    (<) => { $crate::tok::punct::OpeningAngle };
    (>) => { $crate::tok::punct::ClosingAngle };
    (/) => { $crate::tok::punct::Slash };
    (?) => { $crate::tok::punct::Question };
    (->) => { $crate::tok::punct::ThinArrow };
}

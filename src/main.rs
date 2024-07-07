use std::fs::File;

mod char_reader;
mod error_stream;
mod primitive;
mod builtin;
mod span;
mod strings;
mod unknown;
mod stages {
    pub mod tok;
    pub mod ur;
    pub mod vr;
}

fn main() {
    let file = File::open("example.sil").unwrap();
    let char_reader = char_reader::IoCharReader::<1024, _>::new(file);
    let strings = strings::Strings::new();
    let tokens = stages::tok::Tokens::of(char_reader, &strings);
    let errors = error_stream::ErrorStream::new();
    dbg!(stages::ur::Ur::of(tokens, errors).unwrap());

    println!("Hello, world!");
}

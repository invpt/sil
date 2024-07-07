use std::io::{self, Read};

pub trait CharReader {
    fn next(&mut self) -> io::Result<Option<(usize, char)>>;
    fn peek(&mut self) -> io::Result<Option<(usize, char)>>;
}

pub struct CharReaderSaver<'r, R> {
    chars: &'r mut R,
    saved: String,
}

impl<'r, R: CharReader> CharReaderSaver<'r, R> {
    pub fn new(chars: &'r mut R) -> CharReaderSaver<'r, R> {
        CharReaderSaver {
            chars,
            saved: String::new(),
        }
    }

    pub fn finish(self) -> String {
        self.saved
    }
}

impl<'r, R: CharReader> CharReader for CharReaderSaver<'r, R> {
    fn next(&mut self) -> io::Result<Option<(usize, char)>> {
        if let Some((idx, ch)) = self.chars.next()? {
            self.saved.push(ch);

            Ok(Some((idx, ch)))
        } else {
            Ok(None)
        }
    }

    fn peek(&mut self) -> io::Result<Option<(usize, char)>> {
        self.chars.peek()
    }
}

pub struct IoCharReader<const BUF_SIZE: usize, R> {
    read: R,
    cursor: usize,
    buf: [u8; BUF_SIZE],
    len: usize,
    overflow: usize,
    index: usize,
    peek: Option<(usize, char)>,
}

impl<const BUF_SIZE: usize, R: Read> IoCharReader<BUF_SIZE, R> {
    pub const fn new(read: R) -> IoCharReader<BUF_SIZE, R> {
        IoCharReader {
            read,
            cursor: 0,
            buf: [0; BUF_SIZE],
            len: 0,
            overflow: 0,
            index: 0,
            peek: None,
        }
    }

    fn fill_buffer(&mut self) -> io::Result<()> {
        // reset cursor to zero for new buffer
        self.cursor = 0;
        // copy overflow to beginning
        self.buf.copy_within(self.len..self.len + self.overflow, 0);
        // read input
        self.len = self.read.read(&mut self.buf[self.overflow..])? + self.overflow;

        if self.len == 0 {
            return Ok(());
        }

        let buf = &self.buf[..self.len];

        match std::str::from_utf8(buf) {
            Ok(_) => Ok(()),
            Err(e) => {
                if e.error_len().is_some() {
                    Err(io::Error::from(io::ErrorKind::InvalidData))?
                }

                self.overflow = self.len - e.valid_up_to();
                self.len = e.valid_up_to();

                if self.len == 0 {
                    panic!("BUF_SIZE is too small!")
                }

                Ok(())
            }
        }
    }
}

impl<const BUF_SIZE: usize, R: Read> CharReader for IoCharReader<BUF_SIZE, R> {
    fn next(&mut self) -> io::Result<Option<(usize, char)>> {
        if let Some(peek) = self.peek.take() {
            return Ok(Some(peek));
        }

        if self.cursor >= self.len {
            self.fill_buffer()?
        }

        // SAFETY: We verify the validity of the bytes in `fill_buffer`.
        let s = unsafe { std::str::from_utf8_unchecked(&self.buf[self.cursor..self.len]) };

        if let Some(ch) = s.chars().next() {
            self.cursor += ch.len_utf8();

            let index = self.index;
            self.index += ch.len_utf8();
            Ok(Some((index, ch)))
        } else {
            Ok(None)
        }
    }

    fn peek(&mut self) -> io::Result<Option<(usize, char)>> {
        if let Some(peek) = self.peek {
            Ok(Some(peek))
        } else if let Some(peek) = self.next()? {
            self.peek = Some(peek);
            Ok(Some(peek))
        } else {
            Ok(None)
        }
    }
}

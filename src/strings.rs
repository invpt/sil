use core::{
    cell::UnsafeCell,
    hash::Hash,
};
use std::ops::Deref;

use rustc_hash::FxHashSet;

use crate::builtin::Builtin;

#[derive(Debug, Clone, Copy)]
pub struct Intern<'s> {
    text: &'s str,
}

impl Intern<'static> {
    pub fn new(text: &'static str) -> Intern<'static> {
        Intern { text }
    }
}

impl<'s> Intern<'s> {
    pub fn text(&self) -> &'s str {
        self.text
    }
}

impl<'s> Deref for Intern<'s> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.text
    }
}

impl<'s> PartialEq for Intern<'s> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.text, other.text) && self.text.len() == other.text.len()
    }
}

impl<'s> Eq for Intern<'s> {}

impl<'s> Hash for Intern<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.text as *const str as *const u8 as usize);
        state.write_usize(self.text.len());
    }
}

#[derive(Debug)]
pub struct Strings {
    strings: UnsafeCell<FxHashSet<&'static str>>,
}

impl Strings {
    pub fn new() -> Strings {
        Strings {
            strings: UnsafeCell::new(FxHashSet::default()),
        }
    }

    pub fn intern<'a>(&'a self, s: Box<str>) -> Intern<'a> {
        if let Some(builtin) = Builtin::VALUES.iter().find(|b| b.name().text == &*s) {
            return builtin.name()
        }

        let to_intern = Box::leak(s) as &'static str;
        let strings = unsafe { &mut *self.strings.get() };
        if let Some(interned) = strings.get(to_intern) {
            // drop the string we were going to intern
            // SAFETY: we just got this reference from Box::leak
            unsafe {
                drop(Box::from_raw(to_intern as *const str as *mut str));
            }

            Intern {
                text: interned
            }
        } else {
            strings.insert(to_intern);
            Intern {
                text: to_intern
            }
        }
    }
}

impl Drop for Strings {
    fn drop(&mut self) {
        let strings = std::mem::take(self.strings.get_mut());
        for interned in strings {
            // SAFETY: all references to the string are gone now since we
            //         have a mutable reference to Strings
            unsafe {
                drop(Box::from_raw(interned as *const str as *mut str));
            }
        }
    }
}

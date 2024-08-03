use crate::strings::Intern;

#[repr(usize)]
#[derive(Clone, Copy)]
pub enum Builtin {
    S32,
    F32,
    Str,
    True,
    False,
}

impl Builtin {
    pub const COUNT: usize = 5;
    pub const VALUES: [Builtin; Builtin::COUNT] = [Builtin::S32, Builtin::F32, Builtin::Str, Builtin::True, Builtin::False];

    pub fn name(self) -> Intern<'static> {
        const NAMES: [&'static str; Builtin::COUNT] = ["s32", "f32", "str", "true", "false"];

        Intern::new(NAMES[self as usize])
    }

    pub fn index(self) -> usize {
        self as usize
    }
}
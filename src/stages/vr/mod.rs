use super::ur::Symbol;

pub struct Vr<'s> {
    pub procs: Box<[VrProc<'s>]>,
}

pub struct VrProc<'s> {
    pub name: Symbol<'s>,
}
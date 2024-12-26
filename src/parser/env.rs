use std::ops::{Deref, DerefMut};

use crate::{
    ast::{IdentLocation, StackIndex, Upvalue, UpvalueIndex},
    util::nonempty_vec::NEVec,
};

type Scope = Vec<Local>;

#[derive(Debug)]
pub struct Env {
    /// A list of call frames, which is a list of blocks, each with a list
    /// of locals.
    frames: NEVec<CallFrame>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env {
            frames: NEVec::default(),
        };

        crate::interperter::stub_stdlib(&mut env);
        // new scope for non-stdlib
        env.frames.last_mut().scopes.push(Vec::default());

        env
    }

    pub fn create_scope(&mut self) -> ScopeGuard<'_> {
        ScopeGuard::new(self)
    }

    pub fn new_frame(&mut self, name: String) -> FrameGuard<'_> {
        FrameGuard::new(self, name)
    }

    pub fn declare_local(&mut self, name: String) {
        let local = Local { name };
        self.frames.last_mut().scopes.last_mut().push(local);
    }

    // Look for the most deeply-scoped local with the given name.
    pub fn resolve(&mut self, name: &str) -> Option<IdentLocation> {
        // can always subtract 1 from len b/c it is non-empty (a NEVec)
        self.resolve_with_frame(name, self.frames.len() - 1)
    }

    fn resolve_with_frame(
        // Mutable so upvalues can be recorded
        &mut self,
        name: &str,
        frame_index: usize,
    ) -> Option<IdentLocation> {
        self.resolve_in_stack_with_frame(name, frame_index)
            .map(IdentLocation::Stack)
            .or_else(|| {
                self.resolve_upvalue_with_frame(name, frame_index)
                    .map(IdentLocation::Upvalue)
            })
    }

    fn resolve_in_stack_with_frame(
        &mut self,
        name: &str,
        frame_index: usize,
    ) -> Option<StackIndex> {
        // This was much more annoying than I thought it would be
        // <https://users.rust-lang.org/t/cant-flatten-enumerate-and-then-reverse-iterator/122931>

        let call_frame = &self.frames[frame_index];
        let stack = call_frame.scopes.iter().flatten();
        let len = stack.clone().count();
        let indexes_rev = (0..len).map(StackIndex).rev();

        let mut stack = stack.rev().zip(indexes_rev);
        stack.find(|(local, _)| local.name == name).map(|(_, i)| i)
    }

    fn resolve_upvalue_with_frame(
        &mut self,
        name: &str,
        frame_index: usize,
    ) -> Option<UpvalueIndex> {
        let parent_index = frame_index.checked_sub(1)?;

        // make upvalue
        let location = self.resolve_with_frame(name, parent_index)?;

        // This is the upvalue that goes in the func metadata
        let upvalue = Upvalue {
            target: location,
            dbg_name: name.to_string(),
        };

        let current_frame = &mut self.frames[frame_index];
        let upvalue_index = UpvalueIndex(current_frame.upvalues.len());
        current_frame.upvalues.push(upvalue);

        Some(upvalue_index)
    }
}

#[derive(Debug, Default)]
pub struct CallFrame {
    scopes: NEVec<Scope>,
    upvalues: Vec<Upvalue>,
}

#[clippy::has_significant_drop]
pub struct ScopeGuard<'a>(&'a mut Env);
impl<'a> ScopeGuard<'a> {
    fn new(env: &'a mut Env) -> Self {
        env.frames.last_mut().scopes.push(Vec::new());
        ScopeGuard(env)
    }
}
impl Deref for ScopeGuard<'_> {
    type Target = Env;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
impl DerefMut for ScopeGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
impl Drop for ScopeGuard<'_> {
    fn drop(&mut self) {
        self.frames.last_mut().scopes.pop_unchecked();
    }
}

#[clippy::has_significant_drop]
pub struct FrameGuard<'a>(&'a mut Env);
impl<'a> FrameGuard<'a> {
    fn new(env: &'a mut Env, name: String) -> Self {
        env.frames.push(CallFrame::default());

        // declare the function name in locals slot 0
        // this will allow for recursion
        env.declare_local(name);

        FrameGuard(env)
    }

    // Take `self` so that upvalues must be up-to-date.
    pub fn upvalues(mut self) -> Vec<Upvalue> {
        let upvalues = self.frames.pop_unchecked().upvalues;
        // prevent destructor from double-popping
        std::mem::forget(self);
        upvalues
    }
}
impl Deref for FrameGuard<'_> {
    type Target = Env;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
impl DerefMut for FrameGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
impl Drop for FrameGuard<'_> {
    fn drop(&mut self) {
        self.frames.pop_unchecked();
    }
}

#[derive(Debug)]
struct Local {
    name: String,
}

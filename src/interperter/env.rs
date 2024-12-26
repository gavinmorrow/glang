use std::ops::{Deref, DerefMut};

use crate::ast::{IdentLocation, StackIndex, Upvalue, UpvalueIndex};

use super::{Func, Value};

pub struct Env {
    locals_stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env {
            locals_stack: Vec::new(),
            call_frames: Vec::new(),
        };
        super::stdlib::define_stdlib(&mut env);
        env
    }

    pub fn get(&self, i: IdentLocation) -> &Value {
        match i {
            IdentLocation::Stack(i) => self.get_local_from_frame(i, self.call_frames.last()),
            IdentLocation::Upvalue(upvalue_index) => {
                if let Some(frame_index) = (self.call_frames.len()).checked_sub(1) {
                    let frame = &self.call_frames[frame_index];

                    let Func::User(func) = &frame.func else {
                        panic!("call frame should not be native func");
                    };

                    &func.upvalues[upvalue_index.0]
                } else {
                    // a global
                    &self.locals_stack[upvalue_index.0]
                }
            }
        }
    }

    pub fn resolve_upvalue(&self, upvalue: Upvalue) -> &Value {
        let current_frame = self.call_frames.last();
        match upvalue.target {
            IdentLocation::Stack(stack_index) => {
                self.get_local_from_frame(stack_index, current_frame)
            }
            IdentLocation::Upvalue(upvalue_index) => {
                self.get_upvalue_from_frame(upvalue_index, current_frame)
            }
        }
    }

    fn get_local_from_frame(&self, StackIndex(i): StackIndex, frame: Option<&CallFrame>) -> &Value {
        let stack_offset = frame.map(|f| f.stack_offset).unwrap_or(0);
        self.locals_stack.get(stack_offset + i).unwrap_or_else(|| {
            panic!(
                "stack location should be valid. i: {i}, offset: {}, stack: {:#?}",
                stack_offset, self.locals_stack
            )
        })
    }

    fn get_upvalue_from_frame<'f>(
        &self,
        UpvalueIndex(i): UpvalueIndex,
        frame: Option<&'f CallFrame>,
    ) -> &'f Value {
        let Func::User(func) = &frame.expect("frame should exist for upvalue").func else {
            panic!("call frame should not be native func");
        };
        &func.upvalues[i]
    }

    pub fn define(&mut self, value: Value) {
        self.locals_stack.push(value);
    }

    pub fn new_frame(&mut self, func: Func) -> FrameGuard {
        FrameGuard::new(self, func)
    }
}

#[derive(Debug)]
pub struct CallFrame {
    func: Func,
    stack_offset: usize,
}

#[clippy::has_significant_drop]
pub struct FrameGuard<'a>(&'a mut Env);
impl<'a> FrameGuard<'a> {
    fn new(env: &'a mut Env, func: Func) -> Self {
        let stack_offset = env.locals_stack.len();
        let call_frame = CallFrame {
            func: func.clone(),
            stack_offset,
        };
        env.call_frames.push(call_frame);

        // put func in slot 0, to allow for recursion
        env.define(Value::Func(func));

        FrameGuard(env)
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
        let offset = self
            .call_frames
            .last()
            .expect("env should have call frame")
            .stack_offset;
        self.locals_stack.drain(offset..);

        self.call_frames
            .pop()
            .expect("env should have call frame to pop");
    }
}

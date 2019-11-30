extern crate llvm_sys;

use std::collections::BTreeMap;

use llvm_sys::prelude::*;

use CompileError::*;

use crate::treebuild::DecafTreeBuilder;

use self::llvm_sys::core::*;

pub const ADDRESS_SPACE_GENERIC: ::libc::c_uint = 0;
pub const ADDRESS_SPACE_GLOBAL: ::libc::c_uint = 1;
pub const ADDRESS_SPACE_SHARED: ::libc::c_uint = 3;
pub const ADDRESS_SPACE_CONST: ::libc::c_uint = 4;
pub const ADDRESS_SPACE_LOCAL: ::libc::c_uint = 5;
pub const IO_PUT_CHAR_NAME: *const ::libc::c_char = b"IO_putChar".as_ptr() as *const _;
pub const IO_PUT_STRING_NAME: *const ::libc::c_char = b"IO_putString".as_ptr() as *const _;
pub const IO_PUT_INT_NAME: *const ::libc::c_char = b"IO_putInt".as_ptr() as *const _;
pub const IO_GET_CHAR_NAME: *const ::libc::c_char = b"IO_getChar".as_ptr() as *const _;
pub const IO_GET_INT_NAME: *const ::libc::c_char = b"IO_getInt".as_ptr() as *const _;
pub const IO_GET_LINE_NAME: *const ::libc::c_char = b"IO_getLine".as_ptr() as *const _;
pub const IO_PEEK_NAME: *const ::libc::c_char = b"IO_peek".as_ptr() as *const _;

macro_rules! char_type {
      () => { unsafe {
          LLVMInt8Type()
      }}
    }

macro_rules! int_type {
      () => { unsafe {
          LLVMInt64Type()
      }}
    }

macro_rules! i32_type {
      () => { unsafe {
          LLVMInt32Type()
      }}
}

macro_rules! bool_type {
      () => { unsafe {
          LLVMInt1Type()
      }}
    }

macro_rules! void_type {
      () => { unsafe {
          LLVMVoidType()
      }}
    }

macro_rules! char_ptr_type {
      ($cstrct:expr) => { unsafe {
          LLVMPointerType(
                    LLVMInt8TypeInContext($cstrct.ctx), ADDRESS_SPACE_GENERIC)
      }}
}

macro_rules! name {
      ($id:expr) => { $id.as_ptr() as *const _ }
}

macro_rules! indices {
    ($($arg:tt)*) => ({
        vec![$($arg)*].iter().map(|v| LLVMConstInt(i32_type!(), *v, 0)).collect::<Vec<LLVMValueRef>>().as_slice().as_ptr() as *mut _
    })
}

macro_rules! params {
    ($($arg:tt)*) => ({
        vec![$($arg)*].as_slice().as_ptr() as *mut _
    })
}

pub enum CodeValue {
    V(LLVMValueRef),
    T(LLVMTypeRef),
}

pub enum CompileError {
    NotImplemented,
}

pub type GenCodeResult = Result<CodeValue, CompileError>;

pub struct CompilerConstruct {
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,
    types_map: BTreeMap<String, LLVMTypeRef>,
}

impl CompilerConstruct {
    fn get_llvm_type_by_name(&self, name: &str) -> Option<LLVMTypeRef> {
        println!("hello");
        match self.types_map.get(name) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }
}

trait CodeGen {
    fn gencode(&self, construct: &CompilerConstruct) -> GenCodeResult;
}

impl CodeGen for DecafTreeBuilder {
    fn gencode(&self, construct: &CompilerConstruct) -> GenCodeResult {
        // gencode for builtin class.
        {
            // Object, nothing

            // String
            // a field called str
            // class String {
            //     private char * str
            // }
            unsafe {
                let str_type = char_ptr_type!(construct);
                let mut field_types = [str_type];
                let string_type = LLVMStructType((&mut field_types[0]) as *mut _, 1, 0);


                // TODO: Associate every instance of String class with the type and initialize them with string literals
            }

            // IO
            // No fields, a bunch of methods
            // All methods are function calls to a runtime library written in C
            unsafe {
                // putChar
                {
                    // Interface to Runtime library
                    let put_char_inner_ty = LLVMFunctionType(void_type!(), params!(char_type!()), 1, 0);
                    let put_char_inner_val = LLVMAddFunction(construct.module, IO_PUT_CHAR_NAME, put_char_inner_ty);

                    // Called by decaf code
                    let put_char_ty = LLVMFunctionType(void_type!(), params!(char_type!()), 1, 0);
                    let put_char_val = LLVMAddFunction(construct.module, name!(b"putChar"), put_char_ty);
                    let put_char_bb = LLVMAppendBasicBlockInContext(construct.ctx, put_char_val, name!(b"putChar_main"));
                    LLVMPositionBuilderAtEnd(construct.builder, put_char_bb);
                    let char_param_val = LLVMGetFirstParam(put_char_val);
                    LLVMBuildCall2(construct.builder, void_type!(), put_char_inner_val, params!(char_param_val), 1, name!(b"invoke_put_char_inner"));
                }

                // putString
                {
                    // Interface to Runtime library
                    let put_string_inner_ty = LLVMFunctionType(void_type!(), params!(char_ptr_type!(construct)), 1, 0);
                    let put_string_inner_val = LLVMAddFunction(construct.module, IO_PUT_STRING_NAME, put_string_inner_ty);

                    // Wrapper method
                    let put_string_ty = LLVMFunctionType(void_type!(), params!(construct.get_llvm_type_by_name("String").unwrap()), 1, 0);
                    let put_string_val = LLVMAddFunction(construct.module, name!(b"putString"), put_string_inner_ty);
                    let put_string_bb = LLVMAppendBasicBlockInContext(construct.ctx, put_string_val, name!(b"putString_main"));
                    LLVMPositionBuilderAtEnd(construct.builder, put_string_bb);
                    let string_param_val = LLVMGetFirstParam(put_string_val);
                    // Extract the str field
                    let str_val = LLVMBuildGEP2(construct.builder, char_ptr_type!(construct), string_param_val, indices!(0, 0), 2, name!(b"gep_str_from_String"));
                    // Call the runtime
                    LLVMBuildCall2(construct.builder, void_type!(), put_string_inner_val, params!(str_val), 1, name!(b"invoke_put_string_inner"));
                }


                // putInt
                let put_int_ty = LLVMFunctionType(void_type!(), &mut int_type!() as *mut _, 1, 0);
            }

            //
        }

        Err(NotImplemented)
    }
}


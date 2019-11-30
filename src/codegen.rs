extern crate llvm_sys;
use std::collections::BTreeMap;
use std::ptr;
use std::ffi::CString;
use llvm_sys::prelude::*;
use CompileError::*;
use crate::treebuild::DecafTreeBuilder;
use self::llvm_sys::core::*;
use self::decaf;

pub const ADDRESS_SPACE_GENERIC: ::libc::c_uint = 0;
pub const ADDRESS_SPACE_GLOBAL: ::libc::c_uint = 1;
pub const ADDRESS_SPACE_SHARED: ::libc::c_uint = 3;
pub const ADDRESS_SPACE_CONST: ::libc::c_uint = 4;
pub const ADDRESS_SPACE_LOCAL: ::libc::c_uint = 5;
pub const IO_PUT_CHAR_NAME: *const ::libc::c_char = b"IO_putChar_inner".as_ptr() as *const _;
pub const IO_PUT_STRING_NAME: *const ::libc::c_char = b"IO_putString_inner".as_ptr() as *const _;
pub const IO_PUT_INT_NAME: *const ::libc::c_char = b"IO_putInt_inner".as_ptr() as *const _;
pub const IO_GET_CHAR_NAME: *const ::libc::c_char = b"IO_getChar_inner".as_ptr() as *const _;
pub const IO_GET_INT_NAME: *const ::libc::c_char = b"IO_getInt_inner".as_ptr() as *const _;
pub const IO_GET_LINE_NAME: *const ::libc::c_char = b"IO_getLine_inner".as_ptr() as *const _;
pub const IO_PEEK_NAME: *const ::libc::c_char = b"IO_peek_inner".as_ptr() as *const _;

macro_rules! char_type {
      () => {
          LLVMInt8Type()
      }
    }

macro_rules! int_type {
      () => {
          LLVMInt64Type()
      }
    }

macro_rules! i32_type {
      () => {
          LLVMInt32Type()
      }
}

macro_rules! bool_type {
      () => {
          LLVMInt1Type()
      }
    }

macro_rules! void_type {
      () => {
          LLVMVoidType()
      }
    }

macro_rules! null {
      () => { ptr::null::<LLVMTypeRef>() as *mut _ }
}

macro_rules! char_ptr_type {
      ($cstrct:expr) => {
          LLVMPointerType(
                    LLVMInt8TypeInContext($cstrct.ctx), ADDRESS_SPACE_GENERIC)
      }
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

macro_rules! str_to_cstr {
	($s: expr) => {
		CString::new($s).unwrap().to_bytes_with_nul().as_ptr() as *const _
	}
}

pub enum CodeValue {
    V(LLVMValueRef),
    T(LLVMTypeRef),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    NotImplemented,
    DuplicatedType,
    DuplicatedFunction,
}

pub type GenCodeResult = Result<CodeValue, CompileError>;

pub struct CompilerConstruct {
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,
    types_map: BTreeMap<String, LLVMTypeRef>,
    functions_map: BTreeMap<String, LLVMValueRef>,
}

// TODO: implement new method for CompilerConstruct

// Our function naming convention is simple: <ClassName> + "_" + <MethodName>

impl CompilerConstruct {

	fn new(module_name: &str, target_triple: Option<String>) -> Self {
		let module_name_char_ptr = str_to_cstr(module_name);
		let llvm_module;
		unsafe {
			llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);
		}

		let target_triple_cstring = if let Some(target_triple) = target_triple {
			CString::new(target_triple).unwrap()
		} else {
			get_default_target_triple()
		};

		// This is necessary for maximum LLVM performance, see
		// http://llvm.org/docs/Frontend/PerformanceTips.html
		unsafe {
			LLVMSetTarget(llvm_module, target_triple_cstring.as_ptr() as *const _);
		}


	}

    fn get_llvm_type_by_name(&self, name: &str) -> Option<LLVMTypeRef> {
        match self.types_map.get(name.into()) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }

    fn add_llvm_type(&mut self, name: &str, ty: LLVMTypeRef) -> Result<(), CompileError> {
        match self.get_llvm_type_by_name(name) {
            None => {
                self.types_map.insert(name.into(), ty);
                Ok(())
            }
            Some(_) => Err(DuplicatedType)
        }
    }

    fn get_function_by_name(&self, name: &str) -> Option<LLVMValueRef> {
        match self.functions_map.get(name.into()) {
            None => None,
            Some(f) => Some(f.clone())
        }
    }

    fn add_function(&mut self, name: &str, f: LLVMValueRef) -> Result<(), CompileError> {
        match self.get_function_by_name(name) {
            None => {
                self.functions_map.insert(name.into(), f);
                Ok(())
            }
            Some(_) => Err(DuplicatedFunction)
        }
    }

	fn new_string(&self, s: &str) -> LLVMValueRef {
		// Create a String class and return the value of it
		unsafe {
            // Allocate memory
            let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String").unwrap(), name!(b"malloc_string"));
            // Copy string
            let s_val = LLVMConstStringInContext(self.ctx, str_to_cstr(s), s.len() as u32, 1);
            let str_addr = LLVMBuildGEP2(self.builder, char_ptr_type!(self), string_val, indices!(0, 0), 2, name!(b"gep_str_from_String"));
            LLVMBuildStore(self.builder, s_val, str_addr);
            string_val
		}
	}

    fn gen_builtin(&mut self) {
        // Object, nothing

        // String
        // a field called str
        // class String {
        //     private char * str
        // }
        unsafe {
            let str_type = char_ptr_type!(self);
            let mut field_types = [str_type];
            let string_type = LLVMStructType((&mut field_types[0]) as *mut _, 1, 0);
            self.add_llvm_type("String", string_type).unwrap();
            // TODO: Associate every instance of String class with the type and initialize them with string literals
        }


        // 1. Store functions
        // 2. When we see String literals expressions, we call new_string(), which return an LLVMValue of a String instance
        // 3. Implement gencode for classes, expressions and statements


        // IO
        // No fields, a bunch of methods
        // All methods are function calls to a runtime library written in C
        unsafe {
            // putChar
            {
                // Interface of Runtime
                let put_char_inner_ty = LLVMFunctionType(void_type!(), params!(char_type!()), 1, 0);
                let put_char_inner_val = LLVMAddFunction(self.module, IO_PUT_CHAR_NAME, put_char_inner_ty);

                // Called by decaf code
                let put_char_ty = LLVMFunctionType(void_type!(), params!(char_type!()), 1, 0);
                let put_char_val = LLVMAddFunction(self.module, name!(b"IO_putChar"), put_char_ty);
                let put_char_bb = LLVMAppendBasicBlockInContext(self.ctx, put_char_val, name!(b"IO_putChar_main"));
                LLVMPositionBuilderAtEnd(self.builder, put_char_bb);
                let char_param_val = LLVMGetFirstParam(put_char_val);
                LLVMBuildCall2(self.builder, void_type!(), put_char_inner_val, params!(char_param_val), 1, name!(b"invoke_put_char_inner"));

                self.add_function("IO_putChar", put_char_val).unwrap();
            }

            // putString
            {
                // Interface of Runtime
                let put_string_inner_ty = LLVMFunctionType(void_type!(), params!(char_ptr_type!(self)), 1, 0);
                let put_string_inner_val = LLVMAddFunction(self.module, IO_PUT_STRING_NAME, put_string_inner_ty);

                // Wrapper method
                let put_string_ty = LLVMFunctionType(void_type!(), params!(self.get_llvm_type_by_name("String").unwrap()), 1, 0);
                let put_string_val = LLVMAddFunction(self.module, name!(b"IO_putString"), put_string_ty);
                let put_string_bb = LLVMAppendBasicBlockInContext(self.ctx, put_string_val, name!(b"IO_putString_main"));
                LLVMPositionBuilderAtEnd(self.builder, put_string_bb);
                let string_param_val = LLVMGetFirstParam(put_string_val);
                // Extract the str field
                let str_addr = LLVMBuildGEP2(self.builder, char_ptr_type!(self), string_param_val, indices!(0, 0), 2, name!(b"gep_str_from_String"));
                // Load the pointer address
                let str_val = LLVMBuildLoad2(self.builder, char_ptr_type!(self), str_addr, name!(b"load_str_val"));
                // Call the runtime
                LLVMBuildCall2(self.builder, void_type!(), put_string_inner_val, params!(str_val), 1, name!(b"invoke_put_string_inner"));

                self.add_function("IO_putString", put_string_val).unwrap();
            }

            // putInt
            {
                // Interface of Runtime
                let put_int_inner_ty = LLVMFunctionType(void_type!(), params!(int_type!()), 1, 0);
                let put_int_inner_val = LLVMAddFunction(self.module, IO_PUT_INT_NAME, put_int_inner_ty);

                // Wrapper method
                let put_int_ty = LLVMFunctionType(void_type!(), params!(int_type!()), 1, 0);
                let put_int_val = LLVMAddFunction(self.module, name!(b"IO_putInt"), put_int_ty);
                let put_int_bb = LLVMAppendBasicBlockInContext(self.ctx, put_int_val, name!(b"IO_putInt_main"));
                LLVMPositionBuilderAtEnd(self.builder, put_int_bb);
                let int_param_val = LLVMGetFirstParam(put_int_val);
                LLVMBuildCall2(self.builder, void_type!(), put_int_inner_val, params!(int_param_val), 1, name!(b"invoke_putInt_inner"));

                self.add_function("IO_putInt", put_int_val).unwrap();
            }

            // getChar
            {
                // Interface of Runtime
                let get_char_inner_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let get_char_inner_val = LLVMAddFunction(self.module, IO_GET_CHAR_NAME, get_char_inner_ty);

                // Called by decaf code
                let get_char_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let get_char_val = LLVMAddFunction(self.module, name!(b"IO_getChar"), get_char_ty);
                let get_char_bb = LLVMAppendBasicBlockInContext(self.ctx, get_char_val, name!(b"IO_getChar_main"));
                LLVMPositionBuilderAtEnd(self.builder, get_char_bb);
                LLVMBuildCall2(self.builder, char_type!(), get_char_inner_val, null!(), 0, name!(b"invoke_get_char_inner"));

                self.add_function("IO_getChar", get_char_val).unwrap();
            }

            // getLine
            {
                // Interface of Runtime
                let get_line_inner_ty = LLVMFunctionType(char_ptr_type!(self), null!(), 0, 0);
                let get_line_inner_val = LLVMAddFunction(self.module, IO_GET_LINE_NAME, get_line_inner_ty);

                // Called by decaf code
                let get_line_ty = LLVMFunctionType(char_ptr_type!(self), null!(), 0, 0);
                let get_line_val = LLVMAddFunction(self.module, name!(b"IO_getLine"), get_line_ty);
                let get_line_bb = LLVMAppendBasicBlockInContext(self.ctx, get_line_val, name!(b"IO_getLine_main"));
                LLVMPositionBuilderAtEnd(self.builder, get_line_bb);
                LLVMBuildCall2(self.builder, char_ptr_type!(self), get_line_inner_val, null!(), 0, name!(b"invoke_get_line_inner"));

                self.add_function("IO_getLine", get_line_val).unwrap();
            }

            // getInt
            {
                // Interface of Runtime
                let get_int_inner_ty = LLVMFunctionType(int_type!(), null!(), 0, 0);
                let get_int_inner_val = LLVMAddFunction(self.module, IO_GET_INT_NAME, get_int_inner_ty);

                // Called by decaf code
                let get_int_ty = LLVMFunctionType(int_type!(), null!(), 0, 0);
                let get_int_val = LLVMAddFunction(self.module, name!(b"IO_getInt"), get_int_ty);
                let get_int_bb = LLVMAppendBasicBlockInContext(self.ctx, get_int_val, name!(b"IO_getInt_main"));
                LLVMPositionBuilderAtEnd(self.builder, get_int_bb);
                LLVMBuildCall2(self.builder, int_type!(), get_int_inner_val, null!(), 0, name!(b"invoke_get_int_inner"));

                self.add_function("IO_getInt", get_int_val).unwrap();
            }

            // peek
            {
                // Interface of Runtime
                let peek_inner_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let peek_inner_val = LLVMAddFunction(self.module, IO_PEEK_NAME, peek_inner_ty);

                // Called by decaf code
                let peek_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let peek_val = LLVMAddFunction(self.module, name!(b"IO_peek"), peek_ty);
                let peek_bb = LLVMAppendBasicBlockInContext(self.ctx, peek_val, name!(b"IO_peek_main"));
                LLVMPositionBuilderAtEnd(self.builder, peek_bb);
                LLVMBuildCall2(self.builder, char_type!(), peek_inner_val, null!(), 0, name!(b"invoke_peek_inner"));

                self.add_function("IO_peek", peek_val).unwrap();
            }
        }
        //
    }
}

trait CodeGen {
    fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult;
}

impl CodeGen for DecafTreeBuilder {
    fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult {
        // gencode for builtin class.
        construct.gen_builtin();

		for (_, cls) in &self.class_map {
			cls.gencode(construct).unwrap()
		}

		// Optimize

		Err(NotImplemented)
    }
}

impl CodeGen for decaf::Class {
	fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult {

		Err(NotImplemented)
	}
}

impl CodeGen for decaf::Method {
	fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult {
		Err(NotImplemented)
	}
}

impl CodeGen for decaf::Stmt {
	fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult {
		Err(NotImplemented)
	}
}

impl CodeGen for decaf::Expr {
	fn gencode(&self, construct: &mut CompilerConstruct) -> GenCodeResult {
		Err(NotImplemented)
	}
}

extern crate llvm_sys;
extern crate tempfile;

use std::ffi::{CStr, CString};
use std::collections::BTreeMap;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_manager_builder::*;
use crate::treebuild::{DecafTreeBuilder, Program};
use crate::decaf;
use crate::shell;
use CompileError::*;

use tempfile::NamedTempFile;
use std::str;

use std::ptr;
use std::ptr::null_mut;

use std::rc::Rc;

use std::mem;

use self::CodeValue::*;
use std::cell::Ref;
use crate::decaf::{Visibility, Returnable};
use self::llvm_sys::debuginfo::LLVMTemporaryMDNode;
use crate::treebuild::SemanticError::EUninitializedArray;
use crate::decaf::Scope::{BlockScope, WhileScope};
use self::llvm_sys::bit_reader::LLVMParseBitcode2;

pub const ADDRESS_SPACE_GENERIC: ::libc::c_uint = 0;
pub const ADDRESS_SPACE_GLOBAL: ::libc::c_uint = 1;
pub const ADDRESS_SPACE_SHARED: ::libc::c_uint = 3;
pub const ADDRESS_SPACE_CONST: ::libc::c_uint = 4;
pub const ADDRESS_SPACE_LOCAL: ::libc::c_uint = 5;
pub const IO_PUT_CHAR_NAME: *const ::libc::c_char = b"IO_putChar_inner\0".as_ptr() as *const _;
pub const IO_PUT_STRING_NAME: *const ::libc::c_char = b"IO_putString_inner\0".as_ptr() as *const _;
pub const IO_PUT_INT_NAME: *const ::libc::c_char = b"IO_putInt_inner\0".as_ptr() as *const _;
pub const IO_GET_CHAR_NAME: *const ::libc::c_char = b"IO_getChar_inner\0".as_ptr() as *const _;
pub const IO_GET_INT_NAME: *const ::libc::c_char = b"IO_getInt_inner\0".as_ptr() as *const _;
pub const IO_GET_LINE_NAME: *const ::libc::c_char = b"IO_getLine_inner\0".as_ptr() as *const _;
pub const IO_PEEK_NAME: *const ::libc::c_char = b"IO_peek_inner\0".as_ptr() as *const _;
pub const DECAF_MD_PUB_CTOR_KIND: ::libc::c_uint = 1;
pub const DECAF_MD_PROT_CTOR_KIND: ::libc::c_uint = 2;
pub const DECAF_MD_PRIV_CTOR_KIND: ::libc::c_uint = 3;
pub const DECAF_MD_PUB_METHOD_KIND: ::libc::c_uint = 4;
pub const DECAF_MD_PROT_METHOD_KIND: ::libc::c_uint = 5;
pub const DECAF_MD_PRIV_METHOD_KIND: ::libc::c_uint = 6;
pub const DECAF_MD_PUB_STATIC_KIND: ::libc::c_uint = 7;
pub const DECAF_MD_PROT_STATIC_KIND: ::libc::c_uint = 8;
pub const DECAF_MD_PRIV_STATIC_KIND: ::libc::c_uint = 9;
const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

macro_rules! char_type {
      () => {
          unsafe { LLVMInt8Type() }
      }
    }

macro_rules! int_type {
      () => {
          unsafe { LLVMInt64Type() }
      }
    }

macro_rules! i32_type {
      () => {
          LLVMInt32Type()
      }
}

macro_rules! bool_type {
      () => {
          unsafe {  LLVMInt1Type() }
      }
    }

macro_rules! void_type {
      () => {
          unsafe { LLVMVoidType() }
      }
    }

macro_rules! null {
      () => { ptr::null::<LLVMTypeRef>() as *mut _ }
}

macro_rules! i64_ptr_type {
      () => ({
          unsafe { LLVMPointerType(LLVMInt64Type(), ADDRESS_SPACE_GENERIC) }
      })
}

macro_rules! char_ptr_type {
      () => ({
          unsafe { LLVMPointerType(LLVMInt8Type(), ADDRESS_SPACE_GENERIC) }
      });
      ($cstrct:expr) => ({
          LLVMPointerType(
                    LLVMInt8TypeInContext($cstrct.ctx), ADDRESS_SPACE_GENERIC)
      })
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

macro_rules! vtable_name {
    ($cls_name:expr) => {
        format!("{}_vtable", $cls_name)
    }
}

pub enum CodeValue {
    NamedValue(String, decaf::Type, LLVMValueRef),
    UnNamedValue(decaf::Type, LLVMValueRef),
    Type(decaf::Type, LLVMTypeRef),
    OK,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    ENotImplemented,
    EDuplicatedType,
    EDuplicatedFunction,
    EDuplicatedGlobal,
    EFunctionNotFound(String),
    EValueNotFound(String),
    EIndexNotFound(String),
    EUnknownType(String),
    EUninitializedArray(String),
    EBlockNotInFunction(String),
    ELoopLacksCondBB(String),
    EInvalidLHSType(decaf::Type),
}

pub type GenCodeResult = Result<CodeValue, CompileError>;

// Our function naming convention is simple: <ClassName> + "_" + <MethodName>

pub fn get_default_target_triple() -> CString {
    let target_triple;
    unsafe {
        let target_triple_ptr = LLVMGetDefaultTargetTriple();
        target_triple = CStr::from_ptr(target_triple_ptr as *const _).to_owned();
        LLVMDisposeMessage(target_triple_ptr);
    }

    target_triple
}

pub fn llvm_module_to_cstring(module: LLVMModuleRef) -> CString {
    unsafe {
        // LLVM gives us a *char pointer, so wrap it in a CStr to mark it
        // as borrowed.
        let llvm_ir_ptr = LLVMPrintModuleToString(module);
        let llvm_ir = CStr::from_ptr(llvm_ir_ptr as *const _);

        // Make an owned copy of the string in our memory space.
        let module_string = CString::new(llvm_ir.to_bytes()).unwrap();

        // Cleanup borrowed string.
        LLVMDisposeMessage(llvm_ir_ptr);

        module_string
    }
}

fn create_module(module_name: &str, target_triple: Option<String>) -> LLVMModuleRef {
    let c_module_name = CString::new(module_name).unwrap();
    let module_name_char_ptr = c_module_name.to_bytes_with_nul().as_ptr() as *const _;

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

    llvm_module
}

struct TargetMachine {
	tm: LLVMTargetMachineRef,
}

impl TargetMachine {
	fn new(target_triple: *const i8) -> Result<Self, String> {
		let mut target = null_mut();
        let mut err_msg_ptr = null_mut();
		unsafe {
			LLVMGetTargetFromTriple(target_triple, &mut target, &mut err_msg_ptr);
			if target.is_null() {
				// LLVM couldn't find a target triple with this name,
                // so it should have given us an error message.
				assert!(!err_msg_ptr.is_null());

				let err_msg_cstr = CStr::from_ptr(err_msg_ptr as *const _);
				let err_msg = str::from_utf8(err_msg_cstr.to_bytes()).unwrap();
				return Err(err_msg.to_owned());
			}
		}

		let cpu = CString::new("generic").unwrap();
		let features = CString::new("").unwrap();

		let target_machine;
		unsafe {
			target_machine = LLVMCreateTargetMachine(
				target,
				target_triple,
				cpu.as_ptr() as *const _,
				features.as_ptr() as *const _,
				LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
				LLVMRelocMode::LLVMRelocPIC, LLVMCodeModel::LLVMCodeModelDefault
			);
		}

		Ok(TargetMachine { tm: target_machine })
	}
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.tm);
        }
    }
}

#[derive(PartialEq)]
enum GenState {
    First,
    Second,
}

pub struct CodeGenerator {
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,
    types_map: BTreeMap<String, LLVMTypeRef>,
    functions_map: BTreeMap<String, LLVMValueRef>,
    global_map: BTreeMap<String, LLVMValueRef>,
	target_triple: Option<String>,
	llvm_inited: bool,
	strings: Vec<CString>,
    state: GenState,
    scopes: Vec<decaf::Scope>,
}

pub fn new_generator(module_name: &str, target_triple: Option<String>) -> CodeGenerator {
	CodeGenerator::new(module_name, target_triple)
}


impl CodeGenerator {
	fn new(module_name: &str, target_triple: Option<String>) -> Self {
		let ctx = unsafe { LLVMContextCreate() };
		CodeGenerator {
			ctx: ctx,
			builder: unsafe { LLVMCreateBuilderInContext(ctx) },
			module: create_module(module_name, target_triple.clone()),
			types_map: BTreeMap::new(),
			functions_map: BTreeMap::new(),
            global_map: BTreeMap::new(),
			target_triple,
			llvm_inited: false,
			strings: Vec::new(),
            state: GenState::First,
            scopes: Vec::new(),
		}
	}

    // TODO: merge with ty2llvmty
    fn get_llvm_type_from_decaf_type(&self, ty: &decaf::Type, want_ptr: bool) -> Option<LLVMTypeRef> {
        use decaf::TypeBase::*;
        let base_llvm_ty = match &ty.base {
            UnknownTy(name) => {
                panic!("unknown type : {}", name);
            }
            BoolTy => bool_type!(),
            IntTy => int_type!(),
            CharTy => char_type!(),
            StrTy => panic!("StrTy shouldn't be looked up"),
            VoidTy => void_type!(),
            NULLTy => char_ptr_type!(),
            ClassTy(cls) => {
                if let Some(ty) = self.get_llvm_type_by_name(&cls.name, false) {
                    ty
                } else {
                    return None;
                }
            }
        };

        if ty.array_lvl == 0 {
            if want_ptr {
                Some(unsafe { LLVMPointerType(base_llvm_ty.clone(), ADDRESS_SPACE_GENERIC) })
            } else {
                Some(base_llvm_ty)
            }
        } else {
            let mut curr_ty = base_llvm_ty;
            let mut lvl = ty.array_lvl;
            while lvl > 0 {
                unsafe {
                    curr_ty = LLVMPointerType(curr_ty, ADDRESS_SPACE_GENERIC);
                }
                lvl -= 1;
            }
            if want_ptr {
                Some(unsafe { LLVMPointerType(curr_ty.clone(), ADDRESS_SPACE_GENERIC) })
            } else {
                Some(curr_ty);
            }
        }
    }

    fn get_llvm_type_by_name(&self, name: &str, want_ptr: bool) -> Option<LLVMTypeRef> {
        match self.types_map.get(name.into()) {
            None => None,
            Some(t) => {
                if want_ptr {
                    Some(unsafe { LLVMPointerType(t.clone(), ADDRESS_SPACE_GENERIC) })
                } else {
                    Some(t.clone())
                }

            }
        }
    }

    fn add_llvm_type(&mut self, name: &str, ty: LLVMTypeRef) -> Result<(), CompileError> {
        match self.get_llvm_type_by_name(name, false) {
            None => {
                self.types_map.insert(name.into(), ty);
                Ok(())
            }
            Some(_) => Err(EDuplicatedType)
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
            Some(_) => Err(EDuplicatedFunction)
        }
    }

    fn get_global_by_name(&self, name: &str) -> Option<LLVMValueRef> {
        match self.global_map.get(name.into()) {
            None => None,
            Some(g) => Some(g.clone())
        }
    }

    fn add_global(&mut self, name: &str, g: LLVMValueRef) -> Result<(), CompileError> {
        match self.get_global_by_name(name) {
            None => {
                self.global_map.insert(name.into(), g);
                Ok(())
            }
            Some(_) => Err(EDuplicatedGlobal)
        }
    }

	fn new_string_class(&mut self, s: &str) -> LLVMValueRef {
		// Create a String object and return the value of it
		unsafe {
            // Allocate memory
            let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String", false).unwrap(), name!(b"malloc_string\0"));
            // Copy string
            let s_val = LLVMConstStringInContext(self.ctx, self.new_string_ptr(s), s.len() as u32, 1);
            // TODO: allocate ?
            let str_addr = LLVMBuildGEP2(self.builder, char_ptr_type!(self), string_val, indices!(0, 0), 2, name!(b"gep_str_from_String\0"));
            LLVMBuildStore(self.builder, s_val, str_addr);
            string_val
		}
	}

	pub fn new_string_ptr(&mut self, s: &str) -> *const i8 {
		self.new_mut_string_ptr(s)
	}

	pub fn new_mut_string_ptr(&mut self, s: &str) -> *mut i8 {
		let cstring = CString::new(s).unwrap();
		let ptr = cstring.as_ptr() as *mut _;
		self.strings.push(cstring);
		ptr
	}

    fn new_md_from_vals(&mut self, vals: &mut Vec<LLVMValueRef>) -> Option<LLVMMetadataRef> {
        if vals.len() > 0 {
            unsafe {
                Some(LLVMMDNodeInContext(self.ctx,
                                    vals.as_mut_slice().as_mut_ptr() as *mut _,
                                    vals.len() as u32));
                Some(LLVMTemporaryMDNode(self.ctx,
                                         vals.as_mut_slice().as_mut_ptr() as *mut _,
                                         vals.len()))
            }
        } else {
            None
        }
    }

	pub fn optimize(&mut self, llvm_opt: i64) {
		unsafe {
			let builder = LLVMPassManagerBuilderCreate();
			LLVMPassManagerBuilderSetOptLevel(builder, llvm_opt as u32);

			let pass_manager = LLVMCreatePassManager();
			LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);

			LLVMPassManagerBuilderDispose(builder);

			LLVMRunPassManager(pass_manager, self.module);
			// LLVMRunPassManager(pass_manager, self.module);

			LLVMDisposePassManager(pass_manager);
		}
	}

	pub fn write_object_file(&mut self, path: &str) -> Result<(), String> {
		let object_file = match NamedTempFile::new() {
			Ok(v) => v,
			Err(e) => panic!("NamedTempFile::new: {}", e)
		};

		unsafe {
			let target_triple = LLVMGetTarget(self.module);
			let target_machine = TargetMachine::new(target_triple)?;

			let mut obj_error = self.new_mut_string_ptr("Writing object file failed.");
			let result = LLVMTargetMachineEmitToFile(
				target_machine.tm,
				self.module,
				self.new_string_ptr(path) as *mut i8,
				LLVMCodeGenFileType::LLVMObjectFile,
				&mut obj_error,
			);

			if result != 0 {
				panic!("obj_error: {:?}", CStr::from_ptr(obj_error as *const _));
			}
		}

		Ok(())
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
                let put_char_val = LLVMAddFunction(self.module, name!(b"IO_putChar\0"), put_char_ty);
                let put_char_bb = LLVMAppendBasicBlockInContext(self.ctx, put_char_val, name!(b"IO_putChar_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_char_bb);
                let char_param_val = LLVMGetFirstParam(put_char_val);
                LLVMBuildCall(self.builder, put_char_inner_val, params!(char_param_val), 1, name!(b"\0"));

                self.add_function("IO_putChar", put_char_val).unwrap();
            }

            // putString
            {
                // Interface of Runtime
                let put_string_inner_ty = LLVMFunctionType(void_type!(), params!(char_ptr_type!(self)), 1, 0);
                let put_string_inner_val = LLVMAddFunction(self.module, IO_PUT_STRING_NAME, put_string_inner_ty);

                // Wrapper method
                let put_string_ty = LLVMFunctionType(void_type!(), params!(self.get_llvm_type_by_name("String", true).unwrap()), 1, 0);
                let put_string_val = LLVMAddFunction(self.module, name!(b"IO_putString\0"), put_string_ty);
                let put_string_bb = LLVMAppendBasicBlockInContext(self.ctx, put_string_val, name!(b"IO_putString_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_string_bb);
                let string_param_val = LLVMGetFirstParam(put_string_val);
                // Extract the str field
                let str_addr = LLVMBuildGEP(self.builder, string_param_val, indices!(0, 0), 2, name!(b"gep_str_from_String\0"));
                // Load the pointer address
                let str_val = LLVMBuildLoad2(self.builder, char_ptr_type!(self), str_addr, name!(b"load_str_val\0"));
                // Call the runtime
                LLVMBuildCall(self.builder, put_string_inner_val, params!(str_val), 1, name!(b"\0"));
                self.add_function("IO_putString", put_string_val).unwrap();
            }

            // putInt
            {
                // Interface of Runtime
                let put_int_inner_ty = LLVMFunctionType(void_type!(), params!(int_type!()), 1, 0);
                let put_int_inner_val = LLVMAddFunction(self.module, IO_PUT_INT_NAME, put_int_inner_ty);

                // Wrapper method
                let put_int_ty = LLVMFunctionType(void_type!(), params!(int_type!()), 1, 0);
                let put_int_val = LLVMAddFunction(self.module, name!(b"IO_putInt\0"), put_int_ty);
                let put_int_bb = LLVMAppendBasicBlockInContext(self.ctx, put_int_val, name!(b"IO_putInt_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_int_bb);
                let int_param_val = LLVMGetFirstParam(put_int_val);
                LLVMBuildCall(self.builder,put_int_inner_val, params!(int_param_val), 1, name!(b"\0"));

                self.add_function("IO_putInt", put_int_val).unwrap();
            }

            // getChar
            {
                // Interface of Runtime
                let get_char_inner_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let get_char_inner_val = LLVMAddFunction(self.module, IO_GET_CHAR_NAME, get_char_inner_ty);

                // Called by decaf code
                let get_char_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let get_char_val = LLVMAddFunction(self.module, name!(b"IO_getChar\0"), get_char_ty);
                let get_char_bb = LLVMAppendBasicBlockInContext(self.ctx, get_char_val, name!(b"IO_getChar_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_char_bb);
                let char_val = LLVMBuildCall(self.builder, get_char_inner_val, null!(), 0, name!(b"inner_char\0"));
                LLVMBuildRet(self.builder, char_val);
                self.add_function("IO_getChar", get_char_val).unwrap();
            }

            // getLine
            {
                // Note: Runtime should make sure the char array is always alive because we are not going to make copy
                // Interface of Runtime
                let get_line_inner_ty = LLVMFunctionType(char_ptr_type!(self), null!(), 0, 0);
                let get_line_inner_val = LLVMAddFunction(self.module, IO_GET_LINE_NAME, get_line_inner_ty);

                // Called by decaf code
                let get_line_ty = LLVMFunctionType(self.get_llvm_type_by_name("String", true).unwrap(), null!(), 0, 0);
                let get_line_val = LLVMAddFunction(self.module, name!(b"IO_getLine\0"), get_line_ty);
                let get_line_bb = LLVMAppendBasicBlockInContext(self.ctx, get_line_val, name!(b"IO_getLine_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_line_bb);
                let inner_line = LLVMBuildCall(self.builder, get_line_inner_val, null!(), 0, name!(b"inner_line\0"));

                // Construct a String class
                // Allocate memory
                let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String", false).unwrap(), name!(b"malloc_string\0"));
                println!("type of string_val: ");
                LLVMDumpType(LLVMTypeOf(string_val));
                let str_addr = LLVMBuildGEP(self.builder, string_val, indices!(0, 0), 2, name!(b"String_field_str\0"));
                LLVMBuildStore(self.builder, inner_line, str_addr);
                LLVMBuildRet(self.builder, string_val);

                self.add_function("IO_getLine", get_line_val).unwrap();
            }

            // getInt
            {
                // Interface of Runtime
                let get_int_inner_ty = LLVMFunctionType(int_type!(), null!(), 0, 0);
                let get_int_inner_val = LLVMAddFunction(self.module, IO_GET_INT_NAME, get_int_inner_ty);

                // Called by decaf code
                let get_int_ty = LLVMFunctionType(int_type!(), null!(), 0, 0);
                let get_int_val = LLVMAddFunction(self.module, name!(b"IO_getInt\0"), get_int_ty);
                let get_int_bb = LLVMAppendBasicBlockInContext(self.ctx, get_int_val, name!(b"IO_getInt_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_int_bb);
                let inner_int = LLVMBuildCall(self.builder,get_int_inner_val, null!(), 0, name!(b"inner_int\0"));
                LLVMBuildRet(self.builder, inner_int);

                self.add_function("IO_getInt", get_int_val).unwrap();
            }

            // peek
            {
                // Interface of Runtime
                let peek_inner_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let peek_inner_val = LLVMAddFunction(self.module, IO_PEEK_NAME, peek_inner_ty);

                // Called by decaf code
                let peek_ty = LLVMFunctionType(char_type!(), null!(), 0, 0);
                let peek_val = LLVMAddFunction(self.module, name!(b"IO_peek\0"), peek_ty);
                let peek_bb = LLVMAppendBasicBlockInContext(self.ctx, peek_val, name!(b"IO_peek_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, peek_bb);
                let peek_char = LLVMBuildCall(self.builder, peek_inner_val, null!(), 0, name!(b"peek_char\0"));
                LLVMBuildRet(self.builder, peek_char);

                self.add_function("IO_peek", peek_val).unwrap();
            }
        }
    }

	fn init_llvm(&mut self) {
		if !self.llvm_inited {
			unsafe {
				// TODO: are all these necessary? Are there docs?
				LLVM_InitializeAllTargetInfos();
				LLVM_InitializeAllTargets();
				LLVM_InitializeAllTargetMCs();
				LLVM_InitializeAllAsmParsers();
				LLVM_InitializeAllAsmPrinters();
			}
			self.llvm_inited = true;
		}
	}

	fn add_main_fn(&mut self) {
		let mut main_args = vec![];
		unsafe {
			let main_ty = LLVMFunctionType(int_type!(), main_args.as_mut_ptr(), 0, LLVM_FALSE);
			LLVMAddFunction(self.module, self.new_string_ptr("main"), main_ty);
		}
	}

	pub fn link_object_file(&self,
							object_file_path: &str,
							executable_path: &str
	) -> Result<(), String> {
		// Link the object file
		let clang_args = if let Some(ref target_triple) = self.target_triple {
			vec![
				object_file_path,
				"-target",
				&target_triple,
				"-o",
				&executable_path[..],
			]
		} else {
			vec![object_file_path, "-o", &executable_path[..]]
		};

		shell::run_shell_command("clang", &clang_args[..])
	}

	pub fn compile_to_module(&mut self, program: &Program) -> Result<LLVMModuleRef, String> {
		self.init_llvm();
		self.gen_builtin();
		self.add_main_fn();

        for cls in &program.classes {
            match cls.gencode(self) {
                Ok(_) => {}
                Err(e) => {
                    return Err(format!("Compile error {:?}", e));
                }
            }
        }

		Ok(self.module.clone())
	}

}

fn ty2llvmty(ty: &decaf::Type) -> LLVMTypeRef {
    use decaf::TypeBase::*;
    let base_llvm_ty = match &ty.base {
        UnknownTy(name) => {
            panic!("unknown type : {}", name);
        }
        BoolTy => bool_type!(),
        IntTy => int_type!(),
        CharTy => char_type!(),
        StrTy => panic!("StrTy shouldn't be looked up"),
        VoidTy => void_type!(),
        NULLTy => char_ptr_type!(),
        ClassTy(_) => char_ptr_type!(),  // Hack here, we'll cast it to correct type during field accessing
    };
    if ty.array_lvl == 0 {
        base_llvm_ty
    } else {
        let mut curr_ty = base_llvm_ty;
        let mut lvl = ty.array_lvl;
        while lvl > 0 {
            unsafe {
                curr_ty = LLVMPointerType(curr_ty, ADDRESS_SPACE_GENERIC);
            }
            lvl -= 1;
        }
        curr_ty
    }
}

trait CodeGen {
    fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult;
}

impl CodeGen for decaf::Class {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {

        // Skip built-in classes
        if self.name == "Object" || self.name == "String" || self.name == "IO" {
            return Ok(OK);
        }

        match generator.state {
            GenState::First => {
                // Define vtable type
                let vtable_ty;
                let vtable_global;
                unsafe {
                    vtable_ty = LLVMArrayType(i64_ptr_type!(), self.vtable.borrow().len() as u32);
                    vtable_global = LLVMAddGlobal(generator.module, vtable_ty,
                                                  generator.new_string_ptr(&vtable_name!(self.name)));
                }

                generator.add_global(&vtable_name!(self.name), vtable_global);

                // Define LLVM struct type for class
                let mut field_llvm_types: Vec<LLVMTypeRef> = vec![vtable_ty];
                field_llvm_types.extend(self.pub_fields.borrow().iter().map(|f| ty2llvmty(&f.ty.borrow())));
                field_llvm_types.extend(self.prot_fields.borrow().iter().map(|f| ty2llvmty(&f.ty.borrow())));
                field_llvm_types.extend(self.priv_fields.borrow().iter().map(|f| ty2llvmty(&f.ty.borrow())));
                let cls_llvm_type;
                unsafe {
                    cls_llvm_type = LLVMStructType(field_llvm_types.as_mut_ptr() as *mut _, field_llvm_types.len() as u32, 0);
                }
                generator.add_llvm_type(&self.name, cls_llvm_type.clone())?;

                // Define types for all methods/ctors/static methods

                unsafe {
                    for (ix, pub_ctor) in self.pub_ctors.borrow().iter().enumerate() {
                        let ctor_ty;
                        let ctor_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(pub_ctor.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                       args.as_mut_ptr() as *mut _,
                                                       pub_ctor.args.borrow().len() as u32,
                                                       0);
                            ctor_val = LLVMAddFunction(generator.module,
                                                       generator.new_string_ptr(&pub_ctor.llvm_name()),
                                                       ctor_ty);
                            generator.add_function(&pub_ctor.llvm_name(), ctor_val);
                        }
                    }

                    for (ix, prot_ctor) in self.prot_ctors.borrow().iter().enumerate() {
                        let ctor_ty;
                        let ctor_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(prot_ctor.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                       args.as_mut_ptr() as *mut _,
                                                       prot_ctor.args.borrow().len() as u32,
                                                       0);
                            ctor_val = LLVMAddFunction(generator.module,
                                                       generator.new_string_ptr(&prot_ctor.llvm_name()),
                                                       ctor_ty);
                            generator.add_function(&prot_ctor.llvm_name(), ctor_val);
                        }
                    }

                    for (ix, priv_ctor) in self.priv_ctors.borrow().iter().enumerate() {
                        let ctor_ty;
                        let ctor_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(priv_ctor.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                       args.as_mut_ptr() as *mut _,
                                                       priv_ctor.args.borrow().len() as u32,
                                                       0);
                            ctor_val = LLVMAddFunction(generator.module,
                                                       generator.new_string_ptr(&priv_ctor.llvm_name()),
                                                       ctor_ty);
                            generator.add_function(&priv_ctor.llvm_name(), ctor_val);
                        }
                    }

                    for (ix, pub_method) in self.pub_methods.borrow().iter().enumerate() {
                        let method_ty;
                        let method_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(pub_method.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            method_ty = LLVMFunctionType(ty2llvmty(&pub_method.return_ty.borrow()),
                                                         args.as_mut_ptr() as *mut _,
                                                         pub_method.args.borrow().len() as u32,
                                                         0);
                            method_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&pub_method.llvm_name()),
                                                         method_ty);
                            generator.add_function(&pub_method.llvm_name(), method_val);
                        }
                    }

                    for (ix, prot_method) in self.prot_methods.borrow().iter().enumerate() {
                        let method_ty;
                        let method_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(prot_method.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            method_ty = LLVMFunctionType(ty2llvmty(&prot_method.return_ty.borrow()),
                                                         args.as_mut_ptr() as *mut _,
                                                         prot_method.args.borrow().len() as u32,
                                                         0);
                            method_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&prot_method.llvm_name()),
                                                         method_ty);
                            generator.add_function(&prot_method.llvm_name(), method_val);
                        }
                    }

                    for (ix, priv_method) in self.priv_methods.borrow().iter().enumerate() {
                        let method_ty;
                        let method_val;
                        unsafe {
                            let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                            args.extend(priv_method.args.borrow().iter()
                                .map(|(_, ty)| ty2llvmty(ty))
                                .collect::<Vec<LLVMTypeRef>>());
                            method_ty = LLVMFunctionType(ty2llvmty(&priv_method.return_ty.borrow()),
                                                         args.as_mut_ptr() as *mut _,
                                                         priv_method.args.borrow().len() as u32,
                                                         0);
                            method_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&priv_method.llvm_name()),
                                                         method_ty);
                            generator.add_function(&priv_method.llvm_name(), method_val);
                        }
                    }

                    for (ix, pub_static) in self.pub_static_methods.borrow().iter().enumerate() {
                        let static_ty;
                        let static_val;
                        unsafe {
                            static_ty = LLVMFunctionType(ty2llvmty(&pub_static.return_ty.borrow()),
                                                         pub_static.args.borrow().iter()
                                                             .map(|(_, ty)| ty2llvmty(ty))
                                                             .collect::<Vec<LLVMTypeRef>>().as_mut_ptr() as *mut _,
                                                         pub_static.args.borrow().len() as u32,
                                                         0);
                            static_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&pub_static.llvm_name()),
                                                         static_ty);
                            generator.add_function(&pub_static.llvm_name(), static_val);
                        }
                    }

                    for (ix, prot_static) in self.prot_static_methods.borrow().iter().enumerate() {
                        let static_ty;
                        let static_val;
                        unsafe {
                            static_ty = LLVMFunctionType(ty2llvmty(&prot_static.return_ty.borrow()),
                                                         prot_static.args.borrow().iter()
                                                             .map(|(_, ty)| ty2llvmty(ty))
                                                             .collect::<Vec<LLVMTypeRef>>().as_mut_ptr() as *mut _,
                                                         prot_static.args.borrow().len() as u32,
                                                         0);
                            static_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&prot_static.llvm_name()),
                                                         static_ty);
                            generator.add_function(&prot_static.llvm_name(), static_val);
                        }
                    }

                    for (ix, priv_static) in self.priv_static_methods.borrow().iter().enumerate() {
                        let static_ty;
                        let static_val;
                        unsafe {
                            static_ty = LLVMFunctionType(ty2llvmty(&priv_static.return_ty.borrow()),
                                                         priv_static.args.borrow().iter()
                                                             .map(|(_, ty)| ty2llvmty(ty))
                                                             .collect::<Vec<LLVMTypeRef>>().as_mut_ptr() as *mut _,
                                                         priv_static.args.borrow().len() as u32,
                                                         0);
                            static_val = LLVMAddFunction(generator.module,
                                                         generator.new_string_ptr(&priv_static.llvm_name()),
                                                         static_ty);
                            generator.add_function(&priv_static.llvm_name(), static_val);
                        }
                    }
                }
            }
            GenState::Second => {
                // Visit each ctor/method/static method
                for (ix, ctor) in self.pub_ctors.borrow().iter().enumerate() {
                    ctor.gencode(generator)?;
                }

                for (ix, ctor) in self.prot_ctors.borrow().iter().enumerate() {
                    ctor.gencode(generator)?;
                }

                for (ix, ctor) in self.priv_ctors.borrow().iter().enumerate() {
                    ctor.gencode(generator)?;
                }

                for (ix, method) in self.pub_methods.borrow().iter().enumerate() {
                    method.gencode(generator)?;
                }

                for (ix, method) in self.prot_methods.borrow().iter().enumerate() {
                    method.gencode(generator)?;
                }

                for (ix, method) in self.priv_methods.borrow().iter().enumerate() {
                    method.gencode(generator)?;
                }

                for (ix, static_method) in self.pub_static_methods.borrow().iter().enumerate() {
                    static_method.gencode(generator)?;
                }

                for (ix, static_method) in self.prot_static_methods.borrow().iter().enumerate() {
                    static_method.gencode(generator)?;
                }

                for (ix, static_method) in self.priv_static_methods.borrow().iter().enumerate() {
                    static_method.gencode(generator)?;
                }
            }
        }

        Ok(OK)
	}
}

impl CodeGen for decaf::Ctor {
    fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        use decaf::Scope::*;
        assert!(generator.state, GenState::Second);
        // Get function llvm value
		let name = self.llvm_name();
		match generator.get_function_by_name(&name) {
			Some(ctor_llvm_val) => {
				// Create basic block
				unsafe {
					let ctor_bb = LLVMAppendBasicBlockInContext(
						generator.ctx,
						ctor_llvm_val,
						generator.new_string_ptr(
							&format!("{}_mainbb",
									 self.llvm_name()))
					);
					LLVMPositionBuilderAtEnd(generator.builder, ctor_bb);
				}

                self.scopes.push(MethodScope(method.clone()));

				// Visit body
				let ret = decaf::Stmt::Block(self.body.borrow().clone().unwrap()).gencode(generator);

                self.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGen for decaf::Method {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        use decaf::Scope::*;
        assert!(generator.state, GenState::Second);
		let name = self.llvm_name();
		match generator.get_function_by_name(&name) {
			Some(method_llvm_val) => {
				// Create basic block
				unsafe {
					let method_bb = LLVMAppendBasicBlockInContext(
						generator.ctx,
						method_llvm_val,
						generator.new_string_ptr(
							&format!("{}_mainbb",
									 self.llvm_name()))
					);
					LLVMPositionBuilderAtEnd(generator.builder, method_bb);
				}

                self.scopes.push(MethodScope(method.clone()));

				// Visit body
                let ret = decaf::Stmt::Block(self.body.borrow().clone().unwrap()).gencode(generator);

                self.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGen for decaf::Stmt {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state, GenState::Second);
        use CompileError::*;
        use decaf::Stmt::*;
        use CodeValue::*;
        use decaf::TypeBase::*;
		match self {
            Declare(stmt) => {
                match &stmt.init_expr {
                    Some(expr) => {
                        match expr.gencode(generator)? {
                            NamedValue(_, vty, val) | UnNamedValue(vty, val) => {
                                if stmt.ty.is_compatible_with(&vty) {
                                    let lhs_addr = match stmt.ty.array_lvl {
                                        0 => {
                                            match &stmt.ty.base {
                                                IntTy | BoolTy | CharTy  => {
                                                    // Allocate the entire thing
                                                    if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, false) {
                                                        // Allocate lhs
                                                        unsafe {
                                                            LLVMBuildAlloca(generator.builder, llvm_ty,
                                                                            generator.new_string_ptr(&stmt.name))
                                                        }
                                                    } else {
                                                        return Err(EUnknownType(format!("{:?}", stmt.ty)));
                                                    }
                                                }
                                                ClassTy(cls) => {
                                                    // Allocate ptr
                                                    if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, true) {
                                                        unsafe {
                                                            LLVMBuildAlloca(generator.builder, llvm_ty,
                                                                            generator.new_string_ptr(&stmt.name))
                                                        }
                                                    } else {
                                                        return Err(EUnknownType(format!("{:?}", stmt.ty)));
                                                    }
                                                }
                                                VoidTy | NULLTy | UnknownTy(_) | StrTy => {
                                                    return Err(EInvalidLHSType(stmt.ty.clone()));
                                                }
                                            }
                                        }
                                        _ => {
                                            // Array type
                                            match &stmt.ty.base {
                                                IntTy | BoolTy | CharTy | ClassTy(_) => {
                                                    // Allocate ptr
                                                    if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, true) {
                                                        unsafe {
                                                            LLVMBuildAlloca(generator.builder, llvm_ty,
                                                                            generator.new_string_ptr(&stmt.name))
                                                        }
                                                    } else {
                                                        return Err(EUnknownType(format!("{:?}", stmt.ty)));
                                                    }
                                                }
                                                VoidTy | NULLTy | UnknownTy(_) | StrTy => {
                                                    return Err(EInvalidLHSType(stmt.ty.clone()));
                                                }
                                            }
                                        }
                                    };
                                    unsafe {
                                        LLVMBuildStore(generator.builder, val, lhs_addr);
                                    }
                                    self.set_variable_addr(&stmt.name, &stmt.ty, lhs_addr);
                                    Ok(OK);
                                }
                            }
                            _ => Err(EValueNotFound(format!("{:?}", expr)))
                        }
                    }
                    None => {
                        match stmt.ty.array_lvl {
                            0 => {
                                let lhs_addr = match &stmt.ty.base {
                                    IntTy | BoolTy | CharTy  => {
                                        // Allocate the entire thing
                                        // TODO: zero value
                                        if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, false) {
                                            // Allocate lhs
                                            unsafe {
                                                LLVMBuildAlloca(generator.builder, llvm_ty,
                                                                generator.new_string_ptr(&stmt.name))
                                            }
                                        } else {
                                            return Err(EUnknownType(format!("{:?}", stmt.ty)));
                                        }
                                    }
                                    ClassTy(cls) => {
                                        // Allocate ptr
                                        // TODO: zero value
                                        if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, true) {
                                            unsafe {
                                                LLVMBuildAlloca(generator.builder, llvm_ty,
                                                                generator.new_string_ptr(&stmt.name))
                                            }
                                        } else {
                                            return Err(EUnknownType(format!("{:?}", stmt.ty)));
                                        }
                                    }
                                    VoidTy | NULLTy | UnknownTy(_) | StrTy => {
                                        return Err(EInvalidLHSType(stmt.ty.clone()));
                                    }
                                };
                                self.set_variable_addr(&stmt.name, &stmt.ty, lhs_addr);
                                Ok(OK);
                            }
                            _ => {
                                 Err(EUninitializedArray(format!("{:?}", stmt)));
                            }
                        }
                    }
                }
            }
            If(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        // Evaluate condition
                        match stmt.cond.gencode(generator)? {
                            NamedValue(_, vty, cond_val) | UnNamedValue(vty, cond_val) => {
                                unsafe {
                                    let ifbb = LLVMAppendBasicBlockInContext(
                                        generator.ctx,
                                        func_val,
                                        generator.new_string_ptr(
                                            &format!("if_bb_{}", generator.next_if_index())));
                                    let nextbb = LLVMAppendBasicBlockInContext(
                                        generator.ctx,
                                        func_val,
                                        generator.new_string_ptr(
                                            &format!("next_bb_{}", generator.next_next_index())
                                        )
                                    );
                                    LLVMBuildCondBr(generator.builder, cond_val, ifbb, nextbb);

                                    stmt.thenblock.gencode(generator)?;

                                    if !Block(stmt.thenblock.clone()).returnable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);

                                    Ok(OK)
                                }
                            }
                            _ => Err(EValueNotFound(format!("{:?}", stmt.cond)))
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            IfElse(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        match stmt.cond.gencode(generator)? {
                            NamedValue(_, vty, cond_val) | UnNamedValue(vty, cond_val) => {
                                unsafe {
                                    let ifbb = LLVMAppendBasicBlockInContext(
                                        generator.ctx,
                                        func_val,
                                        generator.new_string_ptr(
                                            &format!("if_bb_{}", generator.next_if_index())));
                                    let elsebb = LLVMAppendBasicBlockInContext(
                                        generator.ctx,
                                        func_val,
                                        generator.new_string_ptr(
                                            &format!("else_bb_{}", generator.next_else_index())
                                        )
                                    );
                                    let nextbb = LLVMAppendBasicBlockInContext(
                                        generator.ctx,
                                        func_val,
                                        generator.new_string_ptr(
                                            &format!("next_bb_{}", generator.next_next_index())
                                        )
                                    );
                                    LLVMBuildCondBr(generator.builder, cond_val, ifbb, nextbb);

                                    LLVMPositionBuilderAtEnd(generator.builder, ifbb);

                                    generator.scopes.push(BlockScope(stmt.thenblock.clone()));

                                    stmt.thenblock.gencode(generator)?;

                                    generator.scopes.pop();

                                    if !Block(stmt.thenblock.clone()).returnable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, elsebb);

                                    generator.scopes.push(BlockScope(stmt.elseblock.clone()));

                                    stmt.elseblock.gencode(generator)?;

                                    generator.scopes.pop();

                                    if !Block(stmt.elseblock.clone()).returnable() {
                                        LLVMBuildBr(generator.builder, elsebb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);
                                }
                            }
                            _ => Err(EValueNotFound(format!("{:?}", stmt.cond)))
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            Expr(stmt) => {
                stmt.expr.gencode(generator)
            }
            While(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        unsafe {
                            let condbb = LLVMAppendBasicBlockInContext(
                                generator.ctx,
                                func_val,
                                generator.new_string_ptr(
                                    &format!("whilecond_bb_{}", generator.next_whilecond_index())
                                ));
                            let bodybb = LLVMAppendBasicBlockInContext(
                                generator.ctx,
                                func_val,
                                generator.new_string_ptr(
                                    &format!("whilebody_bb_{}", generator.next_whilebody_index())
                                )
                            );
                            let nextbb = LLVMAppendBasicBlockInContext(
                                generator.ctx,
                                func_val,
                                generator.new_string_ptr(
                                    &format!("whilenext_bb_{}", generator.next_whilenext_index())
                                )
                            );

                            *stmt.condbb.borrow_mut() = condbb.clone();
                            *stmt.bodybb.borrow_mut() = bodybb.clone();
                            *stmt.nextbb.borrow_mut() = nextbb.clone();

                            LLVMBuildBr(generator.builder, condbb);

                            LLVMPositionBuilderAtEnd(generator.builder, condbb);

                            match stmt.condexpr.gencode(generator)? {
                                NamedValue(_, _, cond_val) | UnNamedValue(_, cond_val) => {
                                    LLVMBuildCondBr(generator.builder, cond_val, bodybb, nextbb);

                                    LLVMPositionBuilderAtEnd(generator.builder, bodybb);

                                    generator.scopes.push(WhileScope(stmt.clone()));

                                    generator.scopes.push(BlockScope(stmt.bodyblock.borrow().clone().unwrap()));

                                    stmt.bodyblock.borrow().as_ref().unwrap().gencode(generator);

                                    generator.scopes.pop();

                                    generator.scopes.pop();

                                    // NOTE: we might add Br no matter what
                                    if !Block(stmt.bodyblock.borrow().clone().unwrap()).returnable() {
                                        LLVMBuildBr(generator.builder, condbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);
                                }
                                _ => Err(EValueNotFound(format!("{:?}", stmt.condexpr)))
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            Return(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        unsafe {
                            if let Some(ret_expr) = &stmt.expr {
                                match ret_expr.gencode(generator)? {
                                    NamedValue(_, _, ret_val) | UnNamedValue(_, ret_val) => {
                                        LLVMBuildRet(generator.builder, ret_val);
                                    }
                                    _ => Err(EValueNotFound(format!("{:?}", ret_expr)))
                                }
                            } else {
                                LLVMBuildRetVoid(generator.builder);
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            Continue(stmt) => {
                match stmt.lup.condbb.borrow().clone() {
                    Some(condbb) => {
                        unsafe {
                            LLVMBuildBr(generator.builder, condbb);
                        }
                    }
                    None => {
                        Err(ELoopLacksCondBB(format!("{:?}", stmt.lup)))
                    }
                }
            }
            Break(stmt) => {
                match stmt.lup.nextbb.borrow().clone() {
                    Some(nextbb) => {
                        unsafe {
                            LLVMBuildBr(generator.builder, nextbb);
                        }
                    }
                    None => {
                        Err(ELoopLacksCondBB(format!("{:?}", stmt.lup)))
                    }
                }
            }
            Super(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        // This must be in a ctor, verified by the semantic checking process
                        unsafe {
                            // Get this reference
                            let this_ref = LLVMGetFirstParam(func_val);

                            // Evaluate arguments
                            let mut arg_vals = vec![];
                            for arg in stmt.args.iter() {
                                match arg.gencode(generator)? {
                                    NamedValue(_, _, val) | UnNamedValue(_, val) => {
                                        
                                    }
                                    _ => {
                                        return Err(EValueNotFound(format!("{:?}", arg)));
                                    }
                                }
                            }

                            match generator.get_function_by_name(&stmt.sup_ctor.llvm_name()) {
                                Some(ctor_val) => {

                                }
                                None => Err(EFunctionNotFound(stmt.sup_ctor.llvm_name()))
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            Block(stmt) => {

            }
            NOP => {}
		}
		Err(ENotImplemented)
	}
}

impl CodeGen for decaf::Expr {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
		Err(ENotImplemented)
	}
}

trait LLVMName {
	fn llvm_name(&self) -> String;
}

impl LLVMName for decaf::Ctor {
	fn llvm_name(&self) -> String {
		format!("{}_{}_ctor_{}",
				self.cls.name,
				self.vis.borrow(),
				self.args.borrow().iter()
				.map(|(_, ty)|{format!("{}", ty)})
				.collect::<Vec<String>>()
				.join("_"))
	}
}

impl LLVMName for decaf::Method {
	fn llvm_name(&self) -> String {
		match *self.stat.borrow() {
			true => {
				format!("{}_{}_static_method_{}",
						self.cls.borrow().name,
						self.vis.borrow(),
						self.args.borrow().iter()
						.map(|(_, ty)|{format!("{}", ty)})
						.collect::<Vec<String>>()
						.join("_"))
			}
			false => {
				format!("{}_{}_method_{}",
						self.cls.borrow().name,
						self.vis.borrow(),
						self.args.borrow().iter()
						.map(|(_, ty)|{format!("{}", ty)})
						.collect::<Vec<String>>()
						.join("_"))
			}
		}
	}
}

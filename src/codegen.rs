extern crate llvm_sys;
extern crate tempfile;

use std::borrow::BorrowMut;
use std::collections::BTreeMap;
use std::ffi::{CStr, CString};
use std::ptr;
use std::ptr::null_mut;
use std::rc::Rc;
use std::str;

use llvm_sys::{core::*, LLVMIntPredicate::*, prelude::*, target::*,
               target_machine::*, transforms::pass_manager_builder::*};

use CodeValue::*;
use CompileError::*;
use DecafValue::*;

use crate::decaf;
use crate::decaf::{BlockStmt, Class, Expr::*, LiteralExpr::*, Returnable, Scope::*, Stmt::*, TypeBase::*, Variable, VariableTable, VTable};
use crate::shell;
use crate::treebuild::Program;
use crate::codegen::GenState::Second;
use self::llvm_sys::analysis::LLVMVerifyModule;
use self::llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use self::llvm_sys::LLVMLinkage::LLVMExternalLinkage;

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
//const LLVM_TRUE: LLVMBool = 1;

macro_rules! decaf_bool_type {
	() => {
		decaf::Type {
			base: BoolTy,
			array_lvl: 0
		}
	}
}

macro_rules! null {
      () => { ptr::null::<LLVMTypeRef>() as *mut _ }
}

macro_rules! name {
      ($id:expr) => { $id.as_ptr() as *const _ }
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

pub enum DecafValue {
    Addr(Rc<decaf::Variable>),
    Val(decaf::Type, LLVMValueRef),
}

impl DecafValue {
    pub fn get_type(&self) -> decaf::Type {
        match self {
            Addr(val) => {
                val.ty.clone()
            }
            Val(ty, _) => ty.clone()
        }
    }
    pub fn get_value(&self, generator: &mut CodeGenerator) -> LLVMValueRef {
        match self {
            Addr(val) => {
                unsafe {
                    LLVMBuildLoad(generator.builder,
                                  val.addr.borrow().clone().unwrap(),
                                  generator.new_string_ptr(&format!("{}_ref_{}", val.name, val.next_refcount())))
                }
            }
            Val(_, val) => val.clone()
        }
    }
}

pub enum CodeValue {
    Value(DecafValue),
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
    EArrayZeroDim(String),
    ELoopLacksCondBB(String),
    ENonAddressableValueOnLHS(String),
    ENonArithOp(String),
    ENonLogicalOp(String),
    EUnaryArithNotFound(String),
    ENonCmpOp(String),
    EInvalidStringCreation(String),
    EArrayNotSupportMethodCall(String),
    ENonClassNotSupportMethodCall(String),
    EMethodNotFoundInVTable(String),
    EThisNotFound,
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

#[derive(Debug)]
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
	names: BTreeMap<String, u32>,
	classes: BTreeMap<String, Rc<Class>>,
}

pub fn new_generator(module_name: &str, target_triple: Option<String>) -> CodeGenerator {
	CodeGenerator::new(module_name, target_triple)
}

impl CodeGenerator {
	fn new(module_name: &str, target_triple: Option<String>) -> Self {
		let module = create_module(module_name, target_triple.clone());
		CodeGenerator {
			builder: unsafe { LLVMCreateBuilderInContext(LLVMGetModuleContext(module)) },
			module,
			types_map: BTreeMap::new(),
			functions_map: BTreeMap::new(),
            global_map: BTreeMap::new(),
			target_triple,
			llvm_inited: false,
			strings: Vec::new(),
            state: GenState::First,
            scopes: Vec::new(),
			names: BTreeMap::new(),
			classes: BTreeMap::new(),
		}
	}

    fn char_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMInt8TypeInContext(LLVMGetModuleContext(self.module))
        }
    }

    fn int_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMInt64TypeInContext(LLVMGetModuleContext(self.module))
        }
    }

    fn bool_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMInt1TypeInContext(LLVMGetModuleContext(self.module))
        }
    }

    fn i32_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMInt32TypeInContext(LLVMGetModuleContext(self.module))
        }
    }

    fn void_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMVoidTypeInContext(LLVMGetModuleContext(self.module))
        }
    }

    fn i64_ptr_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMPointerType(self.int_type(), ADDRESS_SPACE_GENERIC)
        }
    }

    fn char_ptr_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMPointerType(self.char_type(), ADDRESS_SPACE_GENERIC)
        }
    }

    fn const_i64(&self, val: i64) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(self.int_type(), val as u64, 0)
        }
    }

    fn const_bool(&self, val: bool) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(self.bool_type(), val as u64, 0)
        }
    }

    fn const_char(&self, val: char) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(self.char_type(), val as u64, 0)
        }
    }

    fn indices(&self, args: Vec<i64>) -> Vec<LLVMValueRef> {
        unsafe {
            args.iter().map(|v| LLVMConstInt(self.i32_type(), (*v) as u64, 0))
                .collect::<Vec<LLVMValueRef>>()
        }
    }

	fn tys2llvmtypes(&self, tys: Vec<Rc<decaf::Type>>) -> Vec<LLVMTypeRef> {
		let mut res = vec![];
		for ty in tys.iter() {
			match self.get_llvm_type_from_decaf_type(&ty.into(), false, true) {
				Some(llvmty) => {
					res.push(llvmty);
				}
				None => {
					panic!("fail to resolved type {}", ty);
				}
			}
		}
		res
	}

    fn get_llvm_type_from_decaf_type(&self, ty: &decaf::Type, want_ptr: bool, resolve: bool) -> Option<LLVMTypeRef> {
        use decaf::TypeBase::*;
        let base_llvm_ty = match &ty.base {
            UnknownTy(name) => {
                panic!("unknown type : {}", name);
            }
            BoolTy => self.bool_type(),
            IntTy => self.int_type(),
            CharTy => self.char_type(),
            StrTy => panic!("StrTy shouldn't be looked up"),
            VoidTy => self.void_type(),
            NULLTy => self.char_ptr_type(),
            ClassTy(cls) => {
                if resolve {
                    if let Some(ty) = self.get_llvm_type_by_name(&cls.name, false) {
                        ty
                    } else {
                        return None;
                    }
                } else {
                    self.char_ptr_type()
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
            let mut lvl = ty.array_lvl;
            let mut curr_ty = base_llvm_ty;
            unsafe {
                loop {
                    let mut field_types = [curr_ty, self.int_type()];
                    curr_ty = LLVMStructType((&mut field_types[0]) as *mut _, 2, 0);
                    lvl -= 1;
                    if lvl == 0 {
                        break
                    }
                }
            }
            if want_ptr {
                Some(unsafe { LLVMPointerType(curr_ty.clone(), ADDRESS_SPACE_GENERIC) })
            } else {
                Some(curr_ty)
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

	fn class_lookup(&self, cls_name: &str) -> Option<Rc<decaf::Class>> {
		match self.classes.get(cls_name.into()) {
			None => None,
			Some(cls) => Some(cls.clone())
		}
	}

	fn get_top_most_function_val(&self) -> Option<LLVMValueRef> {
		use decaf::Scope::*;
		for scope in self.scopes.iter().rev() {
			match scope {
				CtorScope(func_scope) => {
					return self.get_function_by_name(&func_scope.llvm_name());
				}
				MethodScope(func_scope) => {
					return self.get_function_by_name(&func_scope.llvm_name());
				}
				_ => continue,
			}
		}
		None
	}

    fn get_top_most_block(&self) -> Option<Rc<BlockStmt>> {
        for scope in self.scopes.iter().rev() {
            match scope {
                BlockScope(blk) => {
                    return Some(blk.clone())
                }
                _ => continue,
            }
        }
        None
    }

	fn cast_value(&mut self, src_ty: &decaf::Type, dst_ty: &decaf::Type, val: LLVMValueRef) -> LLVMValueRef {
        println!("casting value");
		// Only cast pointer type
		if src_ty.is_compatible_with(dst_ty) {
			let is_ptr = {
				if src_ty.array_lvl > 0 {
					true
				} else if let ClassTy(_) = src_ty.base {
					true
				} else {
					false
				}
			};
			if is_ptr {
				if src_ty.array_lvl == dst_ty.array_lvl {
					if src_ty.base != dst_ty.base {
                        println!("trying to get the llvm_type of {:?}", dst_ty);
                        let dst_llvm_ty = self.get_llvm_type_from_decaf_type(dst_ty, false, true).unwrap();
                        println!("trying to cast src_ty: {:?} to dst_ty: {:?}", src_ty, dst_ty);
                        println!("dst_llvm_ty: {:?}", dst_llvm_ty);
                        unsafe {
                            LLVMBuildPointerCast(
                                self.builder,
                                val,
                                dst_llvm_ty,
                                self.next_name(&format!("cast_{}_{}", src_ty, dst_ty)))
                        }
					} else {
						val
					}
				} else {
					panic!("casting will change array_lvl: src_type: {:?}, dst_type: {:?}", src_ty, dst_ty);
				}
			} else {
				val
			}
		} else {
			panic!("casting incompatible types. src_type: {:?}, dst_type: {:?}", src_ty, dst_ty);
		}
	}

	fn variable_lookup(&self, name: &str) -> Option<Rc<Variable>> {
		for scope in self.scopes.iter().rev() {
			match scope.variable_lookup(name){
				Some(var) => {
					return Some(var);
				}
				_ => continue,
			}
		}
		None
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
        println!("I am in new_string_class");
		// Create a String object and return the value of it
		unsafe {
            // Allocate memory
            let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String", false).unwrap(), self.next_name("malloc_string"));
            // Copy string
            let chars = CString::new(s).unwrap();
            let chars = chars.as_bytes_with_nul();
            let str_addr =  LLVMBuildGEP(self.builder, string_val, self.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _, 2, self.next_name("gep_string_inner"));
            let s_ptr = LLVMBuildGlobalStringPtr(self.builder, chars.as_ptr() as *const _, self.next_name("const_string_2"));
            LLVMBuildStore(self.builder, s_ptr, str_addr);
            println!("new_string_class return");
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

	pub fn next_name(&mut self, name: &str) -> *const i8 {
        *self.names.entry(name.into()).or_insert(0).borrow_mut() += 1;
        self.new_string_ptr(&format!("{}_{}", name, self.names.get(name.into()).unwrap()))
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
        println!("write_object_file called!");
        println!("writing to {}", path);
		unsafe {
			let target_triple = LLVMGetTarget(self.module);
			let target_machine = TargetMachine::new(target_triple)?;

			let mut obj_error = self.new_mut_string_ptr("Writing object file failed.");
            println!("Emitting file");
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
        unsafe {
            let object_type = LLVMStructType(null_mut(), 0, 0);
            self.add_llvm_type("Object", object_type).unwrap();
        }

        // String
        // a field called str
        // class String {
        //     private char * str
        // }
        println!("gen String");
        unsafe {
            let str_type = self.char_ptr_type();
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

            let io_type = LLVMStructType(null_mut(), 0, 0);
            self.add_llvm_type("IO", io_type).unwrap();

            println!("gen putChar");
            // putChar
            {
                // Interface of Runtime
                let put_char_inner_ty = LLVMFunctionType(self.void_type(), params!(self.char_type()), 1, 0);
                let put_char_inner_val = LLVMAddFunction(self.module, IO_PUT_CHAR_NAME, put_char_inner_ty);

                // Called by decaf code
                let put_char_ty = LLVMFunctionType(self.void_type(), params!(self.char_type()), 1, 0);
                let put_char_val = LLVMAddFunction(self.module, name!(b"IO_putChar\0"), put_char_ty);
                let put_char_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), put_char_val, name!(b"IO_putChar_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_char_bb);
                let char_param_val = LLVMGetFirstParam(put_char_val);
                LLVMBuildCall(self.builder, put_char_inner_val, params!(char_param_val), 1, name!(b"\0"));
                LLVMBuildRetVoid(self.builder);

                self.add_function("IO_pub_static_method_putChar_char_0_void", put_char_val).unwrap();
            }

            println!("gen putString");
            // putString
            {
                // Interface of Runtime
                let put_string_inner_ty = LLVMFunctionType(self.void_type(), params!(self.char_ptr_type()), 1, 0);
                let put_string_inner_val = LLVMAddFunction(self.module, IO_PUT_STRING_NAME, put_string_inner_ty);

                // Wrapper method
                let put_string_ty = LLVMFunctionType(self.void_type(), params!(self.get_llvm_type_by_name("String", true).unwrap()), 1, 0);
                let put_string_val = LLVMAddFunction(self.module, name!(b"IO_putString\0"), put_string_ty);
                let put_string_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), put_string_val, name!(b"IO_putString_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_string_bb);
                let string_param_val = LLVMGetFirstParam(put_string_val);
                // Extract the str field
                let str_addr = LLVMBuildGEP(self.builder, string_param_val, self.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _, 2, name!(b"gep_str_from_String\0"));
                // Load the pointer address
                let str_val = LLVMBuildLoad(self.builder, str_addr, name!(b"load_str_val\0"));
                // Call the runtime
                LLVMBuildCall(self.builder, put_string_inner_val, params!(str_val), 1, name!(b"\0"));
                LLVMBuildRetVoid(self.builder);
                self.add_function("IO_pub_static_method_putString_String_0_void_0", put_string_val).unwrap();
            }

            println!("gen putInt");
            // putInt
            {
                // Interface of Runtime
                let put_int_inner_ty = LLVMFunctionType(self.void_type(), params!(self.int_type()), 1, 0);
                let put_int_inner_val = LLVMAddFunction(self.module, IO_PUT_INT_NAME, put_int_inner_ty);

                // Wrapper method
                let put_int_ty = LLVMFunctionType(self.void_type(), params!(self.int_type()), 1, 0);
                let put_int_val = LLVMAddFunction(self.module, name!(b"IO_putInt\0"), put_int_ty);
                let put_int_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), put_int_val, name!(b"IO_putInt_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, put_int_bb);
                let int_param_val = LLVMGetFirstParam(put_int_val);
                LLVMBuildCall(self.builder,put_int_inner_val, params!(int_param_val), 1, name!(b"\0"));
                LLVMBuildRetVoid(self.builder);
                self.add_function("IO_pub_static_method_putInt_int_0_void_0", put_int_val).unwrap();
            }

            println!("gen getChar");
            // getChar
            {
                // Interface of Runtime
                let get_char_inner_ty = LLVMFunctionType(self.char_type(), null!(), 0, 0);
                let get_char_inner_val = LLVMAddFunction(self.module, IO_GET_CHAR_NAME, get_char_inner_ty);

                // Called by decaf code
                let get_char_ty = LLVMFunctionType(self.char_type(), null!(), 0, 0);
                let get_char_val = LLVMAddFunction(self.module, name!(b"IO_getChar\0"), get_char_ty);
                let get_char_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), get_char_val, name!(b"IO_getChar_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_char_bb);
                let char_val = LLVMBuildCall(self.builder, get_char_inner_val, null!(), 0, name!(b"inner_char\0"));
                LLVMBuildRet(self.builder, char_val);
                self.add_function("IO_pub_static_method_getChar__char_0", get_char_val).unwrap();
            }

            println!("gen getLine");
            // getLine
            {
                // Note: Runtime should make sure the char array is always alive because we are not going to make copy
                // Interface of Runtime
                let get_line_inner_ty = LLVMFunctionType(self.char_ptr_type(), null!(), 0, 0);
                let get_line_inner_val = LLVMAddFunction(self.module, IO_GET_LINE_NAME, get_line_inner_ty);
                LLVMSetLinkage(get_line_inner_val, LLVMExternalLinkage);

                // Called by decaf code
                let get_line_ty = LLVMFunctionType(self.get_llvm_type_by_name("String", true).unwrap(), null!(), 0, 0);
                let get_line_val = LLVMAddFunction(self.module, name!(b"IO_getLine\0"), get_line_ty);
                let get_line_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), get_line_val, name!(b"IO_getLine_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_line_bb);
                let inner_line = LLVMBuildCall(self.builder, get_line_inner_val, null!(), 0, name!(b"inner_line\0"));

                // Construct a String class
                // Allocate memory
                let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String", false).unwrap(), name!(b"malloc_string\0"));
                println!("type of string_val: ");
                LLVMDumpType(LLVMTypeOf(string_val));
                let str_addr = LLVMBuildGEP(self.builder, string_val, self.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _, 2, name!(b"String_field_str\0"));
                LLVMBuildStore(self.builder, inner_line, str_addr);
                LLVMBuildRet(self.builder, string_val);

                self.add_function("IO_pub_static_method_getLine__String_0", get_line_val).unwrap();
            }

            println!("gen getInt");
            // getInt
            {
                // Interface of Runtime
                let get_int_inner_ty = LLVMFunctionType(self.int_type(), null!(), 0, 0);
                let get_int_inner_val = LLVMAddFunction(self.module, IO_GET_INT_NAME, get_int_inner_ty);

                // Called by decaf code
                let get_int_ty = LLVMFunctionType(self.int_type(), null!(), 0, 0);
                let get_int_val = LLVMAddFunction(self.module, name!(b"IO_getInt\0"), get_int_ty);
                let get_int_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), get_int_val, name!(b"IO_getInt_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_int_bb);
                let inner_int = LLVMBuildCall(self.builder,get_int_inner_val, null!(), 0, name!(b"inner_int\0"));
                LLVMBuildRet(self.builder, inner_int);

                self.add_function("IO_pub_static_method_getInt__int_0", get_int_val).unwrap();
            }

            println!("gen peek");
            // peek
            {
                // Interface of Runtime
                let peek_inner_ty = LLVMFunctionType(self.char_type(), null!(), 0, 0);
                let peek_inner_val = LLVMAddFunction(self.module, IO_PEEK_NAME, peek_inner_ty);

                // Called by decaf code
                let peek_ty = LLVMFunctionType(self.char_type(), null!(), 0, 0);
                let peek_val = LLVMAddFunction(self.module, name!(b"IO_peek\0"), peek_ty);
                let peek_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), peek_val, name!(b"IO_peek_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, peek_bb);
                let peek_char = LLVMBuildCall(self.builder, peek_inner_val, null!(), 0, name!(b"peek_char\0"));
                LLVMBuildRet(self.builder, peek_char);

                self.add_function("IO_pub_static_method_peek__char_0", peek_val).unwrap();
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
			let main_ty = LLVMFunctionType(self.int_type(), main_args.as_mut_ptr(), 0, LLVM_FALSE);
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

		for cls in program.classes.iter() {
			self.classes.insert(cls.name.clone(), cls.clone());
		}

        // First pass
        for cls in &program.classes {
            match cls.clone().gencode(self) {
                Ok(_) => {}
                Err(e) => {
                    return Err(format!("Compile error {:?}", e));
                }
            }
        }

        // Second pass
        self.state = Second;
        for cls in &program.classes {
            match cls.clone().gencode(self) {
                Ok(_) => {}
                Err(e) => {
                    return Err(format!("Compile error {:?}", e));
                }
            }
        }

        unsafe {
            let mut message = [b'\0'; 1000];
            LLVMVerifyModule(self.module, LLVMAbortProcessAction, message.as_mut_ptr() as *mut _);
        }

		Ok(self.module.clone())
	}

}

trait CodeGenRc {
    fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult;
}

trait CodeGen {
    fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult;
}

impl CodeGenRc for decaf::Class {
	fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult {

        // Skip built-in classes
        if self.name == "Object" || self.name == "String" || self.name == "IO" {
            return Ok(OK);
        }

        println!("gencode for class: {}", self.name);

        match generator.state {
            GenState::First => {
                // Define vtable type
                let vtable_ty;
                let vtable_global;
                unsafe {
                    vtable_ty = LLVMArrayType(generator.i64_ptr_type(), self.vtable.borrow().len() as u32);
                    vtable_global = LLVMAddGlobal(generator.module, vtable_ty,
                                                  generator.new_string_ptr(&vtable_name!(self.name)));
                }

                generator.add_global(&vtable_name!(self.name), vtable_global)?;

                // Define LLVM struct type for class
                let mut field_llvm_types: Vec<LLVMTypeRef> = vec![vtable_ty];
                field_llvm_types.extend(generator.tys2llvmtypes(
					self.pub_fields.borrow().iter().map(|f| f.ty.borrow().clone()).collect::<Vec<Rc<decaf::Type>>>()));
                field_llvm_types.extend(generator.tys2llvmtypes(
					self.prot_fields.borrow().iter().map(|f| f.ty.borrow().clone()).collect::<Vec<Rc<decaf::Type>>>()));
                field_llvm_types.extend(generator.tys2llvmtypes(
					self.priv_fields.borrow().iter().map(|f| f.ty.borrow().clone()).collect::<Vec<Rc<decaf::Type>>>()));
                let cls_llvm_type;
                unsafe {
                    cls_llvm_type = LLVMStructType(field_llvm_types.as_mut_ptr() as *mut _, field_llvm_types.len() as u32, 0);
                }
                generator.add_llvm_type(&self.name, cls_llvm_type.clone())?;

                // Define types for all methods/ctors/static methods

                unsafe {
                    for pub_ctor in self.pub_ctors.borrow().iter() {
                        let ctor_ty;
                        let ctor_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                        args.extend(generator.tys2llvmtypes(
							pub_ctor.args.borrow().iter()
                            .map(|(_, ty)| ty.clone())
                            .collect::<Vec<Rc<decaf::Type>>>()));
                        ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                   args.as_mut_ptr() as *mut _,
                                                   pub_ctor.args.borrow().len() as u32,
                                                   0);
                        ctor_val = LLVMAddFunction(generator.module,
                                                   generator.new_string_ptr(&pub_ctor.llvm_name()),
                                                   ctor_ty);
                        generator.add_function(&pub_ctor.llvm_name(), ctor_val)?;

                    }

                    for prot_ctor in self.prot_ctors.borrow().iter() {
                        let ctor_ty;
                        let ctor_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                        args.extend(generator.tys2llvmtypes(
							prot_ctor.args.borrow().iter()
								.map(|(_, ty)| ty.clone())
								.collect::<Vec<Rc<decaf::Type>>>()));
                        ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                   args.as_mut_ptr() as *mut _,
                                                   prot_ctor.args.borrow().len() as u32,
                                                   0);
                        ctor_val = LLVMAddFunction(generator.module,
                                                   generator.new_string_ptr(&prot_ctor.llvm_name()),
                                                   ctor_ty);
                        generator.add_function(&prot_ctor.llvm_name(), ctor_val)?;

                    }

                    for priv_ctor in self.priv_ctors.borrow().iter() {
                        let ctor_ty;
                        let ctor_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                        args.extend(generator.tys2llvmtypes(
							priv_ctor.args.borrow().iter()
								.map(|(_, ty)| ty.clone())
								.collect::<Vec<Rc<decaf::Type>>>()));
                        ctor_ty = LLVMFunctionType(cls_llvm_type.clone(),
                                                   args.as_mut_ptr() as *mut _,
                                                   priv_ctor.args.borrow().len() as u32,
                                                   0);
                        ctor_val = LLVMAddFunction(generator.module,
                                                   generator.new_string_ptr(&priv_ctor.llvm_name()),
                                                   ctor_ty);
                        generator.add_function(&priv_ctor.llvm_name(), ctor_val)?;

                    }

                    for pub_method in self.pub_methods.borrow().iter() {
                        let method_ty;
                        let method_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
						args.extend(generator.tys2llvmtypes(
							pub_method.args.borrow().iter()
								.map(|(_, ty)| ty.clone())
								.collect::<Vec<Rc<decaf::Type>>>()));
                        method_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&pub_method.return_ty.borrow(), false, true).unwrap(),
                                                     args.as_mut_ptr() as *mut _,
                                                     pub_method.args.borrow().len() as u32,
                                                     0);
                        method_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&pub_method.llvm_name()),
                                                     method_ty);
                        generator.add_function(&pub_method.llvm_name(), method_val)?;

                    }

                    for prot_method in self.prot_methods.borrow().iter() {
                        let method_ty;
                        let method_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                        args.extend(generator.tys2llvmtypes(
							prot_method.args.borrow().iter()
								.map(|(_, ty)| ty.clone())
								.collect::<Vec<Rc<decaf::Type>>>()));
                        method_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&prot_method.return_ty.borrow(), false, true).unwrap(),
                                                     args.as_mut_ptr() as *mut _,
                                                     prot_method.args.borrow().len() as u32,
                                                     0);
                        method_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&prot_method.llvm_name()),
                                                     method_ty);
                        generator.add_function(&prot_method.llvm_name(), method_val)?;

                    }

                    for priv_method in self.priv_methods.borrow().iter() {
                        let method_ty;
                        let method_val;
                        let mut args = vec![cls_llvm_type.clone()]; // The "this" reference
                        args.extend(generator.tys2llvmtypes(
							priv_method.args.borrow().iter()
								.map(|(_, ty)| ty.clone())
								.collect::<Vec<Rc<decaf::Type>>>()));
                        method_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&priv_method.return_ty.borrow(), false, true).unwrap(),
                                                     args.as_mut_ptr() as *mut _,
                                                     priv_method.args.borrow().len() as u32,
                                                     0);
                        method_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&priv_method.llvm_name()),
                                                     method_ty);
                        generator.add_function(&priv_method.llvm_name(), method_val)?;
                    }

                    for pub_static in self.pub_static_methods.borrow().iter() {
                        let static_ty;
                        let static_val;
                        static_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&pub_static.return_ty.borrow(), false, true).unwrap(),
                                                     generator.tys2llvmtypes(pub_static.args.borrow().iter()
                                                         .map(|(_, ty)| ty.clone())
                                                         .collect::<Vec<Rc<decaf::Type>>>()).as_mut_ptr() as *mut _,
                                                     pub_static.args.borrow().len() as u32,
                                                     0);
                        static_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&pub_static.llvm_name()),
                                                     static_ty);
                        generator.add_function(&pub_static.llvm_name(), static_val)?;
                        println!("added function: {}", pub_static.llvm_name());
                    }

                    for prot_static in self.prot_static_methods.borrow().iter() {
                        let static_ty;
                        let static_val;
                        static_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&prot_static.return_ty.borrow(), false, true).unwrap(),
                                                     generator.tys2llvmtypes(prot_static.args.borrow().iter()
																			 .map(|(_, ty)| ty.clone())
																			 .collect::<Vec<Rc<decaf::Type>>>()).as_mut_ptr() as *mut _,
                                                     prot_static.args.borrow().len() as u32,
                                                     0);
                        static_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&prot_static.llvm_name()),
                                                     static_ty);
                        generator.add_function(&prot_static.llvm_name(), static_val)?;

                    }

                    for priv_static in self.priv_static_methods.borrow().iter() {
                        let static_ty;
                        let static_val;
                        static_ty = LLVMFunctionType(generator.get_llvm_type_from_decaf_type(&priv_static.return_ty.borrow(), false, true).unwrap(),
                                                     generator.tys2llvmtypes(priv_static.args.borrow().iter()
																			 .map(|(_, ty)| ty.clone())
																			 .collect::<Vec<Rc<decaf::Type>>>()).as_mut_ptr() as *mut _,
                                                     priv_static.args.borrow().len() as u32,
                                                     0);
                        static_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&priv_static.llvm_name()),
                                                     static_ty);
                        generator.add_function(&priv_static.llvm_name(), static_val)?;

                    }
                }
            }
            GenState::Second => {
                // Visit each ctor/method/static method
                for ctor in self.pub_ctors.borrow().iter() {
                    ctor.clone().gencode(generator)?;
                }

                for ctor in self.prot_ctors.borrow().iter() {
                    ctor.clone().gencode(generator)?;
                }

                for ctor in self.priv_ctors.borrow().iter() {
                    ctor.clone().gencode(generator)?;
                }

                for method in self.pub_methods.borrow().iter() {
                    method.clone().gencode(generator)?;
                }

                for method in self.prot_methods.borrow().iter() {
                    method.clone().gencode(generator)?;
                }

                for method in self.priv_methods.borrow().iter() {
                    method.clone().gencode(generator)?;
                }

                for static_method in self.pub_static_methods.borrow().iter() {
                    static_method.clone().gencode(generator)?;
                }

                for static_method in self.prot_static_methods.borrow().iter() {
                    static_method.clone().gencode(generator)?;
                }

                for static_method in self.priv_static_methods.borrow().iter() {
                    static_method.clone().gencode(generator)?;
                }
            }
        }

        Ok(OK)
	}
}

impl CodeGenRc for decaf::Ctor {
    fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Second);
        // Get function llvm value
		let name = self.llvm_name();
		match generator.get_function_by_name(&name) {
			Some(ctor_llvm_val) => {
				// Create basic block
				unsafe {
					let ctor_bb = LLVMAppendBasicBlockInContext(
						LLVMGetModuleContext(generator.module),
						ctor_llvm_val,
						generator.new_string_ptr(
							&format!("{}_mainbb",
									 self.llvm_name()))
					);
					LLVMPositionBuilderAtEnd(generator.builder, ctor_bb);
				}

                generator.scopes.push(CtorScope(self.clone()));

				// Set argument variable address
				let this_ref = unsafe { LLVMGetFirstParam(ctor_llvm_val) };
				self.vartbl.borrow_mut().set_addr_for_name("this", this_ref);

				for (ix, (arg_name, _)) in self.args.borrow().iter().enumerate() {
					let arg_val = unsafe { LLVMGetParam(ctor_llvm_val, ix as u32 + 1) };
					self.vartbl.borrow_mut().set_addr_for_name(arg_name, arg_val);
				}

				// Visit body
				let ret = decaf::Stmt::Block(self.body.borrow().clone().unwrap()).gencode(generator);

                generator.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGenRc for decaf::Method {
	fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Second);
		let name = self.llvm_name();
		match generator.get_function_by_name(&name) {
			Some(method_llvm_val) => {
				// Create basic block
				unsafe {
					let method_bb = LLVMAppendBasicBlockInContext(
						LLVMGetModuleContext(generator.module),
						method_llvm_val,
						generator.new_string_ptr(
							&format!("{}_mainbb",
									 self.llvm_name()))
					);
					LLVMPositionBuilderAtEnd(generator.builder, method_bb);
				}

                generator.scopes.push(MethodScope(self.clone()));

				// Set argument variable address
				let this_ref = unsafe { LLVMGetFirstParam(method_llvm_val) };
				self.vartbl.borrow_mut().set_addr_for_name("this", this_ref);

				for (ix, (arg_name, _)) in self.args.borrow().iter().enumerate() {
					let arg_val = unsafe { LLVMGetParam(method_llvm_val, ix as u32 + 1) };
					self.vartbl.borrow_mut().set_addr_for_name(arg_name, arg_val);
				}

				// Visit body
                let ret = decaf::Stmt::Block(self.body.borrow().clone().unwrap()).gencode(generator);

                generator.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGen for decaf::Stmt {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Second);
		match self {
            Declare(stmt) => {
                println!("I am in declare");
                let lhs_addr = {
                    let want_ptr =  {
                        match &stmt.ty.base {
                            ClassTy(_) => true,
                            IntTy | BoolTy | CharTy => match stmt.ty.array_lvl == 0 {
                                true => false,
                                false => true,
                            },
                            _ => {
                                return Err(EInvalidLHSType(stmt.ty.clone()));
                            }
                        }
                    };
                    // Allocate the entire thing
                    if let Some(llvm_ty) = generator.get_llvm_type_from_decaf_type(&stmt.ty, want_ptr, true) {
                        // Allocate lhs
                        unsafe {
                            LLVMBuildAlloca(generator.builder, llvm_ty,
                                            generator.new_string_ptr(&stmt.name))
                        }
                    } else {
                        return Err(EUnknownType(format!("{:?}", stmt.ty)));
                    }
                };

                let blk = generator.get_top_most_block().unwrap();
                blk.vartbl.borrow_mut().set_addr_for_name(&stmt.name, lhs_addr);

                match &stmt.init_expr {
                    Some(expr) => {
                        match expr.gencode(generator)? {
                            CodeValue::Value(val) => {
                                if stmt.ty.is_compatible_with(&val.get_type()) {
                                    println!("ready to store");
                                    unsafe {
                                        println!("type of val:");
                                        LLVMDumpType(LLVMTypeOf(val.get_value(generator)));
                                        println!("type of lhs_addr:");
                                        LLVMDumpType(LLVMTypeOf(lhs_addr));
                                        LLVMBuildStore(generator.builder, val.get_value(generator), lhs_addr);
                                    }
                                    println!("store completed");

                                    Ok(OK)
                                } else {
                                    Ok(OK)
                                }
                            }
                            _ => Err(EValueNotFound(format!("{:?}", expr)))
                        }
                    }
                    None => {
                        if stmt.ty.array_lvl > 0 {
                            Err(EUninitializedArray(format!("{:?}", stmt)))
                        } else {
                            Ok(OK)
                        }
                    }
                }
            }
            If(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        // Evaluate condition
                        match stmt.cond.gencode(generator)? {
                            CodeValue::Value(cond_val) => {
                                unsafe {
                                    let ifbb = LLVMAppendBasicBlockInContext(
                                        LLVMGetModuleContext(generator.module),
                                        func_val,
                                        generator.next_name("if"));
                                    let nextbb = LLVMAppendBasicBlockInContext(
                                        LLVMGetModuleContext(generator.module),
                                        func_val,
                                        generator.next_name("ifnext"));
                                    LLVMBuildCondBr(generator.builder, cond_val.get_value(generator), ifbb, nextbb);

                                    Block(stmt.thenblock.clone()).gencode(generator)?;

                                    if let None = Block(stmt.thenblock.clone()).returnable() {
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
                            CodeValue::Value(cond_val) => {
                                unsafe {
                                    let ifbb = LLVMAppendBasicBlockInContext(
                                        LLVMGetModuleContext(generator.module),
                                        func_val,
                                        generator.next_name("if"));
                                    let elsebb = LLVMAppendBasicBlockInContext(
                                        LLVMGetModuleContext(generator.module),
                                        func_val,
                                        generator.next_name("else"));
                                    let nextbb = LLVMAppendBasicBlockInContext(
                                        LLVMGetModuleContext(generator.module),
                                        func_val,
                                        generator.next_name("ifelsenext"));
                                    LLVMBuildCondBr(generator.builder,
                                                    cond_val.get_value(generator),
                                                    ifbb,
                                                    nextbb);

                                    LLVMPositionBuilderAtEnd(generator.builder, ifbb);

                                    generator.scopes.push(BlockScope(stmt.thenblock.clone()));

                                    Block(stmt.thenblock.clone()).gencode(generator)?;

                                    generator.scopes.pop();

                                    if let None = Block(stmt.thenblock.clone()).returnable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, elsebb);

                                    generator.scopes.push(BlockScope(stmt.elseblock.clone()));

                                    Block(stmt.elseblock.clone()).gencode(generator)?;

                                    generator.scopes.pop();

                                    if let None = Block(stmt.elseblock.clone()).returnable() {
                                        LLVMBuildBr(generator.builder, elsebb);
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
            Expr(stmt) => {
                stmt.expr.gencode(generator)?;
                Ok(OK)
            }
            While(stmt) => {
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        unsafe {
                            let condbb = LLVMAppendBasicBlockInContext(
                                LLVMGetModuleContext(generator.module),
                                func_val,
                                generator.next_name("whilecond"));
                            let bodybb = LLVMAppendBasicBlockInContext(
                                LLVMGetModuleContext(generator.module),
                                func_val,
                                generator.next_name("whilebody"));
                            let nextbb = LLVMAppendBasicBlockInContext(
                                LLVMGetModuleContext(generator.module),
                                func_val,
                                generator.next_name("whilenext"));

                            *stmt.condbb.borrow_mut() = Some(condbb.clone());
                            *stmt.bodybb.borrow_mut() = Some(bodybb.clone());
                            *stmt.nextbb.borrow_mut() = Some(nextbb.clone());

                            LLVMBuildBr(generator.builder, condbb);

                            LLVMPositionBuilderAtEnd(generator.builder, condbb);

                            match stmt.condexpr.gencode(generator)? {
                                CodeValue::Value(cond_val) => {
                                    LLVMBuildCondBr(generator.builder, cond_val.get_value(generator), bodybb, nextbb);

                                    LLVMPositionBuilderAtEnd(generator.builder, bodybb);

                                    generator.scopes.push(WhileScope(stmt.clone()));

                                    generator.scopes.push(BlockScope(stmt.bodyblock.borrow().clone().unwrap()));

                                    Block(stmt.bodyblock.borrow().clone().unwrap()).gencode(generator)?;

                                    generator.scopes.pop();

                                    generator.scopes.pop();

                                    // NOTE: we might add Br no matter what
                                    if let None = Block(stmt.bodyblock.borrow().clone().unwrap()).returnable() {
                                        LLVMBuildBr(generator.builder, condbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);
                                    Ok(OK)
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
                    Some(_) => {
                        unsafe {
                            if let Some(ret_expr) = &stmt.expr {
                                match ret_expr.gencode(generator)? {
                                    CodeValue::Value(ret_val) => {
                                        LLVMBuildRet(generator.builder, ret_val.get_value(generator));
                                        Ok(OK)
                                    }
                                    _ => Err(EValueNotFound(format!("{:?}", ret_expr)))
                                }
                            } else {
                                LLVMBuildRetVoid(generator.builder);
                                Ok(OK)
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
                            Ok(OK)
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
                            Ok(OK)
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
                            let mut arg_vals = vec![this_ref];
                            for arg in stmt.args.iter() {
                                match arg.gencode(generator)? {
                                    CodeValue::Value(val) => {
                                        arg_vals.push(val.get_value(generator));
                                    }
                                    _ => {
                                        return Err(EValueNotFound(format!("{:?}", arg)));
                                    }
                                }
                            }
                            let len = arg_vals.len();
                            match generator.get_function_by_name(&stmt.sup_ctor.llvm_name()) {
                                Some(ctor_val) => {
                                    LLVMBuildCall(generator.builder,
                                                  ctor_val,
                                                  params!(arg_vals),
                                                  len as u32,
                                                  generator.next_name(
                                                      &format!("function_call_{}",
                                                               stmt.sup_ctor.llvm_name())));
                                    Ok(OK)
                                }
                                None => Err(EFunctionNotFound(stmt.sup_ctor.llvm_name()))
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{:?}", stmt)))
                }
            }
            Block(stmt) => {
                println!("========Block=======");
                generator.scopes.push(BlockScope(stmt.clone()));
                for (ix, st) in stmt.stmts.borrow().iter().enumerate() {
                    println!("=======Stmt {}======", ix);
                    st.gencode(generator)?;
                    if let Some(_) = st.returnable() {
                        // No need to generate code for the rest
                        break
                    }
                }
                println!("=======Block returnable======");
                if let None = Block(stmt.clone()).returnable() {
                    // Generate return
                    // The outer function must have return type of void, this is checked by semantic processing
                    println!("=======returning void=======");
                    unsafe {
                        LLVMBuildRetVoid(generator.builder);
                    }
                } else {
                    println!("=========block is returnable========");
                }
                Ok(OK)
            }
            NOP => Ok(OK)
		}
	}
}

impl CodeGen for decaf::Expr {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        match &*self {
            Assign(expr) => {
                println!("I am in Assign");
                match (&*expr.rhs).gencode(generator)? {
                    CodeValue::Value(val_rhs) => {
                        println!("gencode lhs");
                        match (&*expr.lhs).gencode(generator)? {
                            CodeValue::Value(Addr(val_lhs)) => {
                                // Assuming type checks done
                                unsafe {
									let llvm_val = val_rhs.get_value(generator);
									let casted_val = generator.cast_value(&val_rhs.get_type(),
																		  &val_lhs.ty,
																		  llvm_val);
                                    LLVMBuildStore(generator.builder,
                                                   casted_val,
                                                   val_lhs.addr.borrow().clone().unwrap());
                                    // enable chained equal without exposing address
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), val_rhs.get_value(generator))))
                                }
                            }
                            CodeValue::Value(_) => Err(ENonAddressableValueOnLHS(format!("{:?}", expr.lhs))),
                            _ => Err(EValueNotFound(format!("{:?}", expr.lhs)))
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr.rhs)))
                }
			}
			BinArith(expr) => {
                match ((&*expr.lhs).gencode(generator)?, (&*expr.rhs).gencode(generator)?) {
                    (CodeValue::Value(val_lhs), CodeValue::Value(val_rhs)) => {
                        use crate::lnp::past::BinOp::*;
                        unsafe {
                            match expr.op {
                                PlusOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildAdd(generator.builder,
                                                 val_lhs.get_value(generator),
                                                 val_rhs.get_value(generator),
                                                 generator.next_name("add")))))
                                }
                                MinusOp => {
                                    let neg = LLVMBuildNeg(generator.builder,
                                                           val_rhs.get_value(generator),
                                                           generator.next_name("neg"));
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildAdd(generator.builder,
                                                 val_lhs.get_value(generator),
                                                 neg,
                                                 generator.next_name("add")))))
                                }
                                TimesOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildMul(generator.builder,
                                                 val_lhs.get_value(generator),
                                                 val_rhs.get_value(generator),
                                                 generator.next_name("mul")))))
                                }
                                DivideOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildSDiv(generator.builder,
                                                  val_lhs.get_value(generator),
                                                  val_rhs.get_value(generator),
                                                  generator.next_name("div")))))
                                }
                                ModOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildSRem(generator.builder,
                                                  val_lhs.get_value(generator),
                                                  val_rhs.get_value(generator),
                                                  generator.next_name("mod")))))
                                }
                                _ => Err(ENonArithOp(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			UnArith(expr) => {
                use crate::lnp::past::UnOp::*;
                match (&*expr.rhs).gencode(generator)? {
                    CodeValue::Value(val_rhs) => {
                        unsafe {
                            match expr.op {
                                PlusUOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), val_rhs.get_value(generator))))
                                }
                                MinusUOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildNeg(
                                        generator.builder,
                                        val_rhs.get_value(generator),
                                        generator.next_name("neg")))))
                                }
                                _ => Err(EUnaryArithNotFound(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			BinLogical(expr) => {
                 match ((&*expr.lhs).gencode(generator)?, (&*expr.rhs).gencode(generator)?) {
                     (CodeValue::Value(val_lhs), CodeValue::Value(val_rhs)) => {
                         use crate::lnp::past::BinOp::*;
                         unsafe {
                             match expr.op {
                                 LogicalAndOp => {
                                     Ok(CodeValue::Value(Val(val_lhs.get_type(),
                                                  LLVMBuildAnd(generator.builder,
                                                               val_lhs.get_value(generator),
                                                               val_rhs.get_value(generator),
                                                               generator.next_name("and")))))
                                 }
                                 LogicalOrOp => {
                                     // TODO: c-type or
                                     Ok(CodeValue::Value(Val(val_lhs.get_type(),
                                                  LLVMBuildOr(generator.builder,
                                                              val_lhs.get_value(generator),
                                                              val_rhs.get_value(generator),
                                                              generator.next_name("or")))))
                                 }
                                 _ => Err(ENonLogicalOp(format!("{:?}", expr.op)))
                             }
                         }
                     }
                     _ => Err(EValueNotFound(format!("{:?}", expr)))
                 }
			}
			UnNot(expr) => {
                match (&*expr.rhs).gencode(generator)? {
                    CodeValue::Value(val) => {
                        unsafe {
                            Ok(CodeValue::Value(Val(val.get_type(),
                                         LLVMBuildNot(generator.builder,
                                                      val.get_value(generator),
                                                      generator.next_name("not")))))
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			BinCmp(expr) => {
                match ((&*expr.lhs).gencode(generator)?, (&*expr.rhs).gencode(generator)?) {
                    (CodeValue::Value(val_lhs), CodeValue::Value(val_rhs)) => {
                        use crate::lnp::past::BinOp::*;
                        unsafe {
                            match expr.op {
                                GreaterThanOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSGT,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("gt")))))
                                }
                                GreaterOrEqOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSGE,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("ge")))))
                                }
                                LessThanOp => {
									Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSLT,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("lt")))))
                                }
                                LessOrEqOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSLE,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("le")))))
                                }
                                EqualsOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntEQ,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("eq")))))
                                }
                                NotEqualsOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntNE,
													  val_lhs.get_value(generator),
													  val_rhs.get_value(generator),
													  generator.next_name("ne")))))
                                }
                                _ => Err(ENonCmpOp(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			CreateArray(expr) => {
                if expr.dims.len() > 0 {
                    match generator.get_top_most_function_val() {
                        Some(func_val) => {
                            struct LoopInfo {
//                                init_bb: Option<LLVMBasicBlockRef>,
//                                cond_bb: Option<LLVMBasicBlockRef>,
//                                body_bb: LLVMBasicBlockRef,
                                finish_bb: LLVMBasicBlockRef,
//                                control_var: Option<LLVMValueRef>,
//                                array_ty: LLVMTypeRef,
//                                array_var: LLVMValueRef,
                                array_inner_val: LLVMValueRef,
                                rep_count: LLVMValueRef,
                            }

                            // Dynamically construct nested array
                            let mut dim_vals = vec![];
                            let mut loop_infos = vec![];
                            let mut array_result = std::ptr::null_mut();

                            let lvl = expr.dims.len() as u32;
                            for dim in expr.dims.iter() {
                                match dim.gencode(generator)? {
                                    CodeValue::Value(dim_val) => {
                                        dim_vals.push(dim_val)
                                    }
                                    _ => {
                                        return Err(EValueNotFound(format!("{:?}", dim)));
                                    }
                                }
                            }

                            unsafe {
                                for (ix, dim_val) in dim_vals.iter().enumerate() {
                                    if ix == 0 {
                                        // body0
                                        let next_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_next")
                                        );
                                        let body_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_body")
                                        );
                                        let ty = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl
                                            },
                                            false,
											true
                                        );
                                        let ty_inner = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl - 1,
                                            },
                                            false,
											true
                                        );

                                        LLVMBuildBr(generator.builder, body_bb);
                                        LLVMPositionBuilderAtEnd(generator.builder, body_bb);
                                        let dim_llvm_val = dim_val.get_value(generator);
                                        let array_var = LLVMBuildMalloc(
                                            generator.builder,
                                            ty.unwrap(),
                                            generator.next_name("aloop_array_struct")
                                        );
                                        let array_inner_val = LLVMBuildArrayMalloc(
                                            generator.builder,
                                            ty_inner.unwrap(),
                                            dim_llvm_val,
                                            generator.next_name("aloop_array_inner")
                                        );
                                        let inner_addr = LLVMBuildGEP(
                                            generator.builder,
                                            array_var,
                                            generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                            2,
                                            generator.next_name("aloop_gep_array_inner_addr")
                                        );
                                        let sz_addr = LLVMBuildGEP(
                                            generator.builder,
                                            array_var,
                                            generator.indices(vec![0, 1]).as_mut_slice().as_mut_ptr() as *mut _,
                                            2,
                                            generator.next_name("aloop_gep_array_sz_addr")
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            array_inner_val,
                                            inner_addr
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            dim_llvm_val,
                                            sz_addr,
                                        );

                                        let info = LoopInfo {
//                                            init_bb: None,
//                                            cond_bb: None,
//                                            body_bb,
                                            finish_bb: next_bb,
//                                            control_var: None,
//                                            array_ty: ty.unwrap(),
//                                            array_var,
                                            array_inner_val,
                                            rep_count: dim_llvm_val,
                                        };
                                        loop_infos.push(info);
                                        array_result = array_var;
                                    }
                                    else {
                                        let init_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_init")
                                        );
                                        let cond_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_cond")
                                        );
                                        let body_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_body")
                                        );
                                        let finish_bb = LLVMAppendBasicBlockInContext(
                                            LLVMGetModuleContext(generator.module),
                                            func_val,
                                            generator.next_name("aloop_finish")
                                        );
                                        let ty = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl - ix as u32
                                            },
                                            false,
											true
                                        );
                                        let inner_ty = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl - ix as u32 - 1,
                                            },
                                            false,
											true
                                        );

                                        LLVMBuildBr(generator.builder, init_bb);
                                        LLVMPositionBuilderAtEnd(generator.builder, init_bb);
                                        let control_var = LLVMBuildAlloca(
                                            generator.builder,
                                            generator.int_type(),
                                            generator.next_name("aloop_control_var")
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            generator.const_i64(0),
                                            control_var
                                        );
                                        LLVMBuildBr(generator.builder, cond_bb);
                                        LLVMPositionBuilderAtEnd(generator.builder, cond_bb);
                                        let control_var_val = LLVMBuildLoad(
                                            generator.builder,
                                            control_var,
                                            generator.next_name("aloop_load_control_var")
                                        );
                                        let cmp_result = LLVMBuildICmp(
                                            generator.builder,
                                            LLVMIntSLT,
                                            control_var_val,
                                            loop_infos.last().unwrap().rep_count,
                                            generator.next_name("aloop_cmp_control_var")
                                        );
                                        LLVMBuildCondBr(
                                            generator.builder,
                                            cmp_result,
                                            body_bb,
                                            loop_infos.last().unwrap().finish_bb
                                        );

                                        LLVMPositionBuilderAtEnd(generator.builder, finish_bb);
                                        let next_control_val = LLVMBuildAdd(
                                            generator.builder,
                                            control_var_val,
                                            generator.const_i64(1),
                                            generator.next_name("aloop_inc_control_var"),
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            next_control_val,
                                            control_var
                                        );
                                        LLVMBuildBr(generator.builder, cond_bb);

                                        LLVMPositionBuilderAtEnd(generator.builder, body_bb);
                                        let dim_llvm_val = dim_val.get_value(generator);
                                        let array_var = LLVMBuildMalloc(
                                            generator.builder,
                                            ty.unwrap(),
                                            generator.next_name("aloop_array_struct")
                                        );
                                        let array_inner_val = LLVMBuildArrayMalloc(
                                            generator.builder,
                                            inner_ty.unwrap(),
                                            dim_llvm_val,
                                            generator.next_name("aloop_array_inner")
                                        );
                                        let inner_addr = LLVMBuildGEP(
                                            generator.builder,
                                            array_var,
                                            generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                            2,
                                            generator.next_name("aloop_gep_array_inner_addr")
                                        );
                                        let sz_addr = LLVMBuildGEP(
                                            generator.builder,
                                            array_var,
                                            generator.indices(vec![0, 1]).as_mut_slice().as_mut_ptr() as *mut _,
                                            2,
                                            generator.next_name("aloop_gep_array_sz_addr")
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            array_inner_val,
                                            inner_addr,
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            dim_llvm_val,
                                            sz_addr,
                                        );

                                        let element_addr = LLVMBuildGEP(
                                            generator.builder,
                                            loop_infos.last().unwrap().array_inner_val,
                                            params!(control_var_val),
                                            1,
                                            generator.next_name("aloop_gep_array_elem_addr")
                                        );
                                        LLVMBuildStore(
                                            generator.builder,
                                            array_var,
                                            element_addr
                                        );

                                        if ix as u32 == lvl - 1 { // last layer
                                            LLVMBuildBr(generator.builder, finish_bb);
                                        }
                                        loop_infos.push(LoopInfo {
//                                            init_bb: Some(init_bb),
//                                            cond_bb: Some(cond_bb),
//                                            body_bb,
                                            finish_bb,
//                                            control_var: Some(control_var),
//                                            array_ty: ty.unwrap(),
//                                            array_var,
                                            array_inner_val,
                                            rep_count: dim_llvm_val
                                        });
                                    }
                                }
                            }

                            Ok(CodeValue::Value(Val(
                                decaf::Type {
                                    base: expr.ty.clone(),
                                    array_lvl: expr.dims.len() as u32
                                },
                                array_result)))
                        }
                        None => Err(EBlockNotInFunction(format!("{:?}", expr)))
                    }
                } else {
                    Err(EArrayZeroDim(format!("{:?}", expr)))
                }
            }
            Literal(expr) => {
                match expr {
                    IntLiteral(i) => {
                        Ok(CodeValue::Value(Val(decaf::Type {
                            base: IntTy,
                            array_lvl: 0,
                        }, generator.const_i64(*i))))
                    }
                    BoolLiteral(b) => {
                        Ok(CodeValue::Value(Val(decaf::Type {
                            base: BoolTy,
                            array_lvl: 0,
                        }, generator.const_bool(*b))))
                    }
                    CharLiteral(c) => {
                        Ok(CodeValue::Value(Val(decaf::Type {
                            base: CharTy,
                            array_lvl: 0,
                        }, generator.const_char(*c))))
                    }
                    StrLiteral(_) => {
                        panic!("StrLiteral should not not be accessed")
                    }
                }
            }
			This(_) => {
                match generator.variable_lookup("this") {
                    Some(var) => {
                        Ok(CodeValue::Value(Addr(var)))
                    }
                    None => Err(EThisNotFound)
                }
			}
			CreateObj(expr) => {
                use decaf::LiteralExpr::*;
                println!("I am in CreateObj of {:?}", expr);
                // Specical case for String
                match expr.cls.name == "String" {
                    true => {
                        println!("I am in special handling of String creation");
                        if expr.args.len() == 1 {
                            match &expr.args[0] {
                                Literal(StrLiteral(s)) => {
                                   Ok(CodeValue::Value(Val(decaf::Type {
                                           base: ClassTy(generator.class_lookup("String").unwrap()),
                                           array_lvl: 0
                                       },
                                       generator.new_string_class(s)
                                   )))
                                }
                                _ => Err(EInvalidStringCreation(format!("{:?}", expr)))
                            }
                        } else {
                            Err(EInvalidStringCreation(format!("{:?}", expr)))
                        }
                    }
                    false => {
                        match generator.get_function_by_name(&expr.ctor.as_ref().unwrap().llvm_name()) {
                            Some(ctor_func_val) => {

                                // Allocate first
                                let this;
                                unsafe {
                                    this = LLVMBuildMalloc(
                                        generator.builder,
                                        generator.get_llvm_type_by_name(&expr.cls.name, false).unwrap(),
                                        generator.next_name(&format!("new_{}", &expr.cls.name))
                                    );
                                    // Store virtual table pointer
                                    let virtual_table_val = generator.get_global_by_name(&expr.cls.name).unwrap();
                                    let virtual_table_addr = LLVMBuildGEP(
                                        generator.builder,
                                        this,
                                        generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_vtable_addr_{}", &expr.cls.name))
                                    );
                                    LLVMBuildStore(
                                        generator.builder,
                                        virtual_table_val,
                                        virtual_table_addr
                                    );

                                    // TODO zero all the fields
                                }

                                let mut arg_vals = vec![this];
                                for (ix, arg) in expr.args.iter().enumerate() {
                                    match arg.gencode(generator)? {
                                        CodeValue::Value(arg_val) => {
                                            // Type casting
                                            let arg_llvm_val = arg_val.get_value(generator);
                                            let casted_val = generator.cast_value(&arg_val.get_type(),  // source type
                                                                                  &expr.ctor.as_ref().unwrap().args.borrow()[ix].1.as_ref(), // destination type
                                                                                  arg_llvm_val // source value
                                            );

                                            arg_vals.push(casted_val);
                                        }
                                        _ => {
                                            return Err(EValueNotFound(format!("{:?}", arg)));
                                        }
                                    }
                                }
                                unsafe {
                                    Ok(CodeValue::Value(Val(
                                        decaf::Type {
                                            base: ClassTy(expr.cls.clone()),
                                            array_lvl: 0,
                                        },
                                        LLVMBuildCall(
                                            generator.builder,
                                            ctor_func_val,
                                            arg_vals.as_slice().as_ptr() as *mut _,
                                            arg_vals.len() as u32,
                                            generator.next_name(&format!("ctor_{}", &expr.cls.name)),
                                    ))))
                                }
                            }
                            None => Err(EFunctionNotFound(expr.ctor.as_ref().unwrap().llvm_name()))
                        }
                    }
                }
			}
			MethodCall(expr) => {
                use decaf::Expr::*;
                use decaf::Visibility::*;

                println!("I am in methodCall", );

                // Check if this is a static call or a private method call
                // If so, look up function by name
                let isfinal =
                    match &*expr.var {
                        ClassId(_) => true,
                        _ => false,
                    } || *expr.method.stat.borrow()
                        || match &*expr.method.vis.borrow() {
                        Pub | Prot => false,
                        Priv => true,
                    };

                let (func_val, mut arg_vals) = {
                    if isfinal {
                        match expr.var.gencode(generator)? { // Private method call
                            CodeValue::Value(val) => {
                                match generator.get_function_by_name(&expr.method.llvm_name()) {
                                    Some(func_val) => {
                                        // Assume type checks done
                                        let this = val.get_value(generator);
                                         (func_val, vec![this])
                                    }
                                    None => {
                                        return Err(EFunctionNotFound(expr.method.llvm_name()));
                                    }
                                }
                            }
                            Type(_, _) => { // Static call
                                println!("=====This is a static call!======");
                                println!("====looking for function {}======", expr.method.llvm_name());
                                match generator.get_function_by_name(&expr.method.llvm_name()) {
                                    Some(func_val) => {
                                        println!("====static method retreived!====");
                                        println!("type of {}", expr.method.llvm_name());
                                        unsafe {
                                            LLVMDumpType(LLVMTypeOf(func_val));
                                        }
                                        (func_val, vec![])
                                    }
                                    None => {
                                        println!("===static method not found====");
                                        return Err(EFunctionNotFound(expr.method.llvm_name()));
                                    }
                                }
                            }
                            _ => {
                                return Err(EValueNotFound(format!("{:?}", expr.var)));
                            }
                        }
                    }
                    else {
                        match expr.var.gencode(generator)? { // Pub/Prot function call
                            CodeValue::Value(val) => {
                                let cls = {
                                    let val_type = val.get_type();
                                    if let ClassTy(c) = &val_type.base {
                                        if val_type.array_lvl == 0 {
                                            c.clone()
                                        } else {
                                            return Err(EArrayNotSupportMethodCall(format!("{:?}", val_type)));
                                        }
                                    } else {
                                        return Err(ENonClassNotSupportMethodCall(format!("{:?}", val_type)));
                                    }
                                };

                                unsafe {
                                    let this_ref = val.get_value(generator);
                                    let virtual_table_global = generator.get_global_by_name(&cls.name).unwrap();
                                    let virtual_table_ty = LLVMTypeOf(virtual_table_global);
                                    let func_val = generator.get_function_by_name(&expr.method.llvm_name()).unwrap();
                                    let func_ty = LLVMTypeOf(func_val);
                                    let func_idx = match cls.vtable.borrow().indexof(&expr.method) {
                                        Some(ix) => ix,
                                        None => {
                                            return Err(EMethodNotFoundInVTable(format!("{:?}", expr.method)))
                                        }
                                    };
                                    let virtual_table_ptr_addr = LLVMBuildGEP(
                                        generator.builder,
                                        this_ref,
                                        generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_vtable_ptr_addr_{}", &cls.name))
                                    );
                                    let virtual_table_ptr = LLVMBuildLoad2(
                                        generator.builder,
                                        virtual_table_ty,
                                        virtual_table_ptr_addr,
                                        generator.next_name(&format!("load_vtable_ptr_{}", &cls.name))
                                    );
                                    let func_addr = LLVMBuildGEP(
                                        generator.builder,
                                        virtual_table_ptr,
                                        generator.indices(vec![0, func_idx as i64]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_func_addr_{}", &cls.name))
                                    );
                                    let func_val = LLVMBuildLoad2(
                                        generator.builder,
                                        func_ty,
                                        func_addr,
                                        generator.next_name(&format!("load_func_val_{}", &cls.name))
                                    );

                                    (func_val, vec![this_ref])
                                }
                            }
                            _ => {
                                return Err(EValueNotFound(format!("{:?}", expr.var)));
                            }
                        }
                    }
                };
                for (ix, arg) in expr.args.iter().enumerate() {
                    match arg.gencode(generator)? {
                        CodeValue::Value(arg_val) => {
                            // Type casting
                            let arg_llvm_val = arg_val.get_value(generator);
                            let casted_val = generator.cast_value(&arg_val.get_type(),  // source type
                                                                  &expr.method.args.borrow()[ix].1.as_ref(), // destination type
                                                                  arg_llvm_val // source value
                            );
                            arg_vals.push(casted_val);
                        }
                        _ => {
                            return Err(EValueNotFound(format!("{:?}", arg)));
                        }
                    }
                }

                println!("right before calling the method");
                unsafe {
                    let ret = Ok(CodeValue::Value(Val(
                        decaf::Type {
                            base: expr.method.return_ty.borrow().base.clone(),
                            array_lvl: expr.method.return_ty.borrow().array_lvl
                        },
                        LLVMBuildCall(
                            generator.builder,
                            func_val,
                            arg_vals.as_slice().as_ptr() as *mut _,
                            arg_vals.len() as u32,
                            if expr.method.return_ty.borrow().base == VoidTy {
                                name!("\0")
                            } else {
                                generator.next_name(&format!("function_call_{}", expr.method.llvm_name()))
                            }),
                        )
                    ));
                    println!("method called! I'm fine!");
                    ret
                }
			}
			SuperCall(expr) => {
                // Assuming type checks done
                let this_ref = generator.variable_lookup("this").unwrap();
                let func_val = generator.get_function_by_name(&expr.method.llvm_name()).unwrap();
                let mut arg_vals = vec![this_ref.addr.borrow().clone().unwrap()];
                for (ix, arg) in expr.args.iter().enumerate() {
                    match arg.gencode(generator)? {
                        CodeValue::Value(arg_val) => {
                            // Type casting
                            let arg_llvm_val = arg_val.get_value(generator);
                            let casted_val = generator.cast_value(&arg_val.get_type(),  // source type
                                                                  &expr.method.args.borrow()[ix].1.as_ref(), // destination type
                                                                  arg_llvm_val // source value
                            );
                            arg_vals.push(casted_val);
                        }
                        _ => {
                            return Err(EValueNotFound(format!("{:?}", arg)));
                        }
                    }
                }
                unsafe {
                    println!("expr.method.return_ty: {:?}", expr.method.return_ty.borrow());
                    Ok(CodeValue::Value(Val(
                        decaf::Type {
                            base: expr.method.return_ty.borrow().base.clone(),
                            array_lvl: expr.method.return_ty.borrow().array_lvl
                        },
                        LLVMBuildCall(
                            generator.builder,
                            func_val,
                            arg_vals.as_slice().as_ptr() as *mut _,
                            arg_vals.len() as u32,
                            if expr.method.return_ty.borrow().base == VoidTy {
                                name!("\0")
                            } else {
                                generator.next_name(&format!("function_call_{}", expr.method.llvm_name()))
                            },
                        )
                    )))
                }
			}
			ArrayAccess(expr) => {
                // TODO: bound check
                match (expr.var.gencode(generator)?, expr.idx.gencode(generator)?) {
                    (CodeValue::Value(array_val), CodeValue::Value(idx_val)) => {
                        let array_ty = array_val.get_type();
                        let array_llvm_val = array_val.get_value(generator);
                        let idx_llvm_val = idx_val.get_value(generator);
                        // Get pointer
                        unsafe {
                            let array_inner_addr = LLVMBuildGEP(
                                generator.builder,
                                array_llvm_val,
                                generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                2,
                                generator.next_name("gep_array_inner_addr")
                            );
                            let array_inner = LLVMBuildLoad(
                                generator.builder,
                                array_inner_addr,
                                generator.next_name("load_array_inner")
                            );
                            let val_addr = LLVMBuildGEP(
                                generator.builder,
                                array_inner,
                                params!(idx_llvm_val),
                                1,
                                generator.next_name("gep_array_element_addr")
                            );
                            let val = LLVMBuildLoad(
                                generator.builder,
                                val_addr,
                                generator.next_name("load_array_element")
                            );

                            Ok(CodeValue::Value(Val(
                                decaf::Type {
                                    base: array_ty.base.clone(),
                                    array_lvl: array_ty.array_lvl - 1
                                },
                                val
                            )))
                        }
                    }
                    (CodeValue::Value(_), _) => Err(EValueNotFound(format!("{:?}", expr.idx))),
                    (_, CodeValue::Value(_)) => Err(EValueNotFound(format!("{:?}", expr.var))),
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			FieldAccess(expr) => {
                // TODO: class check
                match expr.var.gencode(generator)? {
                    CodeValue::Value(var_val) => {
                        let obj_val = var_val.get_value(generator);
                        let obj_ty = var_val.get_type();
                        if obj_ty.array_lvl == 0 {
                            if let ClassTy(cls) = obj_ty.base {
                                let field_idx = cls.get_field_index(&expr.fld).unwrap();
                                let field_ty = generator.get_llvm_type_from_decaf_type(
                                    &*expr.fld.ty.borrow(), false, true).unwrap();
                                unsafe {
                                    let field_addr = LLVMBuildGEP(
                                        generator.builder,
                                        obj_val,
                                        generator.indices(vec![0, field_idx as i64]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("gep_field_addr_{}_{}", cls.name, expr.fld.name))
                                    );
                                    Ok(CodeValue::Value(Val(decaf::Type {
                                        base: expr.fld.ty.borrow().base.clone(),
                                        array_lvl: expr.fld.ty.borrow().array_lvl
                                    },
                                                            LLVMBuildLoad2(
                                                                generator.builder,
                                                                field_ty,
                                                                field_addr,
                                                                generator.next_name(&format!("load_field_{}_{}", cls.name, expr.fld.name))
                                                            )
                                    )))
                                }
                            } else {
                                Err(ENonClassNotSupportMethodCall(format!("{:?}", expr.var)))
                            }
                        } else {
                            Err(EArrayNotSupportMethodCall(format!("{:?}", expr.var)))
                        }
                    }
                    _ => Err(EValueNotFound(format!("{:?}", expr)))
                }
			}
			decaf::Expr::Variable(expr) => {
                Ok(CodeValue::Value(Addr(expr.var.clone())))
			}
			ClassId(expr) => {
                println!("I am in ClassId: {:?}", expr.cls.name);
                Ok(Type(
                    decaf::Type {
                        base: ClassTy(expr.cls.clone()),
                        array_lvl: 0
                    },
                    generator.get_llvm_type_by_name(&expr.cls.name, false).unwrap()
                ))
			}
			NULL => {
                unsafe {
                    Ok(CodeValue::Value(Val(
                        decaf::Type {
                            base: NULLTy,
                            array_lvl: 0
                        },
                        LLVMConstPointerNull(generator.int_type())
                    )))
                }
			}
        }
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
				format!("{}_{}_static_method_{}_{}_{}",
						self.cls.borrow().name,
						self.vis.borrow(),
                        self.name,
						self.args.borrow().iter()
						.map(|(_, ty)|{format!("{}", ty)})
						.collect::<Vec<String>>()
						.join("_"),
                        &*self.return_ty.borrow())
			}
			false => {
				format!("{}_{}_method_{}_{}_{}",
						self.cls.borrow().name,
						self.vis.borrow(),
                        self.name,
						self.args.borrow().iter()
						.map(|(_, ty)|{format!("{}", ty)})
						.collect::<Vec<String>>()
						.join("_"),
                        &*self.return_ty.borrow())
			}
		}
	}
}

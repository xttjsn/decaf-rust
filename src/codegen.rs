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
use TypeSpecification::*;

use crate::decaf;
use crate::decaf::{BlockStmt, Class, Expr::*, LiteralExpr::*, ControlFlow, Scope::*, Stmt::*, TypeBase::*, Variable, VariableTable, VTable, Value};
use crate::shell;
use crate::treebuild::Program;
use crate::codegen::GenState::{Second, Third};
use self::llvm_sys::analysis::LLVMVerifyModule;
use self::llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use self::llvm_sys::LLVMLinkage::LLVMExternalLinkage;
use std::cell::RefCell;
use crate::treebuild::SemanticError::EMissingReturn;

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

macro_rules! ptr_of {
    ($val:expr) => {
        LLVMPointerType($val, ADDRESS_SPACE_GENERIC)
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
                match &*val.addr.borrow() {
                    Some(addr) => {
                        unsafe {
                            LLVMBuildLoad(generator.builder,
                                          addr.clone(),
                                          generator.new_string_ptr(&format!("{}_ref_{}", val.name, val.next_refcount())))
                        }
                    }
                    None => {
                        panic!("variable \"{}\" address not set", val.name)
                    }
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
    EFunctionMissingReturn(String),
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
    Third,
}

#[derive(PartialEq)]
enum TypeSpecification {
    Solid,
    PtrIfClass,
    PtrAll
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
	mains: Vec<LLVMValueRef>,  // void static main functions
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
			mains: vec![],
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

    fn ptr_if_class_or_array(&self, ty: &decaf::Type) -> LLVMTypeRef {
        unsafe {
            match ty.base {
                ClassTy(_) => {
                    ptr_of!(self.get_llvm_type_from_decaf_type(ty))
                }
                _ => {
                    if ty.array_lvl > 0 {
                        ptr_of!(self.get_llvm_type_from_decaf_type(ty))
                    } else {
                        self.get_llvm_type_from_decaf_type(ty)
                    }
                }
            }
        }
    }

    fn get_llvm_type_from_decaf_type(&self, ty: &decaf::Type) -> LLVMTypeRef {
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
                self.get_llvm_type_by_name(&cls.name).unwrap()
            }
        };

        if ty.array_lvl == 0 {
            base_llvm_ty
        } else {
            let mut curr_ty= base_llvm_ty;
            unsafe {
                let mut lvl = ty.array_lvl;
                loop {
                    curr_ty = LLVMPointerType(base_llvm_ty, ADDRESS_SPACE_GENERIC);
                    let mut field_types = [curr_ty, self.int_type()];
                    curr_ty = LLVMStructType((&mut field_types[0]) as *mut _, 2, 0);
                    lvl -= 1;
                    if lvl == 0 {
                        break
                    }
                }
            }
            curr_ty
        }
    }

    fn get_llvm_type_by_name(&self, name: &str) -> Option<LLVMTypeRef> {
        match self.types_map.get(name.into()) {
            None => None,
            Some(t) => {
                Some(t.clone())
            }
        }
    }

    fn add_llvm_type(&mut self, name: &str, ty: LLVMTypeRef) -> Result<(), CompileError> {
        match self.get_llvm_type_by_name(name) {
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

    fn has_function(&self, name: &str) -> bool {
        self.get_function_by_name(name).is_some()
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

	fn add_main(&mut self, main_val: LLVMValueRef) {
		self.mains.push(main_val);
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
                    println!("getting function by name: {}", &func_scope.llvm_name());
					return self.get_function_by_name(&func_scope.llvm_name());
				}
				MethodScope(func_scope) => {
                    println!("getting function by name: {}", &func_scope.llvm_name());
					return self.get_function_by_name(&func_scope.llvm_name());
				}
				_ => continue,
			}
		}
		None
	}

    fn get_top_most_function_return_ty(&self) -> Option<decaf::Type> {
        use decaf::Scope::*;
        for scope in self.scopes.iter().rev() {
            match scope {
                CtorScope(func_scope) => {
                    return Some(decaf::Type {
                        base: ClassTy(func_scope.cls.clone()),
                        array_lvl: 0
                    });
                }
                MethodScope(func_scope) => {
                    return Some(func_scope.return_ty.borrow().as_ref().clone());
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

    fn get_top_most_class(&self) -> Option<Rc<Class>> {
        for scope in self.scopes.iter().rev() {
            match scope {
                ClassScope(cls) => {
                    return Some(cls.clone())
                }
                _ => continue,
            }
        }
        None



    }

	fn cast_value(&mut self, src_ty: &decaf::Type, dst_ty: &decaf::Type, val: LLVMValueRef) -> LLVMValueRef {
        println!("casting value");
		// Only cast pointer type
		if src_ty.is_compatible_with(dst_ty, false) {
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
                        unsafe {
                            let dst_llvm_ty = self.ptr_if_class_or_array(dst_ty);
                            let src_llvm_ty = self.ptr_if_class_or_array(src_ty);
                            println!("trying to cast src_ty: {} to dst_ty: {}", src_ty, dst_ty);
                            println!("src_llvm_ty: ");
                            LLVMDumpType(src_llvm_ty); println!(" ");
                            println!("src_llvm_ty address space: {}", LLVMGetPointerAddressSpace(src_llvm_ty));
                            println!("dst_llvm_ty: ");
                            LLVMDumpType(dst_llvm_ty); println!(" ");
                            println!("dst_llvm_ty address space: {}", LLVMGetPointerAddressSpace(dst_llvm_ty));
//                            let casted_addr = LLVMBuildAlloca(
//                                self.builder,
//                                dst_llvm_ty,
//                                self.next_name(&format!("cast_{}_{}", src_ty, dst_ty))
//                            );
//                            let casted_val = LLVMBuildBitCast(
//                                self.builder,
//                                val,
//                                dst_llvm_ty,
//                                self.next_name(&format!("cast_{}_{}", src_ty, dst_ty)));
//                            LLVMBuildStore(
//                                self.builder,
//                                casted_val,
//                                casted_addr
//                            );
//                            LLVMBuildLoad(
//                                self.builder,
//                                casted_addr,
//                                self.next_name(&format!("load_casted_value"))
//                            )
                            LLVMBuildBitCast(
                                self.builder,
                                val,
                                dst_llvm_ty,
                                self.next_name(&format!("cast_{}_{}", src_ty, dst_ty)))
                        }
					} else {
						val
					}
				} else {
					panic!("casting will change array_lvl: src_type: {}, dst_type: {}", src_ty, dst_ty);
				}
			} else {
                // Primitive type cast or equal type
                if src_ty.base == dst_ty.base {
                    val
                } else {
                    let dst_llvm_ty = self.ptr_if_class_or_array(dst_ty);
                    unsafe {
                        LLVMBuildIntCast(
                            self.builder,
                            val,
                            dst_llvm_ty,
                            self.next_name(&format!("int_cast_{}_{}", src_ty, dst_ty))
                        )
                    }
                }
			}
		} else {
			panic!("casting incompatible types. src_type: {}, dst_type: {}", src_ty, dst_ty);
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

    fn variable_add(&self, name: &str, var: Rc<Variable>) {
        use crate::decaf::Scope::*;
        // The top most scope must be a block scope
        match self.scopes.last() {
            Some(BlockScope(blk)) => {
                blk.vartbl.borrow_mut().push(var);
            }
            Some(CtorScope(ctor)) => {
                ctor.vartbl.borrow_mut().push(var);
            }
            Some(MethodScope(mthd)) => {
                mthd.vartbl.borrow_mut().push(var);
            }
            _ => {
                panic!("top most scope must be a block/ctor/method scope when adding variable");
            }
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
        println!("I am in new_string_class");
		// Create a String object and return the value of it
		unsafe {
            // Allocate memory
            let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String").unwrap(), self.next_name("malloc_string"));
			println!("====new_string_class({})", s);
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

                self.add_function("IO_pub_static_method_putChar_char_0_void_0", put_char_val).unwrap();
            }

            println!("gen putString");
            // putString
            {
                // Interface of Runtime
                let put_string_inner_ty = LLVMFunctionType(self.void_type(), params!(self.char_ptr_type()), 1, 0);
                let put_string_inner_val = LLVMAddFunction(self.module, IO_PUT_STRING_NAME, put_string_inner_ty);

                // Wrapper method
                let put_string_ty = LLVMFunctionType(self.void_type(), params!(ptr_of!(self.get_llvm_type_by_name("String").unwrap())), 1, 0);
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
                let get_line_ty = LLVMFunctionType(ptr_of!(self.get_llvm_type_by_name("String").unwrap()), null!(), 0, 0);
                let get_line_val = LLVMAddFunction(self.module, name!(b"IO_getLine\0"), get_line_ty);
                let get_line_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), get_line_val, name!(b"IO_getLine_main\0"));
                LLVMPositionBuilderAtEnd(self.builder, get_line_bb);
                let inner_line = LLVMBuildCall(self.builder, get_line_inner_val, null!(), 0, name!(b"inner_line\0"));

                // Construct a String class
                // Allocate memory
                let string_val = LLVMBuildMalloc(self.builder, self.get_llvm_type_by_name("String").unwrap(), name!(b"malloc_string\0"));
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

	pub fn link_object_file(&self,
							object_file_path: &str,
							executable_path: &str
	) -> Result<(), String> {
		// Link the object file
		let runtime_source = r#"
#include <stdio.h>
#include <stdlib.h>
#define MAX_STRING_SIZE 1000

struct String {
	char* inner;
};

struct Array {
	struct String* ptr;
	long long length;
};

void DECAF_MAIN();

void IO_putChar_inner(char c) {
	putchar(c);
	fflush(stdout);
}

void IO_putString_inner(char* s) {
	printf("%s", s);
	fflush(stdout);
}

void IO_putInt_inner(long long l) {
	printf("%lld", l);
	fflush(stdout);
}

char IO_getChar_inner() {
    char c = getc(stdin);
	return c;
}

char* IO_getLine_inner() {
	// TODO: set size
	char* line = malloc(MAX_STRING_SIZE);
	return fgets(&line, MAX_STRING_SIZE, stdin);
}

long long IO_getInt_inner() {
	long long l;
	scanf_s("%lld", &l);
	return l;
}

char IO_peek_inner() {
	char c = getc(stdin);
	ungetc(c, stdin);
	return c;
}

int main(int argc, char** argv) {
	struct String* argv_strings = malloc(sizeof(struct String) * argc);
	for (int i = 0; i < argc; i++) {
		argv_strings[i].inner = *(argv + i);
	}


	struct Array arr;
	arr.ptr = argv_strings;
	arr.length = argc;

    DECAF_MAIN(&arr);
}
"#;
		std::fs::write("./decaf_runtime.c", runtime_source).expect("Unable to write runtime");

		let clang_args = if let Some(ref target_triple) = self.target_triple {
			vec![
				"./decaf_runtime.c",
				object_file_path,
				"-target",
				&target_triple,
				"-o",
				&executable_path[..],
			]
		} else {
			vec!["./decaf_runtime.c", object_file_path, "-o", &executable_path[..], "-v"]
		};

		println!("running shell command: {:?}", clang_args);

		shell::run_shell_command("clang", &clang_args[..])
	}

	pub fn compile_to_module(&mut self, program: &Program) -> Result<LLVMModuleRef, String> {
		self.init_llvm();
		self.gen_builtin();

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

        self.state = Third;
        for cls in &program.classes {
            match cls.clone().gencode(self) {
                Ok(_) => {}
                Err(e) => {
                    return Err(format!("Compile error {:?}", e));
                }
            }
        }

		if self.mains.len() == 0 {
			println!("no main function is found");
			std::process::exit(1);
		}
		else if self.mains.len() > 1 {
			println!("more than one main function is found");
			std::process::exit(1);
		} else {
			// Implement decaf_main
			unsafe {
				let main_ty = LLVMFunctionType(self.void_type(),
                params!(self.ptr_if_class_or_array(&decaf::Type {
                    base: ClassTy(self.class_lookup("String").unwrap()),
                    array_lvl: 1
                })), 1, 0);
				let main_val = LLVMAddFunction(self.module, self.new_string_ptr("DECAF_MAIN"), main_ty);
				let main_bb = LLVMAppendBasicBlockInContext(LLVMGetModuleContext(self.module), main_val, name!(b"decaf_main_bb\0"));
				LLVMPositionBuilderAtEnd(self.builder, main_bb);
                let arg_val = LLVMGetFirstParam(main_val);
				LLVMBuildCall(self.builder, *self.mains.last().unwrap(), params!(arg_val), 1, name!(b"\0"));
                LLVMBuildRetVoid(self.builder);
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
                let vtable_ptr_ty;
                let vtable_global;
                unsafe {
                    vtable_ty = LLVMArrayType(generator.i64_ptr_type(), self.vtable.borrow().len() as u32);
                    vtable_global = LLVMAddGlobal(generator.module, vtable_ty,
                                                  generator.new_string_ptr(&vtable_name!(self.name)));
                    vtable_ptr_ty = LLVMPointerType(vtable_ty, ADDRESS_SPACE_GENERIC);
                }

                generator.add_global(&vtable_name!(self.name), vtable_global)?;

                // Define LLVM struct type for class
                let mut field_llvm_types: Vec<LLVMTypeRef> = vec![vtable_ptr_ty];
                let mut all_fields = vec![];
                all_fields.extend(self.sup_pub_fields.borrow().clone());
                all_fields.extend(self.sup_prot_fields.borrow().clone());
                all_fields.extend(self.sup_priv_fields.borrow().clone());
                all_fields.extend(self.pub_fields.borrow().clone());
                all_fields.extend(self.prot_fields.borrow().clone());
                all_fields.extend(self.priv_fields.borrow().clone());
                for field in all_fields.iter() {
                    match field.ty.borrow().base {
                        ClassTy(_) => {
                            field_llvm_types.push(generator.i64_ptr_type()); // Will be bit-casted to correct type
                        }
                        _ => {
                            field_llvm_types.push(generator.ptr_if_class_or_array(&*field.ty.borrow()));
                        }
                    }
                }

                let cls_llvm_type;
                unsafe {
                    cls_llvm_type = LLVMStructType(field_llvm_types.as_mut_ptr() as *mut _, field_llvm_types.len() as u32, 0);
                    generator.add_llvm_type(&self.name, cls_llvm_type.clone())?;
                }
            }
            GenState::Second => {

                // Define types for all methods/ctors/static methods
				let cls_llvm_type = generator.get_llvm_type_by_name(&self.name).unwrap();
                let mut vtable_vec: Vec<Option<LLVMValueRef>> = vec![None; self.vtable.borrow().len()];

                unsafe {
					let mut ctors : Vec<Rc<decaf::Ctor>> = vec![];
                    ctors.extend(self.pub_ctors.borrow().clone());
                    ctors.extend(self.prot_ctors.borrow().clone());
                    ctors.extend(self.priv_ctors.borrow().clone());
                    for ctor in ctors.iter() {
                        let ctor_ty;
                        let ctor_val;
                        let mut args = vec![ptr_of!(cls_llvm_type.clone())]; // The "this" reference
                        for (_, arg_ty) in ctor.args.borrow().iter() {
                            args.push(generator.ptr_if_class_or_array(arg_ty));
                        }
                        ctor_ty = LLVMFunctionType(ptr_of!(cls_llvm_type.clone()),
                                                   args.as_mut_ptr() as *mut _,
                                                   args.len() as u32,
                                                   0);
                        ctor_val = LLVMAddFunction(generator.module,
                                                   generator.new_string_ptr(&ctor.llvm_name()),
                                                   ctor_ty);
                        generator.add_function(&ctor.llvm_name(), ctor_val)?;
                        println!("Added ctor: {}\n Type:", ctor.llvm_name());
                        LLVMDumpType(ctor_ty); println!(" ");
                    }

                    for method in self.vtable.borrow().iter() {
                        let method_val = {
                            if !generator.has_function(&method.llvm_name()) {
                                let method_ty;
                                let method_val;
                                let possibly_super_cls_llvm_type = generator.get_llvm_type_by_name(&method.cls.borrow().name).unwrap();
                                let mut args = vec![ptr_of!(possibly_super_cls_llvm_type.clone())]; // The "this" reference
                                for (_, arg_ty) in method.args.borrow().iter() {
                                    args.push(generator.ptr_if_class_or_array(arg_ty));
                                }
                                println!("{} has {} args", method.llvm_name(), args.len());
                                let return_ty = generator.ptr_if_class_or_array(&*method.return_ty.borrow());
                                method_ty = LLVMFunctionType(return_ty,
                                                             args.as_mut_ptr() as *mut _,
                                                             args.len() as u32,
                                                             0);
                                method_val = LLVMAddFunction(generator.module,
                                                             generator.new_string_ptr(&method.llvm_name()),
                                                             method_ty);
                                generator.add_function(&method.llvm_name(), method_val)?;
                                println!("Added method: {}\n Type:", method.llvm_name());
                                LLVMDumpType(method_ty); println!(" ");
                                method_val
                            } else {
                                println!("Reuse method: {}\n Type: ", method.llvm_name());
                                generator.get_function_by_name(&method.llvm_name()).unwrap()
                            }
                        };

                        let idx = self.vtable.borrow().indexof(method).unwrap();
                        println!("The idx of {} in {} is {}", method.llvm_name(), self.name, idx);
                        vtable_vec[idx as usize] = Some(method_val);
                    }

                    for priv_method in self.priv_methods.borrow().iter() {
                        let method_ty;
                        let method_val;
                        let mut args = vec![ptr_of!(cls_llvm_type.clone())]; // The "this" reference
                        for (_, arg_ty) in priv_method.args.borrow().iter() {
                            args.push(generator.ptr_if_class_or_array(arg_ty));
                        }
                        let return_ty = generator.ptr_if_class_or_array(&*priv_method.return_ty.borrow());
                        method_ty = LLVMFunctionType(return_ty,
                                                     args.as_mut_ptr() as *mut _,
                                                     args.len() as u32,
                                                     0);
                        method_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&priv_method.llvm_name()),
                                                     method_ty);
                        generator.add_function(&priv_method.llvm_name(), method_val)?;
                        println!("Added method: {}\n Type:", priv_method.llvm_name());
                        LLVMDumpType(method_ty); println!(" ");
                    }

                    let mut all_statics: Vec<Rc<decaf::Method>> = vec![];
                    all_statics.extend(self.pub_static_methods.borrow().clone());
                    all_statics.extend(self.prot_static_methods.borrow().clone());
                    all_statics.extend(self.priv_static_methods.borrow().clone());

                    for staticm in all_statics.iter() {
                        let static_ty;
                        let static_val;
                        let mut args = vec![];
                        for (_, arg_ty) in staticm.args.borrow().iter() {
                            args.push(generator.ptr_if_class_or_array(arg_ty));
                        }
                        static_ty = LLVMFunctionType(generator.ptr_if_class_or_array(&*staticm.return_ty.borrow()),
                                                     args.as_mut_ptr() as *mut _,
                                                     staticm.args.borrow().len() as u32,
                                                     0);
                        static_val = LLVMAddFunction(generator.module,
                                                     generator.new_string_ptr(&staticm.llvm_name()),
                                                     static_ty);
                        generator.add_function(&staticm.llvm_name(), static_val)?;

                        // if the static method is public, has return type void and has "main" as its name, add it to mains
                        match *staticm.stat.borrow() {
                            true => {
                                match staticm.return_ty.borrow().base {
                                    VoidTy => {
                                        if staticm.is_main_method() {
                                            generator.add_main(static_val);
                                        } else {}
                                    }
                                    _ => {}
                                }
                            }
                            false => {}
                        }
                        println!("Added method: {}\n Type:", staticm.llvm_name());
                        LLVMDumpType(static_ty); println!(" ");
                    }
                }

                // Set vtable
                unsafe {
                    let vtable_value = LLVMConstArray(generator.i64_ptr_type(),
                                                      vtable_vec.iter().enumerate().map(|(ix, v)| {
                                                          LLVMConstBitCast(match v {
                                                              Some(f) => f.clone(),
                                                              None => {
                                                                  panic!("The {}-th function is not defined for {}", ix, self.name)
                                                              }
                                                          }, generator.i64_ptr_type())
                                                      })
                                                          .collect::<Vec<LLVMValueRef>>().as_mut_slice().as_mut_ptr() as *mut _,
                                                      vtable_vec.len() as u32);
                    println!("Added vtable for {}", self.name);
                    let vtable_global = generator.get_global_by_name(&vtable_name!(self.name)).unwrap();
                    LLVMSetInitializer(vtable_global, vtable_value);
                }
            }
            GenState::Third => {

                generator.scopes.push(ClassScope(self.clone()));

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

                for method in self.local_methods.borrow().iter() {
                    method.clone().gencode(generator)?;
                }

                generator.scopes.pop();
            }
        }

        Ok(OK)
	}
}

impl CodeGenRc for decaf::Ctor {
    fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Third);
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
                unsafe {
                    let this_val = LLVMGetFirstParam(ctor_llvm_val);
                    let this_addr = LLVMBuildAlloca(
                        generator.builder,
                        LLVMTypeOf(this_val),
                        generator.next_name(&format!("{}_this", name))
                    );
                    LLVMBuildStore(
                        generator.builder,
                        this_val,
                        this_addr
                    );
                    self.vartbl.borrow_mut().set_addr_for_name("this", this_addr);

                    for (ix, (arg_name, _)) in self.args.borrow().iter().enumerate() {
                        let arg_val = LLVMGetParam(ctor_llvm_val, ix as u32 + 1);
                        let arg_ty= LLVMTypeOf(arg_val);
                        // Alloca and store it (as a hack to not have PHI node)
                        let arg_addr = LLVMBuildAlloca(
                            generator.builder,
                            arg_ty,
                            generator.next_name(arg_name)
                        );
                        LLVMBuildStore(
                            generator.builder,
                            arg_val,
                            arg_addr,
                        );

                        self.vartbl.borrow_mut().set_addr_for_name(arg_name, arg_addr);
                    }
                }

				// Visit body
                let block = decaf::Stmt::Block(self.body.borrow().clone().unwrap());
				let ret = block.gencode(generator);

                if let None = block.returnable() {
                    // Generate return
                    println!("=======returning this=======");
                    unsafe {
                        let this_val =  LLVMGetFirstParam(ctor_llvm_val);
                        LLVMBuildRet(generator.builder, this_val);
                    }
                } else {
                    println!("=========block is returnable========");
                }

                generator.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGenRc for decaf::Method {
	fn gencode(self: Rc<Self>, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Third);
        println!("===================================");
        println!("gencode on {} in class\"{}\"", self.llvm_name(), generator.get_top_most_class().unwrap().name);
        println!("===================================");
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

                let cls = generator.get_top_most_class().unwrap();

                generator.scopes.push(MethodScope(self.clone()));

                println!("method \"{}\"'s llvm type is ", self.llvm_name());
                unsafe { LLVMDumpType(LLVMTypeOf(method_llvm_val)); }

				// Set argument variable address
                unsafe {
                    if *self.stat.borrow() {  // Static function
                        for (ix, (arg_name, _)) in self.args.borrow().iter().enumerate() {
                            let arg_val = LLVMGetParam(method_llvm_val, ix as u32 );
                            let arg_ty = LLVMTypeOf(arg_val);
                            println!("I am setting address for method variable \"{}\"", arg_name);
                            println!("The llvm type of \"{}\" is", arg_name);
                            LLVMDumpType(LLVMTypeOf(arg_val));

                            // Alloca and store it (as a hack to not have PHI node)
                            let arg_addr = LLVMBuildAlloca(
                                generator.builder,
                                arg_ty,
                                generator.next_name(arg_name)
                            );
                            LLVMBuildStore(
                                generator.builder,
                                arg_val,
                                arg_addr,
                            );

                            self.vartbl.borrow_mut().set_addr_for_name(arg_name, arg_addr);
                        }
                    } else {  // Non static function
                        let this_val = LLVMGetFirstParam(method_llvm_val);
                        let this_addr = LLVMBuildAlloca(
                            generator.builder,
                            LLVMTypeOf(this_val),
                            generator.next_name(&format!("{}_this", cls.name))
                        );
                        LLVMBuildStore(
                            generator.builder,
                            this_val,
                            this_addr
                        );
                        self.vartbl.borrow_mut().set_addr_for_name("this", this_addr);

                        println!("this_val's llvm type is ");
                        LLVMDumpType(LLVMTypeOf(this_val));

                        for (ix, (arg_name, _)) in self.args.borrow().iter().enumerate() {
                            let arg_val = LLVMGetParam(method_llvm_val, ix as u32 + 1);
                            let arg_ty = LLVMTypeOf(arg_val);
                            println!("I am setting address for method variable \"{}\"", arg_name);
                            println!("The llvm type of \"{}\" is", arg_name);
                            LLVMDumpType(LLVMTypeOf(arg_val));

                            // Alloca and store it (as a hack to not have PHI node)
                            let arg_addr = LLVMBuildAlloca(
                                generator.builder,
                                arg_ty,
                                generator.next_name(arg_name)
                            );
                            LLVMBuildStore(
                                generator.builder,
                                arg_val,
                                arg_addr,
                            );

                            self.vartbl.borrow_mut().set_addr_for_name(arg_name, arg_addr);
                        }
                    }
                }

				// Visit body
                let block = decaf::Stmt::Block(self.body.borrow().clone().unwrap());
                let ret = block.gencode(generator);

                match self.return_ty.borrow().base {
                    VoidTy => {
                        if let None = block.returnable() {
                            // Generate return
                            // The outer function must have return type of void, this is checked by semantic processing
                            unsafe {
                                LLVMBuildRetVoid(generator.builder);
                            }
                        } else {
                            println!("=========block is returnable========");
                        }
                    }
                    _ => {
                        if let None = block.returnable() {
                            return Err(EFunctionMissingReturn(format!("{}", self.name)));
                        } else {
                            println!("=========block is returnable========");
                        }
                    }
                }

                generator.scopes.pop();

                ret
			}
			None => Err(EFunctionNotFound(name))
		}
	}
}

impl CodeGen for decaf::Stmt {
	fn gencode(&self, generator: &mut CodeGenerator) -> GenCodeResult {
        assert!(generator.state == GenState::Third);
		println!("I am in gencode for stmt");
		match self {
            Declare(stmt) => {
                println!("I am in declare");
                let lhs_addr = {
                    // Allocate
                    unsafe {
                        LLVMBuildAlloca(generator.builder, generator.ptr_if_class_or_array(&stmt.ty),
                                        generator.new_string_ptr(&stmt.name))
                    }
                };

                let blk = generator.get_top_most_block().unwrap();
                blk.vartbl.borrow_mut().set_addr_for_name(&stmt.name, lhs_addr);

                match &stmt.init_expr {
                    Some(expr) => {
                        match expr.gencode(generator)? {
                            CodeValue::Value(val) => {
                                if stmt.ty.is_compatible_with(&val.get_type(), false) {
                                    println!("ready to store");
                                    unsafe {
                                        println!("type of val:");
                                        LLVMDumpType(LLVMTypeOf(val.get_value(generator)));
                                        println!("type of lhs_addr:");
                                        LLVMDumpType(LLVMTypeOf(lhs_addr));

                                        let llvm_val = val.get_value(generator);
                                        let casted_val = generator.cast_value(&expr.get_type(),
                                                                              &stmt.ty,
                                                                              llvm_val);

                                        LLVMBuildStore(generator.builder, casted_val, lhs_addr);
                                    }
                                    println!("store completed");

                                    Ok(OK)
                                } else {
                                    Ok(OK)
                                }
                            }
                            _ => Err(EValueNotFound(format!("{}", expr)))
                        }
                    }
                    None => {
                        if stmt.ty.array_lvl > 0 {
                            Err(EUninitializedArray(format!("{}", self)))
                        } else {
                            Ok(OK)
                        }
                    }
                }
            }
            If(stmt) => {
				println!("I am in If");
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

                                    LLVMPositionBuilderAtEnd(generator.builder, ifbb);

                                    (&*stmt.thenstmt).gencode(generator)?;

                                    if !(&*stmt.thenstmt).branchable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);
                                    Ok(OK)
                                }
                            }
                            _ => Err(EValueNotFound(format!("{}", stmt.cond)))
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{}", self)))
                }
            }
            IfElse(stmt) => {
				println!("I am in IfElse");
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
                                                    elsebb);

                                    LLVMPositionBuilderAtEnd(generator.builder, ifbb);

                                    (&*stmt.thenstmt).gencode(generator)?;

                                    if !(&*stmt.thenstmt).branchable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, elsebb);

                                    (&*stmt.elsestmt).gencode(generator)?;

                                    if !(&*stmt.elsestmt).branchable() {
                                        LLVMBuildBr(generator.builder, nextbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);

                                    Ok(OK)
                                }
                            }
                            _ => Err(EValueNotFound(format!("{}", stmt.cond)))
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{}", self)))
                }
            }
            Expr(stmt) => {
				println!("I am in Expr Stmt");
                stmt.expr.gencode(generator)?;
                Ok(OK)
            }
            While(stmt) => {
				println!("I am in While Stmt");
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

                                    Block(stmt.bodyblock.borrow().clone().unwrap()).gencode(generator)?;

                                    generator.scopes.pop();

                                    // NOTE: we might add Br no matter what
                                    if !Block(stmt.bodyblock.borrow().clone().unwrap()).branchable() {
                                        LLVMBuildBr(generator.builder, condbb);
                                    }

                                    LLVMPositionBuilderAtEnd(generator.builder, nextbb);
                                    Ok(OK)
                                }
                                _ => Err(EValueNotFound(format!("{}", stmt.condexpr)))
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{}", self)))
                }
            }
            Return(stmt) => {
				println!("I am in Return Stmt");
                match generator.get_top_most_function_return_ty() {
                    Some(func_return_ty) => {
                        unsafe {
                            if let Some(ret_expr) = &stmt.expr {
                                match ret_expr.gencode(generator)? {
                                    CodeValue::Value(ret_val) => {
                                        let llvm_val = ret_val.get_value(generator);
                                        let casted_val = generator.cast_value(&ret_val.get_type(),
                                                                              &func_return_ty,
                                                                              llvm_val);
                                        LLVMBuildRet(generator.builder, casted_val);
                                        Ok(OK)
                                    }
                                    _ => Err(EValueNotFound(format!("{}", ret_expr)))
                                }
                            } else {
                                LLVMBuildRetVoid(generator.builder);
                                Ok(OK)
                            }
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{}", self)))
                }
            }
            Continue(stmt) => {
				println!("I am in Continue Stmt");
                match stmt.lup.condbb.borrow().clone() {
                    Some(condbb) => {
                        unsafe {
                            LLVMBuildBr(generator.builder, condbb);
                            Ok(OK)
                        }
                    }
                    None => {
                        Err(ELoopLacksCondBB(format!("{}", self)))
                    }
                }
            }
            Break(stmt) => {
				println!("I am in Break Stmt");
                match stmt.lup.nextbb.borrow().clone() {
                    Some(nextbb) => {
                        unsafe {
                            LLVMBuildBr(generator.builder, nextbb);
                            Ok(OK)
                        }
                    }
                    None => {
                        Err(ELoopLacksCondBB(format!("{}", self)))
                    }
                }
            }
            Super(stmt) => {
				println!("I am in Super Stmt");
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        // This must be in a ctor, verified by the semantic checking process
                        let child_cls = generator.get_top_most_class().unwrap();
						let cls = stmt.sup.clone();
						let cls_name = cls.name.clone();
						let ctor = stmt.sup_ctor.clone();
                        let args = &stmt.args;
                        match generator.get_function_by_name(&ctor.llvm_name()) {
                            Some(ctor_func_val) => {

                                // Allocate first
                                let mut this;
                                unsafe {
                                    this = LLVMGetFirstParam(func_val);
                                    this = generator.cast_value(&(&child_cls).into(), &(&cls).into(), this);
                                }

                                let mut arg_vals = vec![this];
                                for (ix, arg) in args.iter().enumerate() {
                                    match arg.gencode(generator)? {
                                        CodeValue::Value(arg_val) => {
                                            // Type casting
                                            let arg_llvm_val = arg_val.get_value(generator);
                                            let casted_val = generator.cast_value(&arg_val.get_type(),  // source type
                                                                                  &ctor.args.borrow()[ix].1.as_ref(), // destination type
                                                                                  arg_llvm_val // source value
                                            );

                                            arg_vals.push(casted_val);
                                        }
                                        _ => {
                                            return Err(EValueNotFound(format!("{}", arg)));
                                        }
                                    }
                                }
                                unsafe {
                                    Ok(CodeValue::Value(Val(
                                        decaf::Type {
                                            base: ClassTy(cls.clone()),
                                            array_lvl: 0,
                                        },
                                        LLVMBuildCall(
                                            generator.builder,
                                            ctor_func_val,
                                            arg_vals.as_slice().as_ptr() as *mut _,
                                            arg_vals.len() as u32,
                                            generator.next_name(&format!("ctor_{}", &cls_name)),
                                        ))))
                                }
                            }
                            None => Err(EFunctionNotFound(ctor.llvm_name()))
                        }
                    }
                    None => Err(EBlockNotInFunction(format!("{}", self)))
                }
            }
            Block(stmt) => {
                println!("========Block=======");
                generator.scopes.push(BlockScope(stmt.clone()));
                for (ix, st) in stmt.stmts.borrow().iter().enumerate() {
                    println!("=======Stmt {}======", ix);
					println!("ready to gen code for stmt");
                    st.gencode(generator)?;
                    if let Some(_) = st.returnable() {
                        // No need to generate code for the rest
                        break
                    }
                }
                generator.scopes.pop();
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
                            CodeValue::Value(_) => Err(ENonAddressableValueOnLHS(format!("{}", expr.lhs))),
                            _ => Err(EValueNotFound(format!("{}", expr.lhs)))
                        }
                    }
                    _ => Err(EValueNotFound(format!("{}", expr.rhs)))
                }
			}
			BinArith(expr) => {
                match ((&*expr.lhs).gencode(generator)?, (&*expr.rhs).gencode(generator)?) {
                    (CodeValue::Value(val_lhs), CodeValue::Value(val_rhs)) => {
                        use crate::lnp::past::BinOp::*;
                        unsafe {
                            let lhs_llvm_val = val_lhs.get_value(generator);
                            let rhs_llvm_val = val_rhs.get_value(generator);
                            let lhs_casted = generator.cast_value(&val_lhs.get_type(),
                                                                  &(&IntTy).into(),
                                                                  lhs_llvm_val);
                            let rhs_casted = generator.cast_value(&val_rhs.get_type(),
                                                                  &(&IntTy).into(),
                                                                  rhs_llvm_val);
                            match expr.op {
                                PlusOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildAdd(generator.builder,
                                                 lhs_casted,
                                                 rhs_casted,
                                                 generator.next_name("add")))))
                                }
                                MinusOp => {
                                    let neg = LLVMBuildNeg(generator.builder,
                                                           rhs_casted,
                                                           generator.next_name("neg"));
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildAdd(generator.builder,
                                                 lhs_casted,
                                                 neg,
                                                 generator.next_name("add")))))
                                }
                                TimesOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildMul(generator.builder,
                                                 lhs_casted,
                                                 rhs_casted,
                                                 generator.next_name("mul")))))
                                }
                                DivideOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildSDiv(generator.builder,
                                                  lhs_casted,
                                                  rhs_casted,
                                                  generator.next_name("div")))))
                                }
                                ModOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildSRem(generator.builder,
                                                  lhs_casted,
                                                  rhs_casted,
                                                  generator.next_name("mod")))))
                                }
                                _ => Err(ENonArithOp(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{}", self)))
                }
			}
			UnArith(expr) => {
                use crate::lnp::past::UnOp::*;
                match (&*expr.rhs).gencode(generator)? {
                    CodeValue::Value(val_rhs) => {
                        unsafe {
                            let rhs_llvm_val = val_rhs.get_value(generator);
                            let rhs_casted = generator.cast_value(&val_rhs.get_type(),
                                                                  &(&IntTy).into(),
                                                                  rhs_llvm_val);
                            match expr.op {
                                PlusUOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), rhs_casted)))
                                }
                                MinusUOp => {
                                    Ok(CodeValue::Value(Val(val_rhs.get_type(), LLVMBuildNeg(
                                        generator.builder,
                                        rhs_casted,
                                        generator.next_name("neg")))))
                                }
                                _ => Err(EUnaryArithNotFound(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{}", self)))
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
                     _ => Err(EValueNotFound(format!("{}", self)))
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
                    _ => Err(EValueNotFound(format!("{}", self)))
                }
			}
			BinCmp(expr) => {
                match ((&*expr.lhs).gencode(generator)?, (&*expr.rhs).gencode(generator)?) {
                    (CodeValue::Value(val_lhs), CodeValue::Value(val_rhs)) => {
                        use crate::lnp::past::BinOp::*;
                        unsafe {
                            // Cast rhs to lhs type
                            let lhs_llvm_val = val_lhs.get_value(generator);
                            let rhs_llvm_val = val_rhs.get_value(generator);
                            let lhs_casted = lhs_llvm_val;
                            let rhs_casted = generator.cast_value(&val_rhs.get_type(),
                                                                  &val_lhs.get_type(),
                                                                  rhs_llvm_val);
                            match expr.op {
                                GreaterThanOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSGT,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("gt")))))
                                }
                                GreaterOrEqOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSGE,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("ge")))))
                                }
                                LessThanOp => {
									Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSLT,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("lt")))))
                                }
                                LessOrEqOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntSLE,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("le")))))
                                }
                                EqualsOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntEQ,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("eq")))))
                                }
                                NotEqualsOp => {
                                    Ok(CodeValue::Value(Val(
										decaf_bool_type!(),
										LLVMBuildICmp(generator.builder,
													  LLVMIntNE,
													  lhs_casted,
													  rhs_casted,
													  generator.next_name("ne")))))
                                }
                                _ => Err(ENonCmpOp(format!("{:?}", expr.op)))
                            }
                        }
                    }
                    _ => Err(EValueNotFound(format!("{}", self)))
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
                                        return Err(EValueNotFound(format!("{}", dim)));
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
                                        );
                                        let ty_inner = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl - 1,
                                            },
                                        );

                                        LLVMBuildBr(generator.builder, body_bb);
                                        LLVMPositionBuilderAtEnd(generator.builder, body_bb);
                                        let dim_llvm_val = dim_val.get_value(generator);
                                        let array_var = LLVMBuildMalloc(
                                            generator.builder,
                                            ty,
                                            generator.next_name("aloop_array_struct")
                                        );
                                        let array_inner_val = LLVMBuildArrayMalloc(
                                            generator.builder,
                                            ty_inner,
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
                                        );
                                        let inner_ty = generator.get_llvm_type_from_decaf_type(
                                            &decaf::Type {
                                                base: expr.ty.clone(),
                                                array_lvl: lvl - ix as u32 - 1,
                                            },
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
                                            ty,
                                            generator.next_name("aloop_array_struct")
                                        );
                                        let array_inner_val = LLVMBuildArrayMalloc(
                                            generator.builder,
                                            inner_ty,
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

                                LLVMBuildBr(
                                    generator.builder,
                                    loop_infos.first().unwrap().finish_bb
                                );
                                // Reposition
                                LLVMPositionBuilderAtEnd(
                                    generator.builder,
                                    loop_infos.first().unwrap().finish_bb
                                );
                            }

                            Ok(CodeValue::Value(Val(
                                decaf::Type {
                                    base: expr.ty.clone(),
                                    array_lvl: expr.dims.len() as u32
                                },
                                array_result)))
                        }
                        None => Err(EBlockNotInFunction(format!("{}", self)))
                    }
                } else {
                    Err(EArrayZeroDim(format!("{}", self)))
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
                // Avoid replacing "this" by returning the original one
                match generator.get_top_most_function_val() {
                    Some(func_val) => {
                        let cls = generator.get_top_most_class().unwrap();
                        unsafe {
                            println!("This LLVMType of self of \"this\" of {} is", cls.name);
                            LLVMDumpType(LLVMTypeOf(LLVMGetFirstParam(func_val)));
                            Ok(CodeValue::Value(Val((&cls).into(), LLVMGetFirstParam(func_val))))
                        }
                    }
                    None => Err(EThisNotFound)
                }
			}
			CreateObj(expr) => {
                use decaf::LiteralExpr::*;
                println!("I am in CreateObj of type \"{}\"", CreateObj(expr.clone()).get_type());
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
                                _ => Err(EInvalidStringCreation(format!("{}", self)))
                            }
                        } else {
                            Err(EInvalidStringCreation(format!("{}", self)))
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
                                        generator.get_llvm_type_by_name(&expr.cls.name).unwrap(),
                                        generator.next_name(&format!("new_{}", &expr.cls.name))
                                    );
                                    // Store virtual table pointer
                                    let virtual_table_val = generator.get_global_by_name(&vtable_name!(expr.cls.name)).unwrap();
                                    println!("CreateObject: LLVMBuildGEP");
                                    let virtual_table_addr = LLVMBuildGEP(
                                        generator.builder,
                                        this,
                                        generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_vtable_addr_{}", &expr.cls.name))
                                    );
                                    println!("CreateObject: LLVMBuildStore");
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
                                            return Err(EValueNotFound(format!("{}", arg)));
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

                println!("I am in methodCall");

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
                println!("is final: {}", isfinal);
                println!("method: {}", expr.method.llvm_name());
                let (func_val, mut arg_vals) = {
                    if isfinal {
                        match expr.var.gencode(generator)? {
                            CodeValue::Value(val) => { // Private method call
                                println!("I am in private method call");
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
                                return Err(EValueNotFound(format!("{}", expr.var)));
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
                                            return Err(EArrayNotSupportMethodCall(format!("{}", val_type)));
                                        }
                                    } else {
                                        return Err(ENonClassNotSupportMethodCall(format!("{}", val_type)));
                                    }
                                };

                                unsafe {
                                    let this_ref = val.get_value(generator);
                                    let virtual_table_global = generator.get_global_by_name(&vtable_name!(cls.name)).unwrap();
                                    let virtual_table_ty = LLVMTypeOf(virtual_table_global);
                                    let func_val = generator.get_function_by_name(&expr.method.llvm_name()).unwrap();
                                    let func_ty = LLVMTypeOf(func_val);
                                    let func_idx = match cls.vtable.borrow().indexof(&expr.method) {
                                        Some(ix) => ix,
                                        None => {
                                            return Err(EMethodNotFoundInVTable(format!("{}", expr.method)))
                                        }
                                    };

                                    println!("trying to gep vtable_ptr_addr");
                                    LLVMDumpType(LLVMTypeOf(this_ref));
                                    println!("MethodCall: LLVMBuildGEP vtable_ptr_addr");
                                    let virtual_table_ptr_addr = LLVMBuildGEP(
                                        generator.builder,
                                        this_ref,
                                        generator.indices(vec![0, 0]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_vtable_ptr_addr_{}", &cls.name))
                                    );
                                    println!("trying to load vtable_ptr");
                                    let virtual_table_ptr = LLVMBuildLoad(
                                        generator.builder,
                                        virtual_table_ptr_addr,
                                        generator.next_name(&format!("load_vtable_ptr_{}", &cls.name))
                                    );
                                    println!("trying to gep func_addr");
                                    println!("MethodCall: LLVMBuildGEP func_addr");
                                    let func_addr = LLVMBuildGEP(
                                        generator.builder,
                                        virtual_table_ptr,
                                        generator.indices(vec![0, func_idx as i64]).as_mut_slice().as_mut_ptr() as *mut _,
                                        2,
                                        generator.next_name(&format!("get_func_addr_{}", &cls.name))
                                    );
                                    println!("trying to load func_val");
                                    let func_val = LLVMBuildLoad(
                                        generator.builder,
                                        func_addr,
                                        generator.next_name(&format!("load_func_val_{}", &cls.name))
                                    );
                                    println!("trying to bit cast func_val");
                                    let func_val = LLVMBuildBitCast(
                                        generator.builder,
                                        func_val,
                                        func_ty,
                                        generator.next_name(&format!("bicast_func_val_{}", &cls.name))
                                    );
                                    println!("This is the llvm type of the function:");
                                    LLVMDumpType(LLVMTypeOf(func_val));

                                    (func_val, vec![this_ref])
                                }
                            }
                            _ => {
                                return Err(EValueNotFound(format!("{}", expr.var)));
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
                            return Err(EValueNotFound(format!("{}", arg)));
                        }
                    }
                }

                println!("right before calling the method: {}", expr.method);
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
                let this_ref = this_ref.addr.borrow().clone().unwrap();

                // Cast this
                let cls = generator.get_top_most_class().unwrap();
                let ty = (&cls).into();
                let sup = cls.sup.borrow().clone().unwrap();
                let sup_ty = (&sup).into();
                let this_ref = generator.cast_value(&ty, &sup_ty, this_ref);

                let func_val = generator.get_function_by_name(&expr.method.llvm_name()).unwrap();
                let mut arg_vals = vec![this_ref];
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
                            return Err(EValueNotFound(format!("{}", arg)));
                        }
                    }
                }
                unsafe {
                    println!("expr.method.return_ty: {}", expr.method.return_ty.borrow());
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
                        let elem_variable_name = format!("{}", self);
                        match generator.variable_lookup(&elem_variable_name) {
                            Some(var) => {
                                Ok(Value(Addr(var)))
                            }
                            None => {
                                let array_ty = array_val.get_type();
                                let array_llvm_val = array_val.get_value(generator);
                                let idx_llvm_val = idx_val.get_value(generator);
                                // Get pointer
                                unsafe {
                                    println!("Array Access: LLVMBuildGEP array_inner_addr");
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
                                    println!("Array Access: LLVMBuildGEP val_addr");
                                    let val_addr = LLVMBuildGEP(
                                        generator.builder,
                                        array_inner,
                                        params!(idx_llvm_val),
                                        1,
                                        generator.next_name("gep_array_element_addr")
                                    );

                                    // Make it a variable
                                    let array_elem_variable = Rc::new(Variable {
                                        name: format!("{}", self),
                                        ty: decaf::Type {
                                            base: array_ty.base.clone(),
                                            array_lvl: array_ty.array_lvl - 1
                                        },
                                        addr: RefCell::new(Some(val_addr)),
                                        refcount: RefCell::new(0)
                                    });
                                    generator.variable_add(&elem_variable_name,
                                                           array_elem_variable.clone());

                                    Ok(Value(Addr(array_elem_variable)))
                                }
                            }
                        }
                    }
                    (CodeValue::Value(_), _) => Err(EValueNotFound(format!("{}", expr.idx))),
                    (_, CodeValue::Value(_)) => Err(EValueNotFound(format!("{}", expr.var))),
                    _ => Err(EValueNotFound(format!("{}", self)))
                }
			}
			FieldAccess(expr) => {
                // TODO: class check
                match expr.var.gencode(generator)? {
                    CodeValue::Value(var_val) => {
                        let obj_val = var_val.get_value(generator);
                        let obj_ty = var_val.get_type();
                        if let ClassTy(cls) = obj_ty.base {
                            let (field_idx, field_ty) = {
                                if obj_ty.array_lvl == 0 {
                                    let field_idx = match cls.get_field_index(&expr.fld) {
                                        Some(ix) => ix,
                                        None => panic!("fail to find field index in {}", self),
                                    };
                                    let field_ty = generator.ptr_if_class_or_array(&*expr.fld.ty.borrow());
                                    (field_idx + 1, field_ty)  // Account for vtable ptr
                                } else {
                                    // Array type only have "length" field
                                    assert!(expr.fld.name == "length");
                                    (1, generator.int_type()) // Array does not have vtable ptr
                                }
                            };

                            unsafe {
                                println!("Field Access: LLVMBuildGEP in {}", self);
                                println!("field_idx: {}", field_idx);
                                println!("LLVMTypeOf(obj_val)=");
                                LLVMDumpType(LLVMTypeOf(obj_val));
                                let field_addr = LLVMBuildGEP(
                                    generator.builder,
                                    obj_val,
                                    generator.indices(vec![0, field_idx as i64]).as_mut_slice().as_mut_ptr() as *mut _,
                                    2,
                                    generator.next_name(&format!("gep_field_addr_{}_{}", cls.name, expr.fld.name))
                                );
                                let field_addr_ty = ptr_of!(field_ty);
                                let field_addr = LLVMBuildBitCast(
                                    generator.builder,
                                    field_addr,
                                    field_addr_ty,
                                    generator.next_name(&format!("bitcast_field_addr_{}_{}", cls.name, expr.fld.name))
                                );

                                let field_variable = Variable {
                                    name: expr.fld.name.clone(),
                                    ty: expr.fld.ty.borrow().as_ref().clone(),
                                    addr: RefCell::new(Some(field_addr)),
                                    refcount: RefCell::new(expr.fld.next_refcount()),
                                };

                                Ok(CodeValue::Value(Addr(Rc::new(field_variable))))
                            }
                        } else {
                            Err(ENonClassNotSupportMethodCall(format!("{}", expr.var)))
                        }
                    }
                    _ => Err(EValueNotFound(format!("{}", self)))
                }
			}
			decaf::Expr::Variable(expr) => {
                Ok(CodeValue::Value(Addr(expr.var.clone())))
			}
			ClassId(expr) => {
                println!("I am in ClassId: {}", expr.cls.name);
                Ok(Type(
                    decaf::Type {
                        base: ClassTy(expr.cls.clone()),
                        array_lvl: 0
                    },
                    generator.get_llvm_type_by_name(&expr.cls.name).unwrap()
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

pub trait LLVMName {
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

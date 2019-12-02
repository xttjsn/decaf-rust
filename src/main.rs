extern crate getopts;
extern crate matches;
extern crate decaflib;
extern crate tempfile;

use std::io::Read;
use getopts::{Matches, Options};
use std::env;
use std::fs::File;
use std::path::Path;

use decaflib::{treebuild, codegen};

use tempfile::NamedTempFile;

fn slurp(path: &str) -> Result<String, String> {
	let mut file = match File::open(path) {
		Ok(file) => file,
		Err(message) => {
			return Err(format!("File::open(): {:?}", message));
		}
	};

	let mut contents = String::new();

	match file.read_to_string(&mut contents) {
		Ok(_) => Ok(contents),
		Err(message) => Err(format!("read_to_string(): {:?}", message)),
	}
}


/// Convert "foo.decaf" to "foo".
fn executable_name(decaf_path: &str) -> String {
    let decaf_file_name = Path::new(decaf_path).file_name().unwrap().to_str().unwrap();

    let mut name_parts: Vec<_> = decaf_file_name.split('.').collect();
    let parts_len = name_parts.len();
    if parts_len > 1 {
        name_parts.pop();
    }

    name_parts.join(".")
}

#[test]
fn executable_name_decaf() {
    assert_eq!(executable_name("foo.decaf"), "foo");
}

#[test]
fn executable_name_relative_path() {
    assert_eq!(executable_name("bar/baz.decaf"), "baz");
}


fn convert_io_error<T>(result: Result<T, std::io::Error>) -> Result<T, String> {
    match result {
        Ok(value) => Ok(value),
        Err(e) => Err(format!("{}", e)),
    }
}

fn compile_file(matches: &Matches) -> Result<(), String> {
	let path = &matches.free[0];

	let src = match slurp(path) {
		Ok(src) => src,
		Err(e) => {
			return Err(format!("{}", e));
		}
	};

	let target_triple = matches.opt_str("target");

	let program = treebuild::parse_decaf(&src)?;

	let mut generator = codegen::new_generator(path, target_triple.clone());

	let llvm_module = generator.compile_to_module(&program)?;

	if matches.opt_present("dump-llvm") {
		let llvm_ir_cstr = codegen::llvm_module_to_cstring(llvm_module);
		let llvm_ir = String::from_utf8_lossy(llvm_ir_cstr.as_bytes());
		println!("{}", llvm_ir);
		return Ok(());
	}

	let llvm_opt_raw = matches
		.opt_str("llvm-opt")
		.unwrap_or_else(|| "3".to_owned());
	let mut llvm_opt = llvm_opt_raw.parse::<i64>().unwrap_or(3);
	if llvm_opt < 0 || llvm_opt > 3 {
		llvm_opt = 3;
	}

	generator.optimize(llvm_opt);
	let obj_file = convert_io_error(NamedTempFile::new())?;
	let obj_file_path = obj_file.path().to_str().expect("path not valid utf-8");
	generator.write_object_file(&obj_file_path)?;

	let output_name = executable_name(path);
	generator.link_object_file(
		&obj_file_path,
		&output_name,
	)?;

	Ok(())
}

fn print_usage(bin_name: &str, opts: Options) {
    let brief = format!("Usage: {} SOURCE_FILE [options]", bin_name);
    print!("{}", opts.usage(&brief));
}

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
 	let args: Vec<_> = env::args().collect();
	let mut opts = Options::new();

	opts.optflag("h", "help", "print usage");
	opts.optflag("v", "version", "print decafc version");
	opts.optflag("", "dump-llvm", "print LLVM IR generated");
	opts.optflag("", "dump-ast", "print decaf AST generated");

	opts.optopt("", "llvm-opt", "LLVM optimization level (0 to 3)", "LEVEL");

	let default_triple_cstring = codegen::get_default_target_triple();
	let default_triple = default_triple_cstring.to_str().unwrap();

	opts.optopt(
		"",
		"target",
		&format!("LLVM target triple (default: {})", default_triple),
		"TARGET",
	);

	let matches = match opts.parse(&args[1..]) {
		Ok(m) => m,
		Err(_) => {
			print_usage(&args[0], opts);
			std::process::exit(1);
		}
	};

	if matches.opt_present("h") {
		print_usage(&args[0], opts);
		return;
	}

	if matches.opt_present("v") {
        println!("decafc {}", VERSION);
        return;
    }

	if matches.free.len() != 1 {
		print_usage(&args[0], opts);
		std::process::exit(1);
	}

	match compile_file(&matches) {
		Ok(_) => {}
		Err(e) => {
			eprintln!("{}", e);
			std::process::exit(2);
		}
	}
}

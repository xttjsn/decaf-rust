#![feature(proc_macro_hygiene)]
extern crate plex;

use std::io::Read;

mod lexer {
    use plex::lexer;

    #[derive(Debug, Clone)]
    pub enum Token {
		/// Comments
		Whitespace,
        Comment,

		/// Keywords
		Break,
		Class,
		Continue,
		Else,
		Extends,
		If,
		New,
		Private,
		Protected,
		Public,
		Return,
		Static,
		Super,
		This,
		While,
		Forbidden,

		/// Primitives
		Boolean,
		Char,
		Int,
		Void,

		/// Literals
        IntLit(i64),
		CharLit(char),
		StrLit(String),
		BoolLit(bool),
		NullLit,

		/// Punctuations
		LParen,
		RParen,
		LBrace,
		RBrace,
		LBracket,
		RBracket,
		Semicolon,
		Comma,
		Dot,

		/// Operators
        Assign,
		GreaterThan,
		LessThan,
		Not,
  		Equals,
		GreaterOrEqual,
		LessOrEqual,
		NotEqual,
		Plus,
		Minus,
		Times,
		Divide,
		LogicalAnd,
		LogicalOr,
		Mod,


		/// Identifier
        Ident(String),
    }

    lexer! {
        fn next_token(text: 'a) -> Token;

        r#"[ \t\r\n]+"# => Token::Whitespace,
        r#"/[*](~(.*[*]/.*))[*]/"# => Token::Comment,
        r#"//[^\n]*"# => Token::Comment,

        r#"break"# => Token::Break,
		r#"Class"# => Token::Class,
		r#"continue"# => Token::Continue,
		r#"else"# => Token::Else,
		r#"extends"# => Token::Extends,
		r#"if"# => Token::If,
		r#"new"# => Token::New,
		r#"private"# => Token::Private,
		r#"protected"# => Token::Protected,
		r#"public"# => Token::Public,
		r#"return"# => Token::Return,
		r#"static"# => Token::Static,
		r#"super"# => Token::Super,
		r#"this"# => Token::This,
		r#"while"# => Token::While,
		r#"abstract|byte|case|catch|const|default|do|double|final|finally|for|implements|import|instanceof|interface|long|native|goto|package|short|switch|synchronized|throw|throws|transient|try|volatile"# => { panic!("forbidden keyword: {}", text) },
		r#"byvalue|cast|future|generic|inner|none|operator|outer|rest|var"# => {
			panic!("forbidden keyword: {}", text)
		},
		r#"byte|double|float|long|short"# => {
			panic!("forbidden keyword: {}", text)
		}

		r#"boolean"# => Token::Boolean,
		r#"char"# => Token::Char,
		r#"int"# => Token::Int,
		r#"void"# => Token::Void,

        r#"0|([1-9][0-9]*)"# => {
            if let Ok(i) = text.parse() {
                Token::IntLit(i)
            } else {
                panic!("integer {} is out of range", text)
            }
        }

		r#"\' \'"# => Token::CharLit(' '),
		r#"\'\\n"# => Token::CharLit('\n'),
		r#"\'\\t\'"# => Token::CharLit('\t'),
		r#"\'\\\'"# => Token::CharLit('\\'),
		r#"\'.\'"# => Token::CharLit(text.char_at(1)),
		r#"\'\\.\'"# => Token::CharLit(text.char_at(2)),
		r#"\".*\""# => Token::StrLit({
			let len = s.len();
			s[1..len]
		}),
		r#"true|false"# => Token::BoolLit(text.parse()),
		r#"null"# => Token::NullLit,


		r#"\("# => Token::LParen,
		r#"\)"# => Token::RParen,
		r#"\{"# => Token::LBrace,
		r#"\}"# => Token::RBrace,
		r#"\["# => Token::LBracket,
		r#"\]"# => Token::RBracket,
		r#";"# => Token::Semicolon,
		r#","# => Token::Comma,
		r#"\."# => Token::Dot,

		r#"="# => Token::Assign,
		r#">"# => Token::GreaterThan,
		r#"<"# => Token::LessThan,
		r#"!"# => Token::Not,
		r#"=="# => Token::Equals,
		r#">="# => Token::GreaterOrEqual,
		r#"<="# => Token::LessOrEqual,
		r#"!="# => Token::NotEqual,
		r#"\+"# => Token::Plus,
		r#"-"# => Token::Minus,
		r#"\*"# => Token::Times,
		r#"/"# => Token::Divide,
		r#"\&\&"# => Token::LogicalAnd,
		r#"\|\|"# => Token::LogicalOr,
		r#"%"# => Token::Mod,

        r#"[a-zA-Z_][a-zA-Z0-9_]*"# => Token::Ident(text.to_owned()),

        r#"."# => panic!("unexpected character: {}", text),
    }

    pub struct Lexer<'a> {
        original: &'a str,
        remaining: &'a str,
    }

    impl<'a> Lexer<'a> {
        pub fn new(s: &'a str) -> Lexer<'a> {
            Lexer {
                original: s,
                remaining: s,
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Span {
        pub lo: usize,
        pub hi: usize,
    }

    impl<'a> Iterator for Lexer<'a> {
        type Item = (Token, Span);
        fn next(&mut self) -> Option<(Token, Span)> {
            loop {
                let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                    let lo = self.original.len() - self.remaining.len();
                    let hi = self.original.len() - new_remaining.len();
                    self.remaining = new_remaining;
                    (tok, Span { lo, hi })
                } else {
                    return None;
                };
                match tok {
                    Token::Whitespace | Token::Comment => {
                        continue;
                    }
                    tok => {
                        return Some((tok, span));
                    }
                }
            }
        }
    }
}

mod ast {
    use lexer::Span;

    #[derive(Debug)]
    pub struct Program {
		pub classes: Vec<ClassNode>
    }

	#[derive(Debug)]
	pub struct ClassNode {
		pub span: Span,
		pub name: String,
		pub sup: Option<SuperNode>,
		pub member_list: Vec<Member>,
	}

	#[derive(Debug)]
	pub struct SuperNode {
		pub span: Span,
		pub name: String
	}

	#[derive(Debug)]
	pub enum Member {
		FieldMember(Field),
		MethodMember(Method),
		CtorMember(Ctor),
	}

	#[derive(Debug)]
	pub struct Field {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub ty: Type,
		pub var_decl: Vec<VarDeclarator>,
	}

	#[derive(Debug)]
	pub struct Method {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub ty: Type,
		pub name: String,
		pub fargs: FormalArgs,
		pub block: Block,
	}

	#[derive(Debug)]
	pub struct Ctor {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub name: String,
		pub fargs: FormalArgs,
		pub block: Block,
	}

	#[derive(Debug)]
	pub enum Modifier {
		ModStatic(ModNode),
		ModPublic(ModNode),
		ModPrivate(ModNode),
		ModProtected(ModNode),
	}

	#[derive(Debug)]
	pub struct ModNode {
		pub span: Span,
	}

	#[derive(Debug)]
	pub struct FormalArgs {
		pub span: Span,
		pub farg_list: Vec<FormalArg>,
	}

	#[derive(Debug)]
	pub struct FormalArgList {
		pub span: Span,
		pub fargs: Option<Vec<FormalArg>>
	}

	#[derive(Debug)]
	pub struct FormalArg {
		pub span: Span,
		pub ty: Type,
		pub var_decl_id: VarDeclaratorId,
	}

	#[derive(Debug)]
	pub enum Type {
		Primitive(PrimitiveType),
		Custom(String),
		Array(Box<Type>)
	}

	#[derive(Debug)]
	pub enum PrimitiveType {
		BoolType(PrimitiveTypeNode),
		CharType(PrimitiveTypeNode),
		IntType(PrimitiveTypeNode),
		VoidType(PrimitiveTypeNode),
	}

	#[derive(Debug)]
	pub struct PrimitiveTypeNode {
		pub span: Span,
	}

	#[derive(Debug)]
	pub struct VarDeclarator {
		pub span: Span,
		pub	vardeclid: VarDeclaratorId,
		pub expr: Option<Expression>
	}

	#[derive(Debug)]
	pub enum VarDeclaratorId {
		pub span: Span,
		pub id: VarDeclaratorIdInner,
	}

	#[derive(Debug)]
	pub enum VarDeclaratorInner {
		Single(String),
		Array(Box<VarDeclaratorId>)
	}

	#[derive(Debug)]
	pub struct Block {
		pub span: Span,
		pub stmts: Vec<Statement>
	}

	#[derive(Debug)]
	pub struct Statement {
		pub span: Span,
		pub stmt: Stmt,
	}

	#[derive(Debug)]
	pub enum Stmt {
		EmptyStmt,
		DeclareStmt(Type, VarDeclaratorList),
		IfStmt(Expression, Box<Statement>),
		IfElseStmt(Expression, Box<Statement>, Box<Statement>),
		ExprStmt(Expression),
		WhileStmt(Expression, Box<Statement>),
		ReturnStmt(Option<Expression>),
		ContinueStmt,
		BreakStmt,
		SuperStmt(ActuralArgs),
		BlockStmt(Block)
	}

	#[derive(Debug)]
	pub struct Expression {
		pub span: Span,
		pub expr: Expr,
	}

	#[derive(Debug)]
	pub enum Expr {
		BinaryExpr(Box<Expression>, BinaryOp, Box<Expression>),
		UnaryExpr(UnaryOp, Box<Expression>),
		PrimaryExpr(Primary)
	}

	#[derive(Debug)]
	pub struct BinaryOp {
		pub span: Span,
		pub op: BinOp,
	}

	#[derive(Debug)]
	pub enum BinOp {
		AssignOp,
		LogicalOrOp,
		LogicalAndOp,
		EqualsOp,
		NotEqualsOp,
		LessThanOp,
		GreaterThanOp,
		LessThanEqOp,
		GreaterThanEqOp,
		PlusOp,
		MinusOp,
		TimesOp,
		DivideOp,
		ModOp,
	}

	#[derive(Debug)]
	pub struct UnaryOp {
		pub span: Span,
		pub op: UnOp,
	}

	#[derive(Debug)]
	pub enum UnOp {
		PlusUOp,
		MinusUOp,
		NotUOp,
	}

	#[derive(Debug)]
	pub struct Primary {
		pub span: Span,
		pub prim: Prim,
	}

	#[derive(Debug)]
	pub enum Prim {
		NewArray(NewArrayExpr),
		NonNewArray(NonNewArrayExpr),
		Identifier(String),
	}

	#[deive(Debug)]
	pub struct NewArrayExpr {
		pub span: Span,
		pub expr: NAExpr,
	}

	#[derive(Debug)]
	pub enum NAExpr {
		NewCustom(String, Vec<Dimension>),
		NewPrimitive(PrimitiveType, Vec<Dimension>),
	}

	#[derive(Debug)]
	pub struct Dimension {
		pub span: Span,
		pub expr: Expression
	}

	#[derive(Debug)]
	pub struct NonNewArrayExpr {
		pub span: Span,
		pub expr: NNAExpr,
	}

	#[derive(Debug)]
	pub enum NNAExpr {
		JustLit(Literal),
		JustThis,
		JustExpr(Expression),
		NewObj(String, ActualArgs),
		CallFunc(String, ActualArgs),
		CallMethod(Primary, String, ActualArgs),
		CallSuper(String, ActuralArgs),
		EvalArray(ArrayExpr),
		EvalField(FieldExpr)
	}

	#[derive(Debug)]
	pub struct FieldExpr {
		pub span: Span,
		pub expr: FExpr,
	}

	#[derive(Debug)]
	pub enum FExpr {
		PrimaryFieldExpr(Primary, String),
		SuperFieldExpr(String),
	}

	#[derive(Debug)]
	pub struct ArrayExpr {
		pub span: Span,
		pub expr: AExpr,
	}

	#[derive(Debug)]
	pub enum AExpr {
		SimpleArraryExpr(String, Dimension),
		ComplexArrayExpr(Box<NonNewArrayExpr>, Dimension),
	}

	#[derive(Debug)]
	pub struct Literal {
		pub span: Span,
		pub litr: Litr
	}

	#[derive(Debug)]
	pub enum Litr {
		Null,
		Bool(BoolLit)
		Int(IntLit),
		Char(CharLit),
		Str(StrLit),
	}

	#[derive(Debug)]
	pub struct ActualArgs {
		pub span: Span,
		pub expr_lists: Option<ExprList>,
	}

	#[derive(Debug)]
	pub struct ExprList {
		pub span: Span,
		pub exprs: Vec<Expression>,
	}
}

mod parser {
    use ast::*;
    use lexer::Token::*;
    use lexer::*;
    use plex::parser;
    parser! {
        fn parse_(Token, Span);

        // combine two spans
        (a, b) {
            Span {
                lo: a.lo,
                hi: b.hi,
            }
        }

        program: Program {
			classes[cls] => Program { classes: cls }
        }

		classes: Vec<ClassNode> {
			=> vec![],
			classes[mut cls] class[c] => {
				cls.push(c);
				cls
			}
		}

		class: ClassNode {
			Class Ident(id) sueprnode LBrace members RBrace => ClassNode {
				span: span!(),
				name: id,
				sup: Some(supernode),
				member_list: members,
			},
			Class Ident(id) LBrace members RBrace => ClassNode {
				span: span!(),
				name: id,
				sup: None,
				member_list: members,
			}
		}

		supernode: SuperNode {
			Extends Ident(id) => SuperNode {
				span: span!(),
				name: id,
			}
		}

		members: Vec<Member> {
			=> vec![],
			members[mut ms] member[m] => {
				ms.push(m);
				ms
			}
		}

		member: Member {
			field[f] => Member::FieldMember(f),
			method[m] => Member::MethodMember(m),
			ctor[c] => Member::CtorMember(c)
		}

		field: Field {
			modifiers[ms] ty[t] vardecllist[vars] => Field {
				span: span!(),
				modies: ms,
				ty: t,
				var_decl: vars
			}
		}

		method: Method {
			modifiers[ms] ty[t] Ident(id) formalargs[args] block[b] => Method {
				span: span!(),
				modies: ms,
				ty: t,
				name: id,
				fargs: args,
				block: b
			}
		}

		ctor: Ctor {
			modifiers[ms] Ident(id) formalargs[args] block[b] => Ctor {
				span: span!(),
				modies: ms,
				name: id,
				fargs: args,
				block: b
			}
		}

		modifiers: Vec<Modifier> {
			=> vec![],
			modifiers[mut ms] modifier[m] => {
				ms.push(m);
				ms
			}
		}

		modifier: Modifier {
			Static => ModStatic(ModNode {
				span: span!()
			}),
			Public => ModPublic(ModNode {
				span: span!()
			}),
			Private => ModPrivate(ModNode {
				span: span!()
			}),
			Protected => ModProtected(ModNode {
				span: span!()
			})
		}

		formalargs: FormalArgs {
			LParen RParen => FormalArgs {
				span: span!(),
				frag_list: None,
			},
			LParen formalarglist(arglist) RParen => FormalArgs {
				span: span!(),
				frag_list: Some(arglist)
			},
		}

		formalarglist: Vec<FormalArg> {
			formalarg[arg] => vec![arg],
			formalarg[arg] Comma formalarglist[args] {
				// To maintain order
				let new_args = Vec::new();
				new_args.push(arg);
				new_args.extend(args);
				new_args
			}
		}

		formalarg: FormalArg {
			ty[t] vardeclid[vd] => FormalArg {
				span: span!(),
				ty: t,
				var_decl_id: vd,
			}
		}

		ty: Type {
			primty[pty] => Type::Primitive(pty),
			Indent[id] => Type::Custom(id),
			ty[t] LBracket RBracket => Type::Array(Box::new(t))
		}

		primty: PrimitiveType {
			Boolean => PrimitiveType::BoolType(PrimitiveTypeNode {
				span: span!(),
			}),
			Char => PrimitiveType::CharType(PrimitiveTypeNode {
				span: span!(),
			}),
			Int => PrimitiveType::IntType(PrimitiveTypeNode {
				span: span!(),
			}),
			Void => PrimitiveType::VoidType(PrimitiveTypeNode {
				span: span!(),
			}),
		}

		vardecllist: Vec<VarDeclarator> {
			vardecl[vdecl] => vec![vdecl],
			vardecl[vdecl] Comma vardecllist[vdecls] => {
				let new_vdecls = Vec::new();
				new_vdecls.push(vdecl);
				new_vdecls.extend(vdecls);
				new_vdecls
			}
		}

		vardecl: VarDeclarator {
			vardeclid[id] => VarDeclarator {
				span: span!(),
				vardeclid: id,
				expr: None,
			},
			vardeclid[id] Assign expr[e] => VarDeclarator {
				span: span!(),
				vardeclid: id,
				expr: e,
			}
		}

		vardeclid: VarDeclaratorId {
			Ident(id) => VarDeclaratorId {
				span: span!(),
				id: VarDeclaratorInner::Single(id),
			},
			vardeclid[id] LBracket RBracket => VarDeclaratorId {
				span: span!(),
				id: VarDeclaratorInner::Array(Box::new(id)),
			}
		}

		block: Block {
			LBrace stmts[s] RBrace => Block {
				span: span!(),
				stmts: s,
			}
		}

		stmts: Vec<Statement> {
			=> vec![],

		}


        statements: Vec<Expr> {
            => vec![],
            statements[mut st] assign[e] Semi => {
                st.push(e);
                st
            }
        }

        assign: Expr {
            Print assign[a] => Expr {
                span: span!(),
                node: Expr_::Print(Box::new(a)),
            },
            Ident(var) Equals assign[rhs] => Expr {
                span: span!(),
                node: Expr_::Assign(var, Box::new(rhs)),
            },
            term[t] => t,
        }

        term: Expr {
            term[lhs] Plus fact[rhs] => Expr {
                span: span!(),
                node: Expr_::Add(Box::new(lhs), Box::new(rhs)),
            },
            term[lhs] Minus fact[rhs] => Expr {
                span: span!(),
                node: Expr_::Sub(Box::new(lhs), Box::new(rhs)),
            },
            fact[x] => x
        }

        fact: Expr {
            fact[lhs] Star atom[rhs] => Expr {
                span: span!(),
                node: Expr_::Mul(Box::new(lhs), Box::new(rhs)),
            },
            fact[lhs] Slash atom[rhs] => Expr {
                span: span!(),
                node: Expr_::Div(Box::new(lhs), Box::new(rhs)),
            },
            atom[x] => x
        }

        atom: Expr {
            // round brackets to destructure tokens
            Ident(i) => Expr {
                span: span!(),
                node: Expr_::Var(i),
            },
            Integer(i) => Expr {
                span: span!(),
                node: Expr_::Literal(i),
            },
            LParen assign[a] RParen => a
        }
    }

    pub fn parse<I: Iterator<Item = (Token, Span)>>(
        i: I,
    ) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
        parse_(i)
    }
}

mod interp {
    use ast::*;
    use std::collections::HashMap;

    pub fn interp<'a>(p: &'a Program) {
        let mut env = HashMap::new();
        for expr in &p.stmts {
            interp_expr(&mut env, expr);
        }
    }
    fn interp_expr<'a>(env: &mut HashMap<&'a str, i64>, expr: &'a Expr) -> i64 {
        use ast::Expr_::*;
        match expr.node {
            Add(ref a, ref b) => interp_expr(env, a) + interp_expr(env, b),
            Sub(ref a, ref b) => interp_expr(env, a) - interp_expr(env, b),
            Mul(ref a, ref b) => interp_expr(env, a) * interp_expr(env, b),
            Div(ref a, ref b) => interp_expr(env, a) / interp_expr(env, b),
            Assign(ref var, ref b) => {
                let val = interp_expr(env, b);
                env.insert(var, val);
                val
            }
            Var(ref var) => *env.get(&var[..]).unwrap(),
            Literal(lit) => lit,
            Print(ref e) => {
                let val = interp_expr(env, e);
                println!("{}", val);
                val
            }
        }
    }
}

fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    let lexer = lexer::Lexer::new(&s).inspect(|tok| eprintln!("tok: {:?}", tok));
    let program = parser::parse(lexer).unwrap();
    interp::interp(&program);
}

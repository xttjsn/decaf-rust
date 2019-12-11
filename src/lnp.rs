extern crate plex;
pub mod lexer {

    use plex::lexer;
	use std::str::FromStr;
	use std::fmt;

	#[derive(Debug, PartialEq)]
	enum TokenError {
		EscapeAtEndOfString,
		InvalidEscapedChar(char),
	}

	struct InterpretEscapedString<'a> {
		s: std::str::Chars<'a>,
	}

	impl<'a> Iterator for InterpretEscapedString<'a> {
		type Item = Result<char, TokenError>;

		fn next(&mut self) -> Option<Self::Item> {
			self.s.next().map(|c| match c {
				'\\' => match self.s.next() {
					None => Err(TokenError::EscapeAtEndOfString),
					Some('n') => Ok('\n'),
					Some('\\') => Ok('\\'),
					Some('t') => Ok('\t'),
					Some(c) => Err(TokenError::InvalidEscapedChar(c)),
				}
				c => Ok(c)
			})
		}
	}

	fn interpret_escaped_string(s: &str) -> Result<String, TokenError> {
		(InterpretEscapedString {s : s.chars()}).collect()
	}

    #[derive(Debug, Clone)]
	#[allow(dead_code)]
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
        BracketPair,
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
		r#"class"# => Token::Class,
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
   // Check these for Literals  --Raj
		r#"\' \'"# => Token::CharLit(' '),
		r#"\'\\n"# => Token::CharLit('\n'),
		r#"\'\\t\'"# => Token::CharLit('\t'),
		r#"\'\\\'"# => Token::CharLit('\\'),
		r#"\'.\'"# => Token::CharLit({
			let mut it = text.chars();
			it.next();
			it.next().unwrap()
		}),
		r#"\'\\.\'"# => Token::CharLit( {
			let mut it = text.chars();
			it.next();
			it.next();
			it.next().unwrap()
		}),
		r#"\"[^\n]*\""# => Token::StrLit({
			let len = text.len();
			interpret_escaped_string(&text[1..len-1]).unwrap()
			// String::from_str(&text[1..len-1]).unwrap()
		}),
		r#"true|false"# => Token::BoolLit(text.parse().unwrap()),
		r#"null"# => Token::NullLit,


		r#"\("# => Token::LParen,
		r#"\)"# => Token::RParen,
		r#"\{"# => Token::LBrace,
		r#"\}"# => Token::RBrace,
        r#"\[[ \t]*\]"# => Token::BracketPair,
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
		#[allow(dead_code)]
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

	impl fmt::Display for Span {
		#[allow(unused_must_use)]
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "{},{}", self.lo, self.hi);
			Ok(())
		}
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

pub mod past {
    use super::lexer::{Span};

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

	#[derive(Debug, Clone)]
	pub struct SuperNode {
		pub span: Span,
		pub name: String
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Member {
		FieldMember(Field),
		MethodMember(Method),
		CtorMember(Ctor),
	}

	#[derive(Debug, Clone)]
	pub struct Field {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub ty: Type,
		pub var_decl: Vec<VarDeclarator>,
	}

	#[derive(Debug, Clone)]
	pub struct Method {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub ty: Type,
		pub name: String,
		pub fargs: FormalArgs,
		pub block: Block,
	}

	#[derive(Debug, Clone)]
	pub struct Ctor {
		pub span: Span,
		pub modies: Vec<Modifier>,
		pub name: String,
		pub fargs: FormalArgs,
		pub block: Block,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Modifier {
		ModStatic(ModNode),
		ModPublic(ModNode),
		ModPrivate(ModNode),
		ModProtected(ModNode),
	}

	#[derive(Debug, Clone)]
	pub struct ModNode {
		pub span: Span,
	}

	#[derive(Debug, Clone)]
	pub struct FormalArgs {
		pub span: Span,
		pub farg_list: Vec<FormalArg>,
	}

	#[derive(Debug, Clone)]
	pub struct FormalArgList {
		pub span: Span,
		pub fargs: Option<Vec<FormalArg>>
	}

	#[derive(Debug, Clone)]
	pub struct FormalArg {
		pub span: Span,
		pub ty: Type,
		pub var_decl_id: VarDeclaratorId,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Type {
		Primitive(PrimitiveType),
		Custom(String),
		Array(Box<Type>)
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum PrimitiveType {
		BoolType(PrimitiveTypeNode),
		CharType(PrimitiveTypeNode),
		IntType(PrimitiveTypeNode),
		VoidType(PrimitiveTypeNode),
	}

	#[derive(Debug, Clone)]
	pub struct PrimitiveTypeNode {
		pub span: Span,
	}

	#[derive(Debug, Clone)]
	pub struct VarDeclarator {
		pub span: Span,
		pub	vardeclid: VarDeclaratorId,
		pub expr: Option<Expression>
	}

	#[derive(Debug, Clone)]
	pub struct VarDeclaratorId {
		pub span: Span,
		pub id: VarDeclaratorIdInner,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum VarDeclaratorIdInner {
		Single(String),
		Array(Box<VarDeclaratorId>)
	}

	#[derive(Debug, Clone)]
	pub struct Block {
		pub span: Span,
		pub stmts: Vec<Statement>
	}

	#[derive(Debug, Clone)]
	pub struct Statement {
		pub span: Span,
		pub stmt: Stmt,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Stmt {
		EmptyStmt,
		DeclareStmt(Type, Vec<VarDeclarator>),
		IfStmt(Expression, Box<Statement>),
		IfElseStmt(Expression, Box<Statement>, Box<Statement>),
		ExprStmt(Expression),
		WhileStmt(Expression, Box<Statement>),
		ReturnStmt(Option<Expression>),
		ContinueStmt,
		BreakStmt,
		SuperStmt(ActualArgs),
		BlockStmt(Block)
	}

	#[derive(Debug, Clone)]
	pub struct Expression {
		pub span: Span,
		pub expr: Expr,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Expr {
		BinaryExpr(Box<Expression>, BinaryOp, Box<Expression>),
		UnaryExpr(UnaryOp, Box<Expression>),
		PrimaryExpr(Primary)
	}

	#[derive(Debug, Clone)]
	pub struct BinaryOp {
		pub span: Span,
		pub op: BinOp,
	}

	#[derive(Debug, PartialEq, Clone)]
	#[allow(dead_code)]
	pub enum BinOp {
		AssignOp,
		LogicalOrOp,
		LogicalAndOp,
		EqualsOp,
		NotEqualsOp,
		LessThanOp,
		GreaterThanOp,
		LessOrEqOp,
		GreaterOrEqOp,
		PlusOp,
		MinusOp,
		TimesOp,
		DivideOp,
		ModOp,
	}

	#[derive(Debug, Clone)]
	pub struct UnaryOp {
		pub span: Span,
		pub op: UnOp,
	}

	#[derive(Debug, PartialEq, Clone)]
	#[allow(dead_code)]
	pub enum UnOp {
		PlusUOp,
		MinusUOp,
		NotUOp,
	}

	#[derive(Debug, Clone)]
	pub struct Primary {
		pub span: Span,
		pub prim: Prim,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Prim {
		NewArray(NewArrayExpr),
		NonNewArray(Box<NonNewArrayExpr>),
		Identifier(String),
	}

	#[derive(Debug, Clone)]
	pub struct NewArrayExpr {
		pub span: Span,
		pub expr: NAExpr,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum NAExpr {
		NewCustom(String, Vec<Dimension>),
		NewPrimitive(PrimitiveType, Vec<Dimension>),
	}

	#[derive(Debug, Clone)]
	pub struct Dimension {
		pub span: Span,
		pub expr: Expression
	}

	#[derive(Debug, Clone)]
	pub struct NonNewArrayExpr {
		pub span: Span,
		pub expr: NNAExpr,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum NNAExpr {
		JustLit(Literal),
		JustThis,
		JustExpr(Expression),
		NewObj(String, ActualArgs),
		CallSelfMethod(String, ActualArgs),
		CallMethod(Primary, String, ActualArgs),
		CallSuper(String, ActualArgs),
		EvalArray(ArrayExpr),
		EvalField(FieldExpr)
	}

	#[derive(Debug, Clone)]
	pub struct FieldExpr {
		pub span: Span,
		pub expr: FExpr,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum FExpr {
		PrimaryFieldExpr(Primary, String),
		SuperFieldExpr(String),
	}

	#[derive(Debug, Clone)]
	pub struct ArrayExpr {
		pub span: Span,
		pub expr: AExpr,
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum AExpr {
		SimpleArraryExpr(String, Dimension),
		ComplexArrayExpr(Box<NonNewArrayExpr>, Dimension),
	}

	#[derive(Debug, Clone)]
	pub struct Literal {
		pub span: Span,
		pub litr: Litr
	}

	#[derive(Debug, Clone)]
	#[allow(dead_code)]
	pub enum Litr {
		Null,
		Bool(bool),
		Int(i64),
		Char(char),
		Str(String),
	}

	#[derive(Debug, Clone)]
	pub struct ActualArgs {
		pub span: Span,
		pub exprs: Vec<Expression>,
	}
}

pub mod parser {
    use super::past::*;
    use super::lexer::Token::*;
    use super::lexer::*;
    use plex::parser;

	macro_rules! binop {
		($bop:path) => {
			BinaryOp {
				span: span!(),
				op: $bop,
			}
		}
	}

	macro_rules! unop {
		($uop:path) => {
			UnaryOp {
				span: span!(),
				op: $uop,
			}
		}
	}

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
			Class Ident(id) supernode[s] LBrace members[ms] RBrace => ClassNode {
				span: span!(),
				name: id,
				sup: Some(s),
				member_list: ms,
			},
			Class Ident(id) LBrace members[ms] RBrace => ClassNode {
				span: span!(),
				name: id,
				sup: None,
				member_list: ms,
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
			modifiers[ms] ty[t] vardecllist[vars] Semicolon => Field {
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
			Static => Modifier::ModStatic(ModNode {
				span: span!()
			}),
			Public => Modifier::ModPublic(ModNode {
				span: span!()
			}),
			Private => Modifier::ModPrivate(ModNode {
				span: span!()
			}),
			Protected => Modifier::ModProtected(ModNode {
				span: span!()
			})
		}

		formalargs: FormalArgs {
			LParen RParen => FormalArgs {
				span: span!(),
				farg_list: vec![],
			},
			LParen formalarglist[args] RParen => FormalArgs {
				span: span!(),
				farg_list: args
			},
		}

		formalarglist: Vec<FormalArg> {
			formalarg[arg] => vec![arg],
			formalarg[arg] Comma formalarglist[args] => {
				// To maintain order
				let mut new_args = Vec::new();
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
			#[no_reduce(Dot, LBracket, LBrace, LParen)]
			Ident(id) => Type::Custom(id),
			ty[t] BracketPair => Type::Array(Box::new(t))
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
				let mut new_vdecls = Vec::new();
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
				expr: Some(e),
			}
		}

		vardeclid: VarDeclaratorId {
			Ident(id) => VarDeclaratorId {
				span: span!(),
				id: VarDeclaratorIdInner::Single(id),
			},
			vardeclid[id] BracketPair => VarDeclaratorId {
				span: span!(),
				id: VarDeclaratorIdInner::Array(Box::new(id)),
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
			stmts[mut st] stmt[s] => {
				st.push(s);
				st
			}
		}

		stmt: Statement {
			ty[t] vardecllist[vdecll] Semicolon => Statement {
				span: span!(),
				stmt: Stmt::DeclareStmt(t, vdecll),
			},
			#[no_reduce(Else)]
			If LParen expr[e] RParen stmt[s] => Statement {
				span: span!(),
				stmt: Stmt::IfStmt(e, Box::new(s)),
			},
			If LParen expr[e] RParen stmt[s0] Else stmt[s1] => Statement {
				span: span!(),
				stmt: Stmt::IfElseStmt(e, Box::new(s0), Box::new(s1)),
			},
			expr[e] Semicolon => Statement {
				span: span!(),
				stmt: Stmt::ExprStmt(e),
			},
			While LParen expr[e] RParen stmt[s] => Statement {
				span: span!(),
				stmt: Stmt::WhileStmt(e, Box::new(s)),
			},
			Return Semicolon => Statement {
				span: span!(),
				stmt: Stmt::ReturnStmt(None),
			},
			Return expr[e] Semicolon => Statement {
				span: span!(),
				stmt: Stmt::ReturnStmt(Some(e)),
			},
			Continue Semicolon => Statement {
				span: span!(),
				stmt: Stmt::ContinueStmt,
			},
			Break Semicolon => Statement {
				span: span!(),
				stmt: Stmt::BreakStmt,
			},
			Super actualargs[args] Semicolon => Statement {
				span: span!(),
				stmt: Stmt::SuperStmt(args),
			},
			block[b] => Statement {
				span: span!(),
				stmt: Stmt::BlockStmt(b),
			},
			Semicolon => Statement {
				span: span!(),
				stmt: Stmt::EmptyStmt,
			}
		}

		expr: Expression {
			assi_expr[ase] => ase,
		}

		assi_expr: Expression {
			#[no_reduce(Assign)]
			or_expr[oe] => oe,
			or_expr[oel] Assign or_expr[oer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(oel), binop!(BinOp::AssignOp), Box::new(oer)),
			},
			assi_expr[ase] Assign or_expr[oe] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ase), binop!(BinOp::AssignOp), Box::new(oe)),
			}
		}

		or_expr: Expression {
			#[no_reduce(LogicalOr)]
			and_expr[ae] => ae,
			and_expr[ael] LogicalOr and_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ael), binop!(BinOp::LogicalOrOp), Box::new(aer)),
			},
			or_expr[oe] LogicalOr and_expr[ae] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(oe), binop!(BinOp::LogicalOrOp), Box::new(ae)),
			}
		}

		and_expr: Expression {
			#[no_reduce(LogicalAnd)]
			eq_expr[ee] => ee,
			eq_expr[eel] LogicalAnd eq_expr[eer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(eel), binop!(BinOp::LogicalAndOp), Box::new(eer)),
			},
			and_expr[ae] LogicalAnd eq_expr[ee] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ae), binop!(BinOp::LogicalAndOp), Box::new(ee)),
			},
		}

		eq_expr: Expression {
			#[no_reduce(Equals, NotEqual)]
			cmp_expr[ce] => ce,
			cmp_expr[cel] Equals cmp_expr[cer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(cel), binop!(BinOp::EqualsOp), Box::new(cer)),
			},
			cmp_expr[cel] NotEqual cmp_expr[cer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(cel), binop!(BinOp::NotEqualsOp), Box::new(cer)),
			},
			eq_expr[ee] Equals cmp_expr[cer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ee), binop!(BinOp::EqualsOp), Box::new(cer)),
			},
			eq_expr[ee] NotEqual cmp_expr[cer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ee), binop!(BinOp::NotEqualsOp), Box::new(cer)),
			},
		}

		cmp_expr: Expression {
			#[no_reduce(LessThan, GreaterThan, LessOrEqual, GreaterOrEqual)]
			addi_expr[ae] => ae,
			addi_expr[ael] LessThan addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ael), binop!(BinOp::LessThanOp), Box::new(aer)),
			},
			addi_expr[ael] GreaterThan addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ael), binop!(BinOp::GreaterThanOp), Box::new(aer)),
			},
			addi_expr[ael] LessOrEqual addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ael), binop!(BinOp::LessOrEqOp), Box::new(aer)),
			},
			addi_expr[ael] GreaterOrEqual addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ael), binop!(BinOp::GreaterOrEqOp), Box::new(aer)),
			},
			cmp_expr[ce] LessThan addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ce), binop!(BinOp::LessThanOp), Box::new(aer)),
			},
			cmp_expr[ce] GreaterThan addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ce), binop!(BinOp::GreaterThanOp), Box::new(aer)),
			},
			cmp_expr[ce] LessOrEqual addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ce), binop!(BinOp::LessOrEqOp), Box::new(aer)),
			},
			cmp_expr[ce] GreaterOrEqual addi_expr[aer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ce), binop!(BinOp::GreaterOrEqOp), Box::new(aer)),
			},
		}

		addi_expr: Expression {
			#[no_reduce(Plus, Minus)]
			multi_expr[me] => me,
			multi_expr[mel] Plus multi_expr[mer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(mel), binop!(BinOp::PlusOp), Box::new(mer)),
			},
			multi_expr[mel] Minus multi_expr[mer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(mel), binop!(BinOp::MinusOp), Box::new(mer)),
			},
			addi_expr[ae] Plus multi_expr[mer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ae), binop!(BinOp::PlusOp), Box::new(mer)),
			},
			addi_expr[ae] Minus multi_expr[mer] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(ae), binop!(BinOp::MinusOp), Box::new(mer)),
			},
		}

		multi_expr: Expression {
			#[no_reduce(Times, Divide, Mod)]
			pref_expr[pe] => pe,
			pref_expr[pel] Times pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(pel), binop!(BinOp::TimesOp), Box::new(per)),
			},
			pref_expr[pel] Divide pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(pel), binop!(BinOp::DivideOp), Box::new(per)),
			},
			pref_expr[pel] Mod pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(pel), binop!(BinOp::ModOp), Box::new(per)),
			},
			multi_expr[me] Times pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(me), binop!(BinOp::TimesOp), Box::new(per)),
			},
			multi_expr[me] Divide pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(me), binop!(BinOp::DivideOp), Box::new(per)),
			},
			multi_expr[me] Mod pref_expr[per] => Expression {
				span: span!(),
				expr: Expr::BinaryExpr(Box::new(me), binop!(BinOp::ModOp), Box::new(per)),
			},
		}

		pref_expr: Expression {
			primary[p] => Expression {
				span: span!(),
				expr: Expr::PrimaryExpr(p),
			},
			Plus pref_expr[pe] => Expression {
				span: span!(),
				expr: Expr::UnaryExpr(unop!(UnOp::PlusUOp), Box::new(pe)),
			},
			Minus pref_expr[pe] => Expression {
				span: span!(),
				expr: Expr::UnaryExpr(unop!(UnOp::MinusUOp), Box::new(pe)),
			},
			Not pref_expr[pe] => Expression {
				span: span!(),
				expr: Expr::UnaryExpr(unop!(UnOp::NotUOp), Box::new(pe)),
			},
		}

		primary: Primary {
			newarrayexpr[e] => Primary {
				span: span!(),
				prim: Prim::NewArray(e),
			},
			nnaexpr[ne] => Primary {
				span: span!(),
				prim: Prim::NonNewArray(Box::new(ne)),
			},
			Ident(id) => Primary {
				span: span!(),
				prim: Prim::Identifier(id),
			},
		}

		primary_dot_ident: (Primary, String) {
			primary[p] Dot Ident(id) => (p, id),
		}

		ident_dot_ident: (String, String) {
			Ident(id0) Dot Ident(id1) => (id0, id1),
		}

		super_dot_ident: String {
			Super Dot Ident(id) => id,
		}

		newarrayexpr: NewArrayExpr {
			New Ident(id) dims[ds] => NewArrayExpr {
				span: span!(),
				expr: NAExpr::NewCustom(id, ds),
			},
			New primty[pty] dims[ds] => NewArrayExpr {
				span: span!(),
				expr: NAExpr::NewPrimitive(pty, ds),
			}
		}

		dims: Vec<Dimension> {
			dim[d] => vec![d],
			dims[mut ds] dim[d] => {
				ds.push(d);
				ds
			}
		}

        dim: Dimension{
            LBracket expr[e] RBracket => Dimension{
                span:span!(),
                expr: e,
            }

        }

        nnaexpr: NonNewArrayExpr{
            liter[lt] => NonNewArrayExpr {
                span: span!(),
                expr: NNAExpr::JustLit(lt),
            },

            This => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::JustThis,
            },

            LParen expr[e] RParen => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::JustExpr(e),
            },

            New Ident(id) actualargs[a] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::NewObj(id, a),
            },

            Ident(id) actualargs[a] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::CallSelfMethod(id, a),
            },

			primary_dot_ident[pdi] actualargs[a] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::CallMethod(pdi.0, pdi.1, a),
            },

            super_dot_ident[id] actualargs[a] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::CallSuper(id, a),
            },

            arrayexpr[a] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::EvalArray(a),
            },

            fieldexpr[f] => NonNewArrayExpr{
                span: span!(),
                expr: NNAExpr::EvalField(f)
            },

        }

        exprlist: Vec<Expression> {
            expr[e] => vec![e],
            exprlist[mut el] Comma expr[e] => {
                el.push(e);
                el
            }
        }

		fieldexpr: FieldExpr {
			primary_dot_ident[pdi] => FieldExpr {
				span: span!(),
				expr: FExpr::PrimaryFieldExpr(pdi.0, pdi.1),
			},

			super_dot_ident[id] => FieldExpr {
				span: span!(),
				expr: FExpr::SuperFieldExpr(id),
			},
		}

		arrayexpr: ArrayExpr {
			Ident(id) dim[d] => ArrayExpr {
				span: span!(),
				expr: AExpr::SimpleArraryExpr(id, d),
			},
			nnaexpr[e] dim[d] => ArrayExpr {
				span: span!(),
				expr: AExpr::ComplexArrayExpr(Box::new(e), d),
			},
		}

		liter: Literal {
			NullLit => Literal {
				span: span!(),
				litr: Litr::Null,
			},
			BoolLit(b) => Literal {
				span: span!(),
				litr: Litr::Bool(b),
			},
			IntLit(i) => Literal {
				span: span!(),
				litr: Litr::Int(i),
			},
			CharLit(c) => Literal {
				span: span!(),
				litr: Litr::Char(c),
			},
			StrLit(s) => Literal {
				span: span!(),
				litr: Litr::Str(s),
			}
		}

		actualargs: ActualArgs {
			LParen RParen => ActualArgs {
				span: span!(),
				exprs: vec![],
			},
			LParen exprlist[el] RParen => ActualArgs {
				span: span!(),
				exprs: el,
			}
		}

    }

	#[allow(dead_code)]
    pub fn parse<I: Iterator<Item = (Token, Span)>>(
        i: I,
    ) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
        parse_(i)
    }
}

#[allow(dead_code)]
pub fn parse_decaf<'a>(src: &'a str) -> past::Program {
	let lexer = lexer::Lexer::new(src).inspect(|tok| eprintln!("tok: {:?}", tok));
	parser::parse(lexer).unwrap()
}

use sexp::Atom::*;
use sexp::*;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::prelude::*;

const TRUE_VAL: i64 = 3;
const FALSE_VAL: i64 = 1;

#[derive(Debug, Clone)]
enum Expr {
    Num(i32),
    Bool(bool),
    Input,
    Var(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
}

#[derive(Debug, Clone)]
enum UnOp {
    Add1,
    Sub1,
    Negate,
    IsNum,
    IsBool,
}

#[derive(Debug, Clone)]
enum BinOp {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equal,
}

fn is_reserved(name: &str) -> bool {
    matches!(
        name,
        "let"
            | "add1"
            | "sub1"
            | "negate"
            | "true"
            | "false"
            | "if"
            | "block"
            | "loop"
            | "break"
            | "set!"
            | "input"
            | "isnum"
            | "isbool"
    )
}

fn parse_binding(b: &Sexp) -> (String, Expr) {
    match b {
        Sexp::List(pair) => match &pair[..] {
            [Sexp::Atom(S(name)), expr] => {
                if is_reserved(name) {
                    panic!("Invalid binding name: {}", name);
                }
                (name.to_string(), parse_expr(expr))
            }
            _ => panic!("Invalid binding form: {:?}", b),
        },
        _ => panic!("Invalid binding form: {:?}", b),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).expect("number out of range")),
        Sexp::Atom(S(name)) if name == "true" => Expr::Bool(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Bool(false),
        Sexp::Atom(S(name)) if name == "input" => Expr::Input,
        Sexp::Atom(S(name)) => {
            if is_reserved(name) {
                panic!("Invalid use of keyword: {}", name);
            }
            Expr::Var(name.to_string())
        }
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                if bindings.is_empty() {
                    panic!("let must have at least one binding");
                }
                let parsed: Vec<(String, Expr)> = bindings.iter().map(parse_binding).collect();
                Expr::Let(parsed, Box::new(parse_expr(body)))
            }
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(UnOp::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(UnOp::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "negate" => Expr::UnOp(UnOp::Negate, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(UnOp::IsNum, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(UnOp::IsBool, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(BinOp::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(BinOp::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(BinOp::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(BinOp::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(BinOp::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(BinOp::LessEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(BinOp::GreaterEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(BinOp::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), c, t, e] if op == "if" => Expr::If(
                Box::new(parse_expr(c)),
                Box::new(parse_expr(t)),
                Box::new(parse_expr(e)),
            ),
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("block needs at least one expression");
                }
                Expr::Block(exprs.iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if is_reserved(name) {
                    panic!("Invalid set! target: {}", name);
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            _ => panic!("Invalid expression: {:?}", s),
        },
        _ => panic!("Invalid expression: {:?}", s),
    }
}

fn new_label(counter: &mut i32, base: &str) -> String {
    let n = *counter;
    *counter += 1;
    format!("{}_{}", base, n)
}

fn check_num_rax() -> String {
    "mov rbx, rax\n  and rbx, 1\n  cmp rbx, 0\n  jne throw_error".to_string()
}

fn compile_expr(
    e: &Expr,
    env: &HashMap<String, i32>,
    stack_offset: i32,
    label_counter: &mut i32,
    break_target: Option<&str>,
) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", (*n as i64) << 1),
        Expr::Bool(true) => format!("mov rax, {}", TRUE_VAL),
        Expr::Bool(false) => format!("mov rax, {}", FALSE_VAL),
        Expr::Input => "mov rax, rdi".to_string(),
        Expr::Var(name) => match env.get(name) {
            Some(offset) => format!("mov rax, [rsp - {}]", offset),
            None => panic!("Unbound variable: {}", name),
        },
        Expr::Let(bindings, body) => {
            let mut seen = HashSet::new();
            for (name, _) in bindings {
                if !seen.insert(name.clone()) {
                    panic!("Duplicate binding: {}", name);
                }
            }

            let mut instrs = Vec::new();
            let mut new_env = env.clone();
            let mut current = stack_offset;
            for (name, expr) in bindings {
                instrs.push(compile_expr(expr, &new_env, current, label_counter, break_target));
                instrs.push(format!("mov [rsp - {}], rax", current));
                new_env.insert(name.clone(), current);
                current += 8;
            }
            instrs.push(compile_expr(body, &new_env, current, label_counter, break_target));
            instrs.join("\n  ")
        }
        Expr::UnOp(op, e1) => {
            let mut instrs = vec![compile_expr(e1, env, stack_offset, label_counter, break_target)];
            match op {
                UnOp::Add1 => {
                    instrs.push(check_num_rax());
                    instrs.push("add rax, 2".to_string());
                    instrs.push("jo throw_overflow".to_string());
                }
                UnOp::Sub1 => {
                    instrs.push(check_num_rax());
                    instrs.push("sub rax, 2".to_string());
                    instrs.push("jo throw_overflow".to_string());
                }
                UnOp::Negate => {
                    instrs.push(check_num_rax());
                    instrs.push("imul rax, -1".to_string());
                    instrs.push("jo throw_overflow".to_string());
                }
                UnOp::IsNum => {
                    instrs.push("mov rbx, rax".to_string());
                    instrs.push("and rbx, 1".to_string());
                    instrs.push("cmp rbx, 0".to_string());
                    instrs.push(format!("mov rax, {}", FALSE_VAL));
                    instrs.push(format!("mov rcx, {}", TRUE_VAL));
                    instrs.push("cmove rax, rcx".to_string());
                }
                UnOp::IsBool => {
                    instrs.push("mov rbx, rax".to_string());
                    instrs.push("and rbx, 1".to_string());
                    instrs.push("cmp rbx, 1".to_string());
                    instrs.push(format!("mov rax, {}", FALSE_VAL));
                    instrs.push(format!("mov rcx, {}", TRUE_VAL));
                    instrs.push("cmove rax, rcx".to_string());
                }
            }
            instrs.join("\n  ")
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = Vec::new();
            instrs.push(compile_expr(e1, env, stack_offset, label_counter, break_target));
            instrs.push(format!("mov [rsp - {}], rax", stack_offset));
            instrs.push(compile_expr(
                e2,
                env,
                stack_offset + 8,
                label_counter,
                break_target,
            ));

            match op {
                BinOp::Plus => {
                    instrs.push(check_num_rax());
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("mov rcx, rbx".to_string());
                    instrs.push("and rcx, 1".to_string());
                    instrs.push("cmp rcx, 0".to_string());
                    instrs.push("jne throw_error".to_string());
                    instrs.push("add rax, rbx".to_string());
                    instrs.push("jo throw_overflow".to_string());
                }
                BinOp::Minus => {
                    instrs.push(check_num_rax());
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("mov rcx, rbx".to_string());
                    instrs.push("and rcx, 1".to_string());
                    instrs.push("cmp rcx, 0".to_string());
                    instrs.push("jne throw_error".to_string());
                    instrs.push("sub rbx, rax".to_string());
                    instrs.push("jo throw_overflow".to_string());
                    instrs.push("mov rax, rbx".to_string());
                }
                BinOp::Times => {
                    instrs.push(check_num_rax());
                    instrs.push("sar rax, 1".to_string());
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("mov rcx, rbx".to_string());
                    instrs.push("and rcx, 1".to_string());
                    instrs.push("cmp rcx, 0".to_string());
                    instrs.push("jne throw_error".to_string());
                    instrs.push("imul rax, rbx".to_string());
                    instrs.push("jo throw_overflow".to_string());
                }
                BinOp::Less | BinOp::Greater | BinOp::LessEq | BinOp::GreaterEq => {
                    instrs.push(check_num_rax());
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("mov rcx, rbx".to_string());
                    instrs.push("and rcx, 1".to_string());
                    instrs.push("cmp rcx, 0".to_string());
                    instrs.push("jne throw_error".to_string());
                    instrs.push("cmp rbx, rax".to_string());
                    instrs.push(format!("mov rax, {}", FALSE_VAL));
                    instrs.push(format!("mov rcx, {}", TRUE_VAL));
                    match op {
                        BinOp::Less => instrs.push("cmovl rax, rcx".to_string()),
                        BinOp::Greater => instrs.push("cmovg rax, rcx".to_string()),
                        BinOp::LessEq => instrs.push("cmovle rax, rcx".to_string()),
                        BinOp::GreaterEq => instrs.push("cmovge rax, rcx".to_string()),
                        _ => unreachable!(),
                    }
                }
                BinOp::Equal => {
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("mov rcx, rbx".to_string());
                    instrs.push("xor rcx, rax".to_string());
                    instrs.push("and rcx, 1".to_string());
                    instrs.push("cmp rcx, 0".to_string());
                    instrs.push("jne throw_error".to_string());
                    instrs.push("cmp rbx, rax".to_string());
                    instrs.push(format!("mov rax, {}", FALSE_VAL));
                    instrs.push(format!("mov rcx, {}", TRUE_VAL));
                    instrs.push("cmove rax, rcx".to_string());
                }
            }

            instrs.join("\n  ")
        }
        Expr::If(cond, thn, els) => {
            let else_label = new_label(label_counter, "if_else");
            let end_label = new_label(label_counter, "if_end");
            let mut instrs = Vec::new();
            instrs.push(compile_expr(cond, env, stack_offset, label_counter, break_target));
            instrs.push(format!("cmp rax, {}", FALSE_VAL));
            instrs.push(format!("je {}", else_label));
            instrs.push(compile_expr(thn, env, stack_offset, label_counter, break_target));
            instrs.push(format!("jmp {}", end_label));
            instrs.push(format!("{}:", else_label));
            instrs.push(compile_expr(els, env, stack_offset, label_counter, break_target));
            instrs.push(format!("{}:", end_label));
            instrs.join("\n  ")
        }
        Expr::Block(exprs) => exprs
            .iter()
            .map(|ex| compile_expr(ex, env, stack_offset, label_counter, break_target))
            .collect::<Vec<_>>()
            .join("\n  "),
        Expr::Loop(body) => {
            let start = new_label(label_counter, "loop_start");
            let end = new_label(label_counter, "loop_end");
            let mut instrs = Vec::new();
            instrs.push(format!("{}:", start));
            instrs.push(compile_expr(
                body,
                env,
                stack_offset,
                label_counter,
                Some(&end),
            ));
            instrs.push(format!("jmp {}", start));
            instrs.push(format!("{}:", end));
            instrs.join("\n  ")
        }
        Expr::Break(e1) => {
            let target = break_target.expect("break used outside of loop");
            let mut instrs = Vec::new();
            instrs.push(compile_expr(e1, env, stack_offset, label_counter, break_target));
            instrs.push(format!("jmp {}", target));
            instrs.join("\n  ")
        }
        Expr::Set(name, e1) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable in set!: {}", name));
            let mut instrs = Vec::new();
            instrs.push(compile_expr(e1, env, stack_offset, label_counter, break_target));
            instrs.push(format!("mov [rsp - {}], rax", offset));
            instrs.join("\n  ")
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let mut in_file = File::open(&args[1])?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse(&in_contents).unwrap_or_else(|e| panic!("Parse error: {}", e));
    let expr = parse_expr(&sexp);

    let mut label_counter = 0;
    let instrs = compile_expr(&expr, &HashMap::new(), 8, &mut label_counter, None);

    let program = format!(
        "section .text\nextern snek_error\nglobal our_code_starts_here\nour_code_starts_here:\n  push rbp\n  mov rbp, rsp\n  sub rsp, 1024\n  {}\n  add rsp, 1024\n  pop rbp\n  ret\nthrow_error:\n  mov rdi, 1\n  sub rsp, 8\n  call snek_error\nthrow_overflow:\n  mov rdi, 2\n  sub rsp, 8\n  call snek_error\n",
        instrs
    );

    let mut out_file = File::create(&args[2])?;
    out_file.write_all(program.as_bytes())?;
    Ok(())
}

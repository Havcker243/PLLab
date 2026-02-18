use sexp::Atom::*;
use sexp::*;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Clone)]
enum Expr {
    Num(i32),
    Var(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
enum UnOp {
    Add1,
    Sub1,
    Negate,
}

#[derive(Debug, Clone)]
enum BinOp {
    Plus,
    Minus,
    Times,
}

fn is_reserved(name: &str) -> bool {
    matches!(name, "let" | "add1" | "sub1" | "negate")
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
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(BinOp::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(BinOp::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(BinOp::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            _ => panic!("Invalid expression: {:?}", s),
        },
        _ => panic!("Invalid expression: {:?}", s),
    }
}

fn compile_expr(e: &Expr, env: &HashMap<String, i32>, stack_offset: i32) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", n),
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
                instrs.push(compile_expr(expr, &new_env, current));
                instrs.push(format!("mov [rsp - {}], rax", current));
                new_env.insert(name.clone(), current);
                current += 8;
            }

            instrs.push(compile_expr(body, &new_env, current));
            instrs.join("\n  ")
        }
        Expr::UnOp(op, e1) => {
            let instr = compile_expr(e1, env, stack_offset);
            let op_instr = match op {
                UnOp::Add1 => "add rax, 1",
                UnOp::Sub1 => "sub rax, 1",
                UnOp::Negate => "imul rax, -1",
            };
            format!("{}\n  {}", instr, op_instr)
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = Vec::new();
            instrs.push(compile_expr(e1, env, stack_offset));
            instrs.push(format!("mov [rsp - {}], rax", stack_offset));
            instrs.push(compile_expr(e2, env, stack_offset + 8));

            match op {
                BinOp::Plus => instrs.push(format!("add rax, [rsp - {}]", stack_offset)),
                BinOp::Minus => {
                    instrs.push(format!("mov rbx, [rsp - {}]", stack_offset));
                    instrs.push("sub rbx, rax".to_string());
                    instrs.push("mov rax, rbx".to_string());
                }
                BinOp::Times => instrs.push(format!("imul rax, [rsp - {}]", stack_offset)),
            }

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

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse(&in_contents).unwrap_or_else(|e| panic!("Parse error: {}", e));
    let expr = parse_expr(&sexp);
    let instrs = compile_expr(&expr, &HashMap::new(), 8);

    let asm_program = format!(
        "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n  push rbp\n  mov rbp, rsp\n  sub rsp, 1024\n  {}\n  add rsp, 1024\n  pop rbp\n  ret\n",
        instrs
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    Ok(())
}

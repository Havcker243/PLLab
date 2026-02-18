// Assignment 1: Adder compiler implementation
// Compiles Adder expressions to x86-64 assembly.

use sexp::*;
use sexp::Atom::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

// AST for Adder expressions.
// Recursive variants are boxed to keep the enum sized.
#[derive(Debug)]
enum Expr {
    Num(i32),
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
    Negate(Box<Expr>),
}

/// Parse an S-expression into our Expr AST
/// 
/// This function converts S-expressions like (add1 5) into
/// our internal AST representation: Expr::Add1(Box::new(Expr::Num(5)))
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // Parse integer atoms and enforce i32 bounds.
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        
        // Parse unary operation forms: (add1 e), (sub1 e), (negate e).
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::Add1(Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::Sub1(Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "negate" => {
                    Expr::Negate(Box::new(parse_expr(e)))
                }
                _ => panic!("Invalid expression: {:?}", s),
            }
        }
        
        _ => panic!("Invalid expression: {:?}", s),
    }
}

/// Compile an Expr to x86-64 assembly instructions
/// 
/// The goal: generate assembly that evaluates the expression
/// and leaves the result in the rax register.
///
/// For example:
///   Num(5) should generate: "mov rax, 5"
///   Add1(Num(5)) should generate: "mov rax, 5\nadd rax, 1"
fn compile_expr(e: &Expr) -> String {
    match e {
        // Load immediate number into rax.
        Expr::Num(n) => format!("mov rax, {}", *n),
        
        // Compile subexpression, then increment rax.
        Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
        
        // Compile subexpression, then decrement rax.
        Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
        
        // Compile subexpression, then multiply by -1.
        Expr::Negate(subexpr) => compile_expr(subexpr) + "\nimul rax, -1",
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

    // Read input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse S-expression from text
    let sexp = parse(&in_contents).unwrap_or_else(|e| {
        panic!("Parse error: {}", e)
    });
    
    // Convert S-expression to our AST
    let expr = parse_expr(&sexp);
    
    // Generate assembly instructions
    let instrs = compile_expr(&expr);
    
    // Wrap instructions in assembly program template
    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        instrs
    );

    // Write output assembly file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

// ============= TESTS (Optional but recommended) =============
// 
// Uncomment and run with: cargo test
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn test_parse_number() {
//         let sexp = parse("42").unwrap();
//         let expr = parse_expr(&sexp);
//         // Add your assertions here
//     }
//
//     #[test]
//     fn test_compile_number() {
//         let expr = Expr::Num(42);
//         let asm = compile_expr(&expr);
//         assert_eq!(asm, "mov rax, 42");
//     }
// }

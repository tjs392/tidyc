use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn run_compiler(code: &str) -> (bool, String) {
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let path = format!("/tmp/test_lex_{}.c", id);
    
    std::fs::write(&path, code).unwrap();
    
    Command::new("cargo")
        .args(["build", "--quiet"])
        .status()
        .unwrap();
    
    let output = Command::new("./target/debug/cvm")
        .arg(&path)
        .output()
        .unwrap();
    
    let _ = std::fs::remove_file(&path);
    
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    (output.status.success(), stdout)
}

// ============ KEYWORDS ============

#[test]
fn test_keyword_int() {
    let (success, _) = run_compiler("int main(void) { return 0; }");
    assert!(success);
}

#[test]
fn test_keyword_char() {
    let (success, _) = run_compiler("void f(void) { char c; }");
    assert!(success);
}

#[test]
fn test_keyword_short() {
    let (success, _) = run_compiler("void f(void) { short s; }");
    assert!(success);
}

#[test]
fn test_keyword_long() {
    let (success, _) = run_compiler("void f(void) { long l; }");
    assert!(success);
}

#[test]
fn test_keyword_long_long() {
    let (success, _) = run_compiler("void f(void) { long long ll; }");
    assert!(success);
}

#[test]
fn test_keyword_float() {
    let (success, _) = run_compiler("void f(void) { float f; }");
    assert!(success);
}

#[test]
fn test_keyword_double() {
    let (success, _) = run_compiler("void f(void) { double d; }");
    assert!(success);
}

#[test]
fn test_keyword_void() {
    let (success, _) = run_compiler("void f(void) { }");
    assert!(success);
}

#[test]
fn test_keyword_signed() {
    let (success, _) = run_compiler("void f(void) { signed int x; }");
    assert!(success);
}

#[test]
fn test_keyword_unsigned() {
    let (success, _) = run_compiler("void f(void) { unsigned int x; }");
    assert!(success);
}

#[test]
fn test_keyword_const() {
    let (success, _) = run_compiler("void f(void) { const int x = 5; }");
    assert!(success);
}

#[test]
fn test_keyword_static() {
    let (success, _) = run_compiler("static int x;");
    assert!(success);
}

#[test]
fn test_keyword_extern() {
    let (success, _) = run_compiler("extern int x;");
    assert!(success);
}

#[test]
fn test_keyword_struct() {
    let (success, _) = run_compiler("struct S { int x; };");
    assert!(success);
}

#[test]
fn test_keyword_union() {
    let (success, _) = run_compiler("union U { int x; float f; };");
    assert!(success);
}

#[test]
fn test_keyword_enum() {
    let (success, _) = run_compiler("enum E { A, B, C };");
    assert!(success);
}

#[test]
fn test_keyword_typedef() {
    let (success, _) = run_compiler("typedef int MyInt;");
    assert!(success);
}

#[test]
fn test_keyword_if() {
    let (success, _) = run_compiler("void f(void) { if (1) { } }");
    assert!(success);
}

#[test]
fn test_keyword_else() {
    let (success, _) = run_compiler("void f(void) { if (1) { } else { } }");
    assert!(success);
}

#[test]
fn test_keyword_while() {
    let (success, _) = run_compiler("void f(void) { while (0) { } }");
    assert!(success);
}

#[test]
fn test_keyword_do() {
    let (success, _) = run_compiler("void f(void) { do { } while (0); }");
    assert!(success);
}

#[test]
fn test_keyword_for() {
    let (success, _) = run_compiler("void f(void) { for (;;) { break; } }");
    assert!(success);
}

#[test]
fn test_keyword_switch() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { } }");
    assert!(success);
}

#[test]
fn test_keyword_case() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { case 1: break; } }");
    assert!(success);
}

#[test]
fn test_keyword_default() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { default: break; } }");
    assert!(success);
}

#[test]
fn test_keyword_break() {
    let (success, _) = run_compiler("void f(void) { while (1) { break; } }");
    assert!(success);
}

#[test]
fn test_keyword_continue() {
    let (success, _) = run_compiler("void f(void) { while (1) { continue; } }");
    assert!(success);
}

#[test]
fn test_keyword_return() {
    let (success, _) = run_compiler("int f(void) { return 0; }");
    assert!(success);
}

#[test]
fn test_keyword_goto() {
    let (success, _) = run_compiler("void f(void) { goto end; end: return; }");
    assert!(success);
}

#[test]
fn test_keyword_sizeof() {
    let (success, _) = run_compiler("void f(void) { int x = sizeof(int); }");
    assert!(success);
}

// ============ LITERALS ============

#[test]
fn test_int_literal_decimal() {
    let (success, _) = run_compiler("void f(void) { int x = 42; }");
    assert!(success);
}

#[test]
fn test_int_literal_zero() {
    let (success, _) = run_compiler("void f(void) { int x = 0; }");
    assert!(success);
}

#[test]
fn test_int_literal_negative() {
    let (success, _) = run_compiler("void f(void) { int x = -123; }");
    assert!(success);
}

#[test]
fn test_int_literal_large() {
    let (success, _) = run_compiler("void f(void) { long x = 2147483647; }");
    assert!(success);
}

#[test]
fn test_float_literal_simple() {
    let (success, _) = run_compiler("void f(void) { float x = 3.14; }");
    assert!(success);
}

#[test]
fn test_float_literal_no_fraction() {
    let (success, _) = run_compiler("void f(void) { float x = 3.0; }");
    assert!(success);
}

#[test]
fn test_float_literal_small() {
    let (success, _) = run_compiler("void f(void) { float x = 0.001; }");
    assert!(success);
}

#[test]
fn test_char_literal_letter() {
    let (success, _) = run_compiler("void f(void) { char c = 'a'; }");
    assert!(success);
}

#[test]
fn test_char_literal_digit() {
    let (success, _) = run_compiler("void f(void) { char c = '5'; }");
    assert!(success);
}

#[test]
fn test_char_literal_space() {
    let (success, _) = run_compiler("void f(void) { char c = ' '; }");
    assert!(success);
}

#[test]
fn test_char_literal_escape_newline() {
    let (success, _) = run_compiler("void f(void) { char c = '\\n'; }");
    assert!(success);
}

#[test]
fn test_char_literal_escape_tab() {
    let (success, _) = run_compiler("void f(void) { char c = '\\t'; }");
    assert!(success);
}

#[test]
fn test_char_literal_escape_backslash() {
    let (success, _) = run_compiler("void f(void) { char c = '\\\\'; }");
    assert!(success);
}

#[test]
fn test_char_literal_escape_quote() {
    let (success, _) = run_compiler("void f(void) { char c = '\\''; }");
    assert!(success);
}

#[test]
fn test_string_literal_simple() {
    let (success, _) = run_compiler("void f(void) { char* s = \"hello\"; }");
    assert!(success);
}

#[test]
fn test_string_literal_empty() {
    let (success, _) = run_compiler("void f(void) { char* s = \"\"; }");
    assert!(success);
}

#[test]
fn test_string_literal_with_spaces() {
    let (success, _) = run_compiler("void f(void) { char* s = \"hello world\"; }");
    assert!(success);
}

#[test]
fn test_string_literal_escape_newline() {
    let (success, _) = run_compiler("void f(void) { char* s = \"hello\\nworld\"; }");
    assert!(success);
}

#[test]
fn test_string_literal_escape_tab() {
    let (success, _) = run_compiler("void f(void) { char* s = \"hello\\tworld\"; }");
    assert!(success);
}

#[test]
fn test_string_literal_escape_quote() {
    let (success, _) = run_compiler("void f(void) { char* s = \"say \\\"hi\\\"\"; }");
    assert!(success);
}

#[test]
fn test_null_literal() {
    let (success, _) = run_compiler("void f(void) { int* p = NULL; }");
    assert!(success);
}

// ============ OPERATORS ============

#[test]
fn test_op_plus() {
    let (success, _) = run_compiler("void f(void) { int x = 1 + 2; }");
    assert!(success);
}

#[test]
fn test_op_minus() {
    let (success, _) = run_compiler("void f(void) { int x = 5 - 3; }");
    assert!(success);
}

#[test]
fn test_op_multiply() {
    let (success, _) = run_compiler("void f(void) { int x = 2 * 3; }");
    assert!(success);
}

#[test]
fn test_op_divide() {
    let (success, _) = run_compiler("void f(void) { int x = 6 / 2; }");
    assert!(success);
}

#[test]
fn test_op_modulo() {
    let (success, _) = run_compiler("void f(void) { int x = 7 % 3; }");
    assert!(success);
}

#[test]
fn test_op_equal() {
    let (success, _) = run_compiler("void f(void) { int x = (1 == 1); }");
    assert!(success);
}

#[test]
fn test_op_not_equal() {
    let (success, _) = run_compiler("void f(void) { int x = (1 != 2); }");
    assert!(success);
}

#[test]
fn test_op_less_than() {
    let (success, _) = run_compiler("void f(void) { int x = (1 < 2); }");
    assert!(success);
}

#[test]
fn test_op_greater_than() {
    let (success, _) = run_compiler("void f(void) { int x = (2 > 1); }");
    assert!(success);
}

#[test]
fn test_op_less_equal() {
    let (success, _) = run_compiler("void f(void) { int x = (1 <= 2); }");
    assert!(success);
}

#[test]
fn test_op_greater_equal() {
    let (success, _) = run_compiler("void f(void) { int x = (2 >= 1); }");
    assert!(success);
}

#[test]
fn test_op_logical_and() {
    let (success, _) = run_compiler("void f(void) { int x = (1 && 1); }");
    assert!(success);
}

#[test]
fn test_op_logical_or() {
    let (success, _) = run_compiler("void f(void) { int x = (1 || 0); }");
    assert!(success);
}

#[test]
fn test_op_logical_not() {
    let (success, _) = run_compiler("void f(void) { int x = !0; }");
    assert!(success);
}

#[test]
fn test_op_bitwise_and() {
    let (success, _) = run_compiler("void f(void) { int x = 5 & 3; }");
    assert!(success);
}

#[test]
fn test_op_bitwise_or() {
    let (success, _) = run_compiler("void f(void) { int x = 5 | 3; }");
    assert!(success);
}

#[test]
fn test_op_bitwise_xor() {
    let (success, _) = run_compiler("void f(void) { int x = 5 ^ 3; }");
    assert!(success);
}

#[test]
fn test_op_bitwise_not() {
    let (success, _) = run_compiler("void f(void) { int x = ~5; }");
    assert!(success);
}

#[test]
fn test_op_left_shift() {
    let (success, _) = run_compiler("void f(void) { int x = 1 << 4; }");
    assert!(success);
}

#[test]
fn test_op_right_shift() {
    let (success, _) = run_compiler("void f(void) { int x = 16 >> 2; }");
    assert!(success);
}

#[test]
fn test_op_assign() {
    let (success, _) = run_compiler("void f(void) { int x; x = 5; }");
    assert!(success);
}

#[test]
fn test_op_plus_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x += 3; }");
    assert!(success);
}

#[test]
fn test_op_minus_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x -= 3; }");
    assert!(success);
}

#[test]
fn test_op_multiply_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x *= 3; }");
    assert!(success);
}

#[test]
fn test_op_divide_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 6; x /= 2; }");
    assert!(success);
}

#[test]
fn test_op_modulo_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 7; x %= 3; }");
    assert!(success);
}

#[test]
fn test_op_and_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 7; x &= 3; }");
    assert!(success);
}

#[test]
fn test_op_or_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x |= 3; }");
    assert!(success);
}

#[test]
fn test_op_xor_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x ^= 3; }");
    assert!(success);
}

#[test]
fn test_op_left_shift_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 1; x <<= 4; }");
    assert!(success);
}

#[test]
fn test_op_right_shift_assign() {
    let (success, _) = run_compiler("void f(void) { int x = 16; x >>= 2; }");
    assert!(success);
}

#[test]
fn test_op_pre_increment() {
    let (success, _) = run_compiler("void f(void) { int x = 5; ++x; }");
    assert!(success);
}

#[test]
fn test_op_pre_decrement() {
    let (success, _) = run_compiler("void f(void) { int x = 5; --x; }");
    assert!(success);
}

#[test]
fn test_op_post_increment() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x++; }");
    assert!(success);
}

#[test]
fn test_op_post_decrement() {
    let (success, _) = run_compiler("void f(void) { int x = 5; x--; }");
    assert!(success);
}

#[test]
fn test_op_address_of() {
    let (success, _) = run_compiler("void f(void) { int x; int* p = &x; }");
    assert!(success);
}

#[test]
fn test_op_dereference() {
    let (success, _) = run_compiler("void f(void) { int x; int* p = &x; *p = 5; }");
    assert!(success);
}

#[test]
fn test_op_ternary() {
    let (success, _) = run_compiler("void f(void) { int x = 1 ? 2 : 3; }");
    assert!(success);
}

#[test]
fn test_op_arrow() {
    let (success, _) = run_compiler("struct S { int x; }; void f(void) { struct S s; struct S* p = &s; p->x = 5; }");
    assert!(success);
}

#[test]
fn test_op_dot() {
    let (success, _) = run_compiler("struct S { int x; }; void f(void) { struct S s; s.x = 5; }");
    assert!(success);
}

// ============ IDENTIFIERS ============

#[test]
fn test_identifier_single_char() {
    let (success, _) = run_compiler("void f(void) { int x; }");
    assert!(success);
}

#[test]
fn test_identifier_multiple_chars() {
    let (success, _) = run_compiler("void f(void) { int myVariable; }");
    assert!(success);
}

#[test]
fn test_identifier_with_underscore() {
    let (success, _) = run_compiler("void f(void) { int my_variable; }");
    assert!(success);
}

#[test]
fn test_identifier_starts_with_underscore() {
    let (success, _) = run_compiler("void f(void) { int _private; }");
    assert!(success);
}

#[test]
fn test_identifier_with_numbers() {
    let (success, _) = run_compiler("void f(void) { int var123; }");
    assert!(success);
}

#[test]
fn test_identifier_mixed_case() {
    let (success, _) = run_compiler("void f(void) { int myVariableName; }");
    assert!(success);
}

#[test]
fn test_identifier_all_caps() {
    let (success, _) = run_compiler("void f(void) { int MY_CONSTANT; }");
    assert!(success);
}

// ============ PUNCTUATION ============

#[test]
fn test_punct_semicolon() {
    let (success, _) = run_compiler("void f(void) { int x; }");
    assert!(success);
}

#[test]
fn test_punct_comma() {
    let (success, _) = run_compiler("void f(int a, int b) { }");
    assert!(success);
}

#[test]
fn test_punct_braces() {
    let (success, _) = run_compiler("void f(void) { { { } } }");
    assert!(success);
}

#[test]
fn test_punct_brackets() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; arr[0] = 1; }");
    assert!(success);
}

#[test]
fn test_punct_parens() {
    let (success, _) = run_compiler("void f(void) { int x = ((1 + 2) * 3); }");
    assert!(success);
}

#[test]
fn test_punct_colon() {
    let (success, _) = run_compiler("void f(void) { label: return; }");
    assert!(success);
}
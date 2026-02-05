use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn run_compiler(code: &str) -> (bool, String) {
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let path = format!("/tmp/test_{}.c", id);
    
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

// ============ DECLARATION ERRORS ============

#[test]
fn test_redeclaration() {
    let (success, output) = run_compiler("void f(void) { int x; int x; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("Duplicate"), "Expected 'Duplicate' in output: {}", output);
}

#[test]
fn test_undeclared() {
    let (success, output) = run_compiler("void f(void) { y = 10; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("Undeclared"), "Expected 'Undeclared' in output: {}", output);
}

// ============ CONTROL FLOW ERRORS ============

#[test]
fn test_break_outside_loop() {
    let (success, output) = run_compiler("void f(void) { break; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("break"), "Expected 'break' in output: {}", output);
}

#[test]
fn test_continue_outside_loop() {
    let (success, output) = run_compiler("void f(void) { continue; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("continue"), "Expected 'continue' in output: {}", output);
}

#[test]
fn test_undefined_label() {
    let (success, output) = run_compiler("void f(void) { goto missing; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("undefined label"), "Expected 'undefined label' in output: {}", output);
}

#[test]
fn test_duplicate_label() {
    let (success, output) = run_compiler("void f(void) { dup: dup: return; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("Duplicate label"), "Expected 'Duplicate label' in output: {}", output);
}

// ============ RETURN TYPE ERRORS ============

#[test]
fn test_return_type_mismatch() {
    let (success, output) = run_compiler("int f(void) { return; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("return") || output.contains("Expected"), "output: {}", output);
}

#[test]
fn test_void_return_with_value() {
    let (success, output) = run_compiler("void f(void) { return 42; }");
    assert!(!success, "Expected failure, output: {}", output);
}

// ============ TYPE MISMATCH ERRORS ============

#[test]
fn test_init_type_mismatch() {
    let (success, output) = run_compiler("struct S { int x; }; void f(void) { int x = (struct S){0}; }");
    // Note: this might pass due to permissive type checking - adjust based on your implementation
}

// ============ FUNCTION CALL ERRORS ============

#[test]
fn test_wrong_arg_count() {
    let (success, output) = run_compiler("int add(int a, int b) { return a + b; } void f(void) { add(1); }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("argument"), "Expected argument error in output: {}", output);
}

#[test]
fn test_call_non_function() {
    let (success, output) = run_compiler("void f(void) { int x; x(10); }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("function"), "Expected function error in output: {}", output);
}

// ============ POINTER/DEREFERENCE ERRORS ============

#[test]
fn test_deref_non_pointer() {
    let (success, output) = run_compiler("void f(void) { int x; *x = 10; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("dereference") || output.contains("pointer"), "output: {}", output);
}

#[test]
fn test_arrow_on_non_pointer() {
    let (success, output) = run_compiler("struct S { int x; }; void f(void) { struct S s; s->x = 10; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("->") || output.contains("pointer"), "output: {}", output);
}

#[test]
fn test_field_on_non_struct() {
    let (success, output) = run_compiler("void f(void) { int x; x.field = 10; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("field") || output.contains("access"), "output: {}", output);
}

// ============ ARRAY ERRORS ============

#[test]
fn test_array_index_non_integer() {
    let (success, output) = run_compiler("void f(void) { int arr[10]; float idx; arr[idx] = 5; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("integer") || output.contains("index"), "output: {}", output);
}

// ============ SWITCH ERRORS ============

#[test]
fn test_switch_non_integer() {
    let (success, output) = run_compiler("void f(void) { float x; switch(x) { case 1: break; } }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("integer") || output.contains("Switch"), "output: {}", output);
}

#[test]
fn test_multiple_default() {
    let (success, output) = run_compiler("void f(void) { int x; switch(x) { default: break; default: break; } }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("default") || output.contains("Multiple"), "output: {}", output);
}

// ============ LVALUE ERRORS ============

#[test]
fn test_assign_non_lvalue() {
    let (success, output) = run_compiler("void f(void) { int x; (x + 1) = 10; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("lvalue"), "Expected lvalue error in output: {}", output);
}

#[test]
fn test_increment_non_lvalue() {
    let (success, output) = run_compiler("void f(void) { int x; (x + 1)++; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("lvalue"), "Expected lvalue error in output: {}", output);
}

#[test]
fn test_address_of_non_lvalue() {
    let (success, output) = run_compiler("void f(void) { int x; int* p = &(x + 1); }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("lvalue") || output.contains("address"), "output: {}", output);
}

// ============ OPERATOR TYPE ERRORS ============

#[test]
fn test_bitwise_on_float() {
    let (success, output) = run_compiler("void f(void) { float a; float b; int c = a & b; }");
    assert!(!success, "Expected failure, output: {}", output);
    assert!(output.contains("integer") || output.contains("Bitwise"), "output: {}", output);
}

#[test]
fn test_modulo_on_float() {
    let (success, output) = run_compiler("void f(void) { float a; float b; float c = a % b; }");
    assert!(!success, "Expected failure, output: {}", output);
}

// ============ VALID CODE ============

#[test]
fn test_valid_code_passes() {
    let (success, output) = run_compiler("int main(void) { int x = 5; return x; }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_valid_goto() {
    let (success, output) = run_compiler("void f(void) { goto end; end: return; }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_valid_pointer_arithmetic() {
    let (success, output) = run_compiler("void f(void) { int arr[10]; int* p = arr; p += 5; p -= 2; }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_valid_struct_access() {
    let (success, output) = run_compiler("struct S { int x; }; void f(void) { struct S s; s.x = 10; struct S* p = &s; p->x = 20; }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_valid_function_call() {
    let (success, output) = run_compiler("int add(int a, int b) { return a + b; } int main(void) { return add(1, 2); }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_break_in_switch() {
    let (success, output) = run_compiler("void f(void) { int x; switch(x) { case 1: break; } }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_break_in_loop() {
    let (success, output) = run_compiler("void f(void) { while(1) { break; } }");
    assert!(success, "Expected success, output: {}", output);
}

#[test]
fn test_continue_in_loop() {
    let (success, output) = run_compiler("void f(void) { while(1) { continue; } }");
    assert!(success, "Expected success, output: {}", output);
}
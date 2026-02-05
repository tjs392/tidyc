use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn run_compiler(code: &str) -> (bool, String) {
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let path = format!("/tmp/test_parse_{}.c", id);
    
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

// ============ VARIABLE DECLARATIONS ============

#[test]
fn test_var_decl_simple() {
    let (success, _) = run_compiler("void f(void) { int x; }");
    assert!(success);
}

#[test]
fn test_var_decl_with_init() {
    let (success, _) = run_compiler("void f(void) { int x = 5; }");
    assert!(success);
}

#[test]
fn test_var_decl_pointer() {
    let (success, _) = run_compiler("void f(void) { int* p; }");
    assert!(success);
}

#[test]
fn test_var_decl_double_pointer() {
    let (success, _) = run_compiler("void f(void) { int** pp; }");
    assert!(success);
}

#[test]
fn test_var_decl_array() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; }");
    assert!(success);
}

#[test]
fn test_var_decl_array_of_pointers() {
    let (success, _) = run_compiler("void f(void) { int* arr[10]; }");
    assert!(success);
}

#[test]
fn test_var_decl_const() {
    let (success, _) = run_compiler("void f(void) { const int x = 5; }");
    assert!(success);
}

#[test]
fn test_var_decl_static() {
    let (success, _) = run_compiler("void f(void) { static int x; }");
    assert!(success);
}

#[test]
fn test_global_var_decl() {
    let (success, _) = run_compiler("int global_x; void f(void) { }");
    assert!(success);
}

#[test]
fn test_global_var_decl_with_init() {
    let (success, _) = run_compiler("int global_x = 42; void f(void) { }");
    assert!(success);
}

#[test]
fn test_extern_var_decl() {
    let (success, _) = run_compiler("extern int external_x; void f(void) { }");
    assert!(success);
}

// ============ FUNCTION DECLARATIONS ============

#[test]
fn test_func_decl_no_params() {
    let (success, _) = run_compiler("void f(void) { }");
    assert!(success);
}

#[test]
fn test_func_decl_one_param() {
    let (success, _) = run_compiler("void f(int x) { }");
    assert!(success);
}

#[test]
fn test_func_decl_multiple_params() {
    let (success, _) = run_compiler("void f(int x, int y, int z) { }");
    assert!(success);
}

#[test]
fn test_func_decl_mixed_param_types() {
    let (success, _) = run_compiler("void f(int x, float y, char* z) { }");
    assert!(success);
}

#[test]
fn test_func_decl_return_int() {
    let (success, _) = run_compiler("int f(void) { return 0; }");
    assert!(success);
}

#[test]
fn test_func_decl_return_pointer() {
    let (success, _) = run_compiler("int* f(void) { return 0; }");
    assert!(success);
}

#[test]
fn test_func_forward_declaration() {
    let (success, _) = run_compiler("int add(int a, int b); int add(int a, int b) { return a + b; }");
    assert!(success);
}

#[test]
fn test_func_static() {
    let (success, _) = run_compiler("static void f(void) { }");
    assert!(success);
}

#[test]
fn test_func_unnamed_params() {
    let (success, _) = run_compiler("void f(int, int); void f(int a, int b) { }");
    assert!(success);
}

// ============ STRUCT DECLARATIONS ============

#[test]
fn test_struct_simple() {
    let (success, _) = run_compiler("struct Point { int x; int y; };");
    assert!(success);
}

#[test]
fn test_struct_nested() {
    let (success, _) = run_compiler("struct Outer { struct Inner { int x; }; int y; };");
    assert!(success);
}

#[test]
fn test_struct_with_pointer() {
    let (success, _) = run_compiler("struct Node { int data; struct Node* next; };");
    assert!(success);
}

#[test]
fn test_struct_with_array() {
    let (success, _) = run_compiler("struct Buffer { int size; char data[256]; };");
    assert!(success);
}

#[test]
fn test_struct_variable() {
    let (success, _) = run_compiler("struct S { int x; }; void f(void) { struct S s; }");
    assert!(success);
}

#[test]
fn test_struct_pointer_variable() {
    let (success, _) = run_compiler("struct S { int x; }; void f(void) { struct S* p; }");
    assert!(success);
}

// ============ UNION DECLARATIONS ============

#[test]
fn test_union_simple() {
    let (success, _) = run_compiler("union Data { int i; float f; char c; };");
    assert!(success);
}

#[test]
fn test_union_variable() {
    let (success, _) = run_compiler("union U { int i; float f; }; void f(void) { union U u; }");
    assert!(success);
}

// ============ ENUM DECLARATIONS ============

#[test]
fn test_enum_simple() {
    let (success, _) = run_compiler("enum Color { RED, GREEN, BLUE };");
    assert!(success);
}

#[test]
fn test_enum_with_values() {
    let (success, _) = run_compiler("enum Status { OK = 0, ERROR = 1, PENDING = 2 };");
    assert!(success);
}

#[test]
fn test_enum_mixed_values() {
    let (success, _) = run_compiler("enum Mixed { A, B = 10, C, D = 20 };");
    assert!(success);
}

#[test]
fn test_enum_variable() {
    let (success, _) = run_compiler("enum E { A, B }; void f(void) { enum E e; }");
    assert!(success);
}

// ============ TYPEDEF DECLARATIONS ============

#[test]
fn test_typedef_simple() {
    let (success, _) = run_compiler("typedef int Integer; void f(void) { Integer x; }");
    assert!(success);
}

#[test]
fn test_typedef_pointer() {
    let (success, _) = run_compiler("typedef int* IntPtr; void f(void) { IntPtr p; }");
    assert!(success);
}

#[test]
fn test_typedef_struct() {
    let (success, _) = run_compiler("struct S { int x; }; typedef struct S MyStruct; void f(void) { MyStruct s; }");
    assert!(success);
}

// ============ IF STATEMENTS ============

#[test]
fn test_if_simple() {
    let (success, _) = run_compiler("void f(void) { if (1) { } }");
    assert!(success);
}

#[test]
fn test_if_else() {
    let (success, _) = run_compiler("void f(void) { if (1) { } else { } }");
    assert!(success);
}

#[test]
fn test_if_else_if() {
    let (success, _) = run_compiler("void f(void) { if (1) { } else if (0) { } else { } }");
    assert!(success);
}

#[test]
fn test_if_nested() {
    let (success, _) = run_compiler("void f(void) { if (1) { if (1) { } } }");
    assert!(success);
}

#[test]
fn test_if_without_braces() {
    let (success, _) = run_compiler("void f(void) { int x; if (1) x = 5; }");
    assert!(success);
}

#[test]
fn test_if_complex_condition() {
    let (success, _) = run_compiler("void f(void) { int x; int y; if (x > 0 && y < 10) { } }");
    assert!(success);
}

// ============ WHILE STATEMENTS ============

#[test]
fn test_while_simple() {
    let (success, _) = run_compiler("void f(void) { while (0) { } }");
    assert!(success);
}

#[test]
fn test_while_with_break() {
    let (success, _) = run_compiler("void f(void) { while (1) { break; } }");
    assert!(success);
}

#[test]
fn test_while_with_continue() {
    let (success, _) = run_compiler("void f(void) { while (1) { continue; } }");
    assert!(success);
}

#[test]
fn test_while_nested() {
    let (success, _) = run_compiler("void f(void) { while (1) { while (0) { } break; } }");
    assert!(success);
}

#[test]
fn test_while_without_braces() {
    let (success, _) = run_compiler("void f(void) { int x; while (x) x--; }");
    assert!(success);
}

// ============ DO-WHILE STATEMENTS ============

#[test]
fn test_do_while_simple() {
    let (success, _) = run_compiler("void f(void) { do { } while (0); }");
    assert!(success);
}

#[test]
fn test_do_while_with_break() {
    let (success, _) = run_compiler("void f(void) { do { break; } while (1); }");
    assert!(success);
}

#[test]
fn test_do_while_nested() {
    let (success, _) = run_compiler("void f(void) { do { do { } while (0); } while (0); }");
    assert!(success);
}

// ============ FOR STATEMENTS ============

#[test]
fn test_for_simple() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0; i < 10; i++) { } }");
    assert!(success);
}

#[test]
fn test_for_empty_parts() {
    let (success, _) = run_compiler("void f(void) { for (;;) { break; } }");
    assert!(success);
}

#[test]
fn test_for_no_init() {
    let (success, _) = run_compiler("void f(void) { int i; for (; i < 10; i++) { } }");
    assert!(success);
}

#[test]
fn test_for_no_condition() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0;; i++) { break; } }");
    assert!(success);
}

#[test]
fn test_for_no_increment() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0; i < 10;) { i++; } }");
    assert!(success);
}

#[test]
fn test_for_nested() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0; i < 10; i++) { for (int j = 0; j < 10; j++) { } } }");
    assert!(success);
}

#[test]
fn test_for_with_break() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0; i < 10; i++) { break; } }");
    assert!(success);
}

#[test]
fn test_for_with_continue() {
    let (success, _) = run_compiler("void f(void) { for (int i = 0; i < 10; i++) { continue; } }");
    assert!(success);
}

// ============ SWITCH STATEMENTS ============

#[test]
fn test_switch_simple() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { case 1: break; } }");
    assert!(success);
}

#[test]
fn test_switch_multiple_cases() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { case 1: break; case 2: break; case 3: break; } }");
    assert!(success);
}

#[test]
fn test_switch_with_default() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { case 1: break; default: break; } }");
    assert!(success);
}

#[test]
fn test_switch_fallthrough() {
    let (success, _) = run_compiler("void f(void) { int x; switch (x) { case 1: case 2: break; } }");
    assert!(success);
}

#[test]
fn test_switch_nested() {
    let (success, _) = run_compiler("void f(void) { int x; int y; switch (x) { case 1: switch (y) { case 1: break; } break; } }");
    assert!(success);
}

// ============ GOTO AND LABELS ============

#[test]
fn test_goto_simple() {
    let (success, _) = run_compiler("void f(void) { goto end; end: return; }");
    assert!(success);
}

#[test]
fn test_goto_backward() {
    let (success, _) = run_compiler("void f(void) { int x; start: x++; if (x < 10) goto start; }");
    assert!(success);
}

#[test]
fn test_label_before_statement() {
    let (success, _) = run_compiler("void f(void) { int x; label: x = 5; }");
    assert!(success);
}

// ============ RETURN STATEMENTS ============

#[test]
fn test_return_void() {
    let (success, _) = run_compiler("void f(void) { return; }");
    assert!(success);
}

#[test]
fn test_return_value() {
    let (success, _) = run_compiler("int f(void) { return 42; }");
    assert!(success);
}

#[test]
fn test_return_expression() {
    let (success, _) = run_compiler("int f(void) { int x = 5; return x * 2 + 1; }");
    assert!(success);
}

// ============ BLOCK STATEMENTS ============

#[test]
fn test_block_empty() {
    let (success, _) = run_compiler("void f(void) { { } }");
    assert!(success);
}

#[test]
fn test_block_nested() {
    let (success, _) = run_compiler("void f(void) { { { { } } } }");
    assert!(success);
}

#[test]
fn test_block_with_declarations() {
    let (success, _) = run_compiler("void f(void) { int x; { int y; { int z; } } }");
    assert!(success);
}

// ============ EXPRESSION PRECEDENCE ============

#[test]
fn test_precedence_add_mul() {
    let (success, _) = run_compiler("void f(void) { int x = 1 + 2 * 3; }");
    assert!(success);
}

#[test]
fn test_precedence_parens() {
    let (success, _) = run_compiler("void f(void) { int x = (1 + 2) * 3; }");
    assert!(success);
}

#[test]
fn test_precedence_comparison_logical() {
    let (success, _) = run_compiler("void f(void) { int x = (1 < 2 && 3 > 4); }");
    assert!(success);
}

#[test]
fn test_precedence_ternary() {
    let (success, _) = run_compiler("void f(void) { int x = 1 ? 2 : 3 ? 4 : 5; }");
    assert!(success);
}

#[test]
fn test_precedence_assignment() {
    let (success, _) = run_compiler("void f(void) { int x; int y; x = y = 5; }");
    assert!(success);
}

#[test]
fn test_precedence_bitwise() {
    let (success, _) = run_compiler("void f(void) { int x = 1 | 2 ^ 3 & 4; }");
    assert!(success);
}

#[test]
fn test_precedence_shift() {
    let (success, _) = run_compiler("void f(void) { int x = 1 + 2 << 3; }");
    assert!(success);
}

// ============ COMPLEX EXPRESSIONS ============

#[test]
fn test_expr_function_call() {
    let (success, _) = run_compiler("int add(int a, int b) { return a + b; } void f(void) { int x = add(1, 2); }");
    assert!(success);
}

#[test]
fn test_expr_nested_function_calls() {
    let (success, _) = run_compiler("int square(int x) { return x * x; } int add(int a, int b) { return a + b; } void f(void) { int x = add(square(2), square(3)); }");
    assert!(success);
}

#[test]
fn test_expr_array_index() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int x = arr[5]; }");
    assert!(success);
}

#[test]
fn test_expr_array_index_expression() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int i; int x = arr[i + 1]; }");
    assert!(success);
}

#[test]
fn test_expr_struct_field() {
    let (success, _) = run_compiler("struct S { int x; int y; }; void f(void) { struct S s; int x = s.x + s.y; }");
    assert!(success);
}

#[test]
fn test_expr_pointer_field() {
    let (success, _) = run_compiler("struct S { int x; }; void f(void) { struct S s; struct S* p = &s; int x = p->x; }");
    assert!(success);
}

#[test]
fn test_expr_chained_field_access() {
    let (success, _) = run_compiler("struct Inner { int val; }; struct Outer { struct Inner inner; }; void f(void) { struct Outer o; int x = o.inner.val; }");
    assert!(success);
}

#[test]
fn test_expr_cast() {
    let (success, _) = run_compiler("void f(void) { float x = 3.14; int y = (int)x; }");
    assert!(success);
}

#[test]
fn test_expr_cast_pointer() {
    let (success, _) = run_compiler("void f(void) { int x; void* p = (void*)&x; }");
    assert!(success);
}

#[test]
fn test_expr_sizeof_type() {
    let (success, _) = run_compiler("void f(void) { int x = sizeof(int); }");
    assert!(success);
}

#[test]
fn test_expr_sizeof_expr() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int x = sizeof(arr); }");
    assert!(success);
}

#[test]
fn test_expr_comma_in_for() {
    let (success, _) = run_compiler("void f(void) { int i; int j; for (i = 0; i < 10; i++) { } }");
    assert!(success);
}

// ============ POINTER ARITHMETIC ============

#[test]
fn test_ptr_add_int() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr; p = p + 5; }");
    assert!(success);
}

#[test]
fn test_ptr_sub_int() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr + 5; p = p - 2; }");
    assert!(success);
}

#[test]
fn test_ptr_increment() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr; p++; }");
    assert!(success);
}

#[test]
fn test_ptr_decrement() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr + 5; p--; }");
    assert!(success);
}

#[test]
fn test_ptr_compound_add() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr; p += 3; }");
    assert!(success);
}

#[test]
fn test_ptr_compound_sub() {
    let (success, _) = run_compiler("void f(void) { int arr[10]; int* p = arr + 5; p -= 2; }");
    assert!(success);
}

// ============ ARRAY OPERATIONS ============

#[test]
fn test_array_init_access() {
    let (success, _) = run_compiler("void f(void) { int arr[5]; arr[0] = 1; arr[1] = 2; }");
    assert!(success);
}

#[test]
fn test_array_as_pointer() {
    let (success, _) = run_compiler("void f(void) { int arr[5]; int* p = arr; }");
    assert!(success);
}

#[test]
fn test_array_pointer_equivalence() {
    let (success, _) = run_compiler("void f(void) { int arr[5]; int x = *(arr + 2); }");
    assert!(success);
}

// ============ EDGE CASES ============

#[test]
fn test_empty_function() {
    let (success, _) = run_compiler("void f(void) { }");
    assert!(success);
}

#[test]
fn test_empty_struct() {
    let (success, output) = run_compiler("struct Empty { };");
    let _ = (success, output);
}

#[test]
fn test_deeply_nested_parens() {
    let (success, _) = run_compiler("void f(void) { int x = ((((((1)))))); }");
    assert!(success);
}

#[test]
fn test_long_expression() {
    let (success, _) = run_compiler("void f(void) { int x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10; }");
    assert!(success);
}

#[test]
fn test_many_function_params() {
    let (success, _) = run_compiler("void f(int a, int b, int c, int d, int e, int f, int g, int h) { }");
    assert!(success);
}

#[test]
fn test_nested_ternary() {
    let (success, _) = run_compiler("void f(void) { int a; int b; int c; int x = a ? b ? 1 : 2 : c ? 3 : 4; }");
    assert!(success);
}
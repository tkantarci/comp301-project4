(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; NO PROCEDURE CALLS UNTIL HERE!
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      ;; Recursive Print:
      ;; anonym --> 1

      (apply-simple-proc "let func = proc (x) -(x,1) in (func 30)" 29)
      ;; Recursive Print:
      ;; func --> 1

      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      ;; Recursive Print:
      ;; anonym --> 1
      ;; anonym --> 1

      (nested-procs2 "let g = proc(x) proc (y) -(x,y) in ((g -(10,5)) 6)" -1)
      ;; Recursive Print:
      ;; g --> 1
      ;; anonym --> 1
      

      (simple-letrec-1 "letrec minus(x) = -(x,1) in (minus 33)" 32)
      ;; Recursive Print:
      ;; minus --> 1

      (simple-letrec-2
        "letrec double(x) = if zero?(x)  
          then 0 else -((double -(x,1)), -2) in (double 4)"
        8)
      ;; Recursive Print:
      ;; double --> 1
      ;; ....double --> 2
      ;; ........double --> 3
      ;; ............double --> 4
      ;; ................double --> 5

      (simple-letrec-3
        "let m = -5 
          in letrec times-m(x) = if zero?(x) 
          then 0 else -((times-m -(x,1)), m) in (times-m 4)"
        20)
      ;; Recursive Print:
      ;; times-m --> 1
      ;; ....times-m --> 2
      ;; ........times-m --> 3
      ;; ............times-m --> 4
      ;; ................times-m --> 5

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;; EXTRA TEST CASES
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Check evenness of a positive number
      (even-test
       "let n = 8 in letrec even?(x) = if zero?(x) then 0 else if zero?(-(x, 1)) then -1 else (even? -(x, 2)) in (even? n)" 0)

      ;; Recursive Print:
      ;; even? --> 1
      ;; ....even? --> 2
      ;; ........even? --> 3
      ;; ............even? --> 4
      ;; ................even? --> 5


      (rec-test "let x = 1 in
                      let y = 5 in
                          letrec f(z) = if zero?(z) then 0
                              else -((f -(z,1)), -2) in (f 4)" 8)
      ;; Recursive Print:
      ;;f --> 1
      ;....f --> 2
      ;........f --> 3
      ;............f --> 4
      ;................f --> 5
      

      (fibonacci "letrec fib(n) =
                                  if zero?(n) then 0
                                  else if zero?(-(n,1)) then 1
                                                        else -((fib -(n,1)), -(0,(fib -(n,2))))
                                                                    in (fib 8)" 21)

      ;; Recursive Print:
      ;fib --> 1
      ;....fib --> 2
      ;........fib --> 3
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;........................fib --> 7
      ;............................fib --> 8
      ;............................fib --> 8
      ;........................fib --> 7
      ;....................fib --> 6
      ;........................fib --> 7
      ;........................fib --> 7
      ;................fib --> 5
      ;....................fib --> 6
      ;........................fib --> 7
      ;........................fib --> 7
      ;....................fib --> 6
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;........................fib --> 7
      ;........................fib --> 7
      ;....................fib --> 6
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;........fib --> 3
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;........................fib --> 7
      ;........................fib --> 7
      ;....................fib --> 6
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;................fib --> 5
      ;....fib --> 2
      ;........fib --> 3
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;........................fib --> 7
      ;........................fib --> 7
      ;....................fib --> 6
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;................fib --> 5
      ;........fib --> 3
      ;............fib --> 4
      ;................fib --> 5
      ;....................fib --> 6
      ;....................fib --> 6
      ;................fib --> 5
      ;............fib --> 4
      ;................fib --> 5
      ;................fib --> 5
      ))
  )

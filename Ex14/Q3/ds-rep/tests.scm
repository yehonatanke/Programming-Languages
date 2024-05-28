(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      ;; Expected output: (bool-val #f)
      (example-1 "
let p = proc (x)
  {
    Int : zero? (-(x, 8));
    Bool : if x then 20 else 30;
    Proc : -((x 50), 30);
  }
in (p 7)" #f)
       
      ;; Expected output: (num-val 30)
      (example-2 "
let p = proc (x)
  {
    Int : zero? (-(x, 8));
    Bool : if x then 20 else 30;
    Proc : -((x 50), 30);
  }
in (p zero?(4))" 30)

      ;; Expected output: (num-val -580)
      (example-3 "
let p = proc (x)
  {
    Int : zero? (-(x, 8));
    Bool : if x then 20 else 30;
    Proc : -((x 50), 30);
  }
in (p proc (d) { Int : -(d,600); })" -580)

      ;; Expected output: Error - duplicate types
      (example-4 "
let p = proc (x)
  {
    Int : zero? (-(x, 8));
    Bool : if x then 20 else 30;
    Int : -(5,x);
    Proc : -((x 50), 30);
  }
in (p 7)" error)

      ;; Expected output: Error - no body
      (example-5 "
let p = proc (x)
  {

  }
in (p 7)" error)

      ;; Expected output: Error - no body for such parameter
      (example-6 "
let p = proc (x)
  {
    Bool : if x then 20 else 30;
    Proc : -((x 50), 30);
  }
in (p 7)" error)

      ;; Additional Tests

      ;; Expected output: (num-val 1)
      (example-7 "
let p = proc (x)
  {
    Int : -(x, 1);
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p 2)" 1)

      ;; Expected output: (num-val 1)
      (example-8 "
let p = proc (x)
  {
    Int : 0;
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p zero?(0))" 1)

      ;; Expected output: (num-val 100)
      (example-9 "
let p = proc (x)
  {
    Int : x;
    Bool : 0;
    Proc : 100;
  }
in (p proc (d) { Int : 100; })" 100)

      ;; Expected output: Error - duplicate types
      (example-10 "
let p = proc (x)
  {
    Int : x;
    Bool : if x then 1 else 0;
    Int : -(x, 1);
    Proc : 0;
  }
in (p 5)" error)

      ;; Expected output: (num-val 3)
      (example-11 "
let p = proc (x)
  {
    Int : if zero?(x) then 0 else 3;
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p 1)" 3)

      ;; Expected output: (num-val 1)
      (example-12 "
let p = proc (x)
  {
    Int : if zero?(x) then 0 else 3;
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p zero?(0))" 1)

      ;; Expected output: Error - no body for such parameter
      (example-13 "
let p = proc (x)
  {
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p 100)" error)

      ;; Expected output: (num-val 9)
      (example-14 "
let p = proc (x)
  {
    Int : -(x, 1);
    Bool : if x then 1 else 0;
    Proc : 0;
  }
in (p 10)" 9)

      ;; Expected output: (num-val 100)
      (example-15 "
let p = proc (x)
  {
    Int : x;
    Bool : 0;
    Proc : 100;
  }
in (p proc (d) { Int : 99; })" 100)

      ;; Expected output: (bool-val #t)
      (example-16 "
let p = proc (x)
  {
    Int : zero?(x);
    Bool : x;
    Proc : 0;
  }
in (p 0)" #t)

      ;; Expected output: Error - no body for such parameter
      (example-17 "
let p = proc (x)
  {
    Int : x;
  }
in (p true)" error)

      ;; Expected output: Error - no body for such parameter
      (example-18 "
let p = proc (x)
  {
    Int : x;
    Bool : if x then 1 else 0;
  }
in (p proc (d) { Int : 1; })" error)

      ;; Expected output: (num-val 0)
      (example-19 "
let p = proc (x)
  {
    Bool : if x then 1 else 0;
  }
in (p zero?(1))" 0)

      ;; Expected output: (num-val 19)
      (example-20 "
let p = proc (x)
  {
    Int : 19;
    Bool : 0;
    Proc : 100;
  }
in (p 19)" 19)

      ;; Expected output: (num-val 100)
      (example-21 "
let p = proc (x)
  {
    Int : x;
    Bool : 0;
    Proc : 100;
  }
in (p proc (d) { Int : 77; })" 100)

      ;; Expected output: (num-val 1)
      (example-22 "
let p = proc (x)
  {
    Int : if zero?(x) then 1 else 0;
    Bool : 0;
    Proc : 100;
  }
in (p 0)" 1)

      ))
  )

# 1. Building Abstractions with Procedures

## 1.1. The Elements of Programming

### Exercise 1.1.

*Below is a sequence of expressions. What is the result printed by the
interpreter in response to each expression? Assume that the sequence is to be
evaluated in the order in which it is presented.*

```scheme
10
;Value: 10

(+ 5 3 4)
;Value: 12

(- 9 1)
;Value: 8

(/ 6 2)
;Value: 3

(+ (* 2 4) (- 4 6))
;Value: 6

(define a 3)
;Value: a

(define b (+ a 1))
;Value: b

(+ a b (* a b))
;Value: 19

(= a b)
;Value: false

(if (and (> b a) (< b (* a b)))
    b
    a)
;Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16

(+ 2 (if (> b a) b a))
;Value: 6

(* (cond ((> a b) a)
      ((< a b) b)
      (else -1))
(+ a 1))
;Value: 16
```

### Exercise 1.2.

*Translate the following expression into prefix form:*
`(5+4+(2-(3-(6+4/5)))) / (3*(6-2)*(2-7))`

```scheme
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
```

### Exercise 1.3.

*Define a procedure that takes three numbers as arguments and returns the sum
of the squares of the two larger numbers.*

```scheme
(define (largest-two-squared-sum x y z)
  (cond (= (min x y z) x) (sum-of-squares y z)
        (= (min x y z) y) (sum-of-squares x z)
        (= (min x y z) z) (sum-of-squares x y)))

;; helper procedures
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
```

### Exercise 1.4.

*Observe that our model of evaluation allows for combinations whose operators
are compound expressions. Use this observation to describe the behavior of the
following procedure:*

```scheme
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```

The procedure will add the value of a to the absolute value of b. It does this
via the conditional in the procedure definition: if b is positive, the object
that represents the “+” procedure is returned and the arguments will be added;
if not, the object that represents the “-“ procedure is returned and b will be
subtracted from a (cancelling the negative).

### Exercise 1.5.

*Ben Bitdiddle has invented a test to determine whether the interpreter he is
faced with is using applicative-order evaluation or normal-order evaluation.
He defines the following two procedures:*

```scheme
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
```

*Then he evaluates the expression:* `(test 0 (p))`

*What behavior will Ben observe with an interpreter that uses applicative-order
evaluation? What behavior will he observe with an interpreter that uses
normal-order evaluation? Explain your answer. (Assume that the evaluation rule
for the special form if is the same whether the interpreter is using normal or
applicative order: The predicate expression is evaluated first, and the result
determines whether to evaluate the consequent or the alternative expression.)*

* With the applicative-order evaluation: Ben will encounter a
  `StackOverflowError` as the compiler expands `(p)` into itself until the
  stack limit:

  ```scheme
  (test 0 (p))
  ;; ↓ expand (p)
  (test 0 (p))
  ;; ↓ expand (p) into (p) forever
  ;; …
  ;; StackOverflowError
  ```

* With the normal-order evaluation: The result will be 0 since the procedures
  are fully expanded to the condition, which short-circuits evaluation before
  the `(p)` term is reached:

  ```scheme
  (test 0 (p))
  ;; ↓ expand test to (if (= x 0) 0 y)
  (if (= 0 0) 0 (p))
  (if true 0 (p))
  0
  ```

### Exercise 1.6.

*Alyssa P. Hacker doesn't see why `if` needs to be provided as a special form.
“Why can't I just define it as an ordinary procedure in terms of `cond`?” she
asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she
defines a new version of `if`:*

```scheme
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
```

*Eva demonstrates the program for Alyssa:*

```scheme
(new-if (= 2 3) 0 5)
;Value: 5

(new-if (= 1 1) 0 5)
;Value: 0
```

*Delighted, Alyssa uses `new-if` to rewrite the square-root program:*

```scheme
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
```

*What happens when Alyssa attempts to use this to compute square roots? Explain.*

You get a StackOverflowError: `;Aborting!:maximum recursion depth exceeded`.
This is because, since the compiler uses applicative order, the second term to
`new-if` will recursively call `square-iter` forever: `new-if` never expands
into `cond` and thus can never test for a base case. The special form `if`, in
contrast, only evaluates one term, even in applicative order.

### Exercise 1.7.

*The `good-enough?` test used in computing square roots will not be very
effective for finding the square roots of very small numbers. Also, in real
computers, arithmetic operations are almost always performed with limited
precision. This makes our test inadequate for very large numbers. Explain these
statements, with examples showing how the test fails for small and large
numbers.*

*An alternative strategy for implementing `good-enough?` is to watch how
`guess` changes from one iteration to the next and to stop when the change is
a very small fraction of the guess. Design a square-root procedure that uses
this kind of end test. Does this work better for small and large numbers?*

* For very small numbers: 0.001 is a significantly large margin when you’re
  finding the square root of something of a similar (or much smaller) order of
  magnitude; thus, the answer wouldn’t be meaningful. For example:

  ```scheme
  (square (sqrt 1e-3))
  ;Value: 1.7011851721075596e-3 (~50% off)

  (square (sqrt 1e-9))
  ;Value: 9.765631660156936e-4 (nearly six orders of magnitude off)
  ```

* For very large numbers, floating point operations usually take place with
  limited precision (with details under that level of precision being
  inaccurate), so if the result isn’t accurate to 0.001, the program will get
  stuck in an endless loop. `(improve guess x)` will keep returning the same
  value.

  ```scheme
  (square (sqrt 1e12))
  ;Value: 1000000000000. (almost immediately)
  (square (sqrt 1e13))
  ;; <endless loop>
  ```

* To fix this, you can determine `good-enough?` by the relative change in
  `guess`; this will keep the delta at the right order of magnitude for both
  small and large numbers.

  ```scheme
  (define (good-enough? guess x)
    (< (abs (- (improve guess x) guess))
       (* guess 1e-9))) ;<0.1% change

  ;; Results with modified `good-enough?`
  (square (sqrt 1e-9))
  ;Value: 1.0010048424065935e-9
  (square (sqrt 1e13))
  ;Value: 10000985940724.807
  ```

### Exercise 1.8.

*Newton's method for cube roots is based on the fact that if y is an
approximation to the cube root of x, then a better approximation is given by
the value: `(x/y^2 + 2*y)/3`*

```scheme
(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 1e-9))) ;;accurate to nine digits

;; changed procedure
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

;; for testing
(define (cube x) (* x x x))
```

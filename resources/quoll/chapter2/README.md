# Notes
## Chapter 2
### 2.1.1
#### Rational Numbers
The `equal-rat?` function appears to be incorrect. The `*` characters should be `/` characters.

```scheme
(define (equal-rat? x y)
 (= (/ (numer x) (denom y))
    (/ (numer y) (denom x))))
```

While `cons` is the better way to go, it's also possible to build these functions using lambdas instead of other data structures. I believe that cons/car/cdr all get defined this way later in the book, using a `cond`. Below, I'm using lambdas.

```scheme
(define (cons2 a b) (lambda (f) (f a b)))

(define (car2 c) (c (lambda (a b) a)))

(define (cdr2 c) (c (lambda (a b) b)))
```

I love how EVERYTHING can be defined with lambdas.

In exercise 2.2, notice that points are just pairs? This means that rational numbers can be useed as points, and vice-versa. Languages like Clojure give you typing with records, which avoid the potential confusion that this can cause. Type safe languages like Haskell make it hard to not do this.

### 2.1.3
The definition of cons/car/cdr occurs here. Given the ability to use `let` I don't feel comfortable with `define` in a function, but I appreciate that it's common Scheme idiom. On the other hand, it's not needed here, since a lambda can be returned directly:

```scheme
(define (cons x y)
 (lambda (m)
  (cond ((= m 0) x)
        ((= m 1) y)
        (else (error "Argument not 0 or 1: CONS" m)))))
```

Amusingly, Exercise 2.4 appears to be what I explained above.

### 2.1.4
2.8 is sneaky, because it has to be (- lower upper), and (- upper lower)

## 2.2
Interesting note: "Closure" has 2 distinct meanings, but only one is used in this book.
The other meaning (procedures with free variables) is not in this book, though it is
the more common usage in contemporary programming.

### 2.2.1 Representing Sequences
Note that `'(1 2 3 4)` is equivalent to `(list 1 2 3 4)`. This is where the syntax for `'()` comes from for the empty list.





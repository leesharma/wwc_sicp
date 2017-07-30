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



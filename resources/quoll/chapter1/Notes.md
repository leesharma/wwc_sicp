#Notes

##Chapter 1
###1.1.8
####Local Names
[Footnote 20](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#footnote_Temp_32) shows an important concept:
> Declarative and imperative descriptions are intimately related, as indeed are mathematics and computer science. For instance, to say that the answer produced by a program is ``correct’' is to make a declarative statement about the program. There is a large amount of research aimed at establishing techniques for proving that programs are correct, and much of the technical difficulty of this subject has to do with negotiating the transition between imperative statements (from which programs are constructed) and declarative statements (which can be used to deduce things). In a related vein, an important current area in programming-language design is the exploration of so-called very high-level languages, in which one actually programs in terms of declarative statements. The idea is to make interpreters sophisticated enough so that, given ``what is’' knowledge specified by the programmer, they can generate ``how to’' knowledge automatically. This cannot be done in general, but there are important areas where progress has been made. We shall revisit this idea in chapter 4.

It is worth attempting to write code that _declares_ as much of its state as possible as data. The “mechanism” that operates on this data should be as minimized as possible. This makes code significantly more maintainable. It will also have far less duplication, and is typically easier to show to be correct.

A simple example is in the Java language, where a mapping of values is needed in a HashMap. There is no way to declare this directly. About the only kind of declaration that Java allows is arrays. This can be done with an array declaration, and a small bit of code. (The following presumes the declaration of a `PairSI` class, which is a pair made up of a String and an Integer).

```java
  static PairSI[] data = new PairSI[] {
        new PairSI("one", 1),
        new PairSI("two", 2),
        new PairSI("three", 3)
  };
  static Map<String,Integer> map = new HashMap<String,Integer>();
  static {
    for (PairSI p: data) map.put(p.first, p.second);
  }
```

This looks more complex thatn just creating the map and adding the data one element at a time, and in simple cases it is. However, as requirements evolve over time (as happens in all significant software projects), this approach allows for significant changes with minimal effort, and a simplicity that helps ensure correctness.

####Internal definitions and block structure
This section shows the use of `define` within the _lexical scope_ of a function (from where the `sqrt` function starts, until its closing parenthesis).

There are a few things worth noting here.

All names that have a `define` inside the function have been defined _within the lexical scope_. This means that it's theoretically possible to refer to the name before the definition. e.g.

```scheme
(define (triple-increment-then-double x)
  (define (inc-double a) (* 2 (inc x)))
  (define (inc b) (+ 1 b))
  (* 3 (inc-double x)))
```
Notice how `inc-double` is refering ahead to `inc`? Scheme allows this, because `inc` is defined as being valid within the _lexical scope_ of the `sqrt` function. i.e. it's somewhere in the function, so the entire function has access to it. This is very unusual for most languages.

There is an explicit reference to this peculiarity in [footnote 28](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#footnote_Temp_45). It starts out by asking that people **not** use forward declarations like this, and follows with a disclaimer about what happens if you do:
> Embedded definitions must come first in a procedure body. The management is not responsible for the consequences of running programs that intertwine definition and use.

Instead, many functional languages include a `let` block that defines a value within its scope. For 
instance, this returns 5:

```scheme
(let ((x 2)
      (y 3))
  (+ x y))
```
One issue with using `let` is that multiple definitions cannot refer to each other. For instance, the following will not work:

```scheme
(let ((x 2)
      (y (+ x 3)))
  y)
; Unbound variable: x
```

This fails, because `x` is still unbound when `y` tries to use it. One way around this problem is to bind `y` inside the scope of the `let` block that binds `x`. That can be done by nesting `let` blocks:

```scheme
(let ((x 2))
  (let ((y (+ x 3)))
    y))
```

While it works, it's messy. For this reason, an alternative version of `let` has been created, called `let*`:

```scheme
(let* ((x 2)
       (y (+ x 3)))
  y)
```

Languages like Clojure and Haskell have `let` operations that are much more like `let*` in Scheme.

It's some way ahead in the book, but these `let` blocks can be realized using lambdas (&lambda;). It turns out that you can do anything using &lambda;s, so this is not surprising.

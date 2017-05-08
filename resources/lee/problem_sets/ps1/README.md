# Problem Set 1

Here are my answers to [Problem Set 1] (book section 1.1). The full problem
set can be found there.

I skipped three questions that either depended on the MIT environment or were
outdated; the rest of the questions and answers are below.

[problem set 1.1]: https://mitpress.mit.edu/sicp/psets/ps1/readme.html

* [Exercises](#exercises)
* [Reflections](#reflections)

## Exercises

### Exercise 0: Prep Questions

The problem set body had several questions/tasks before the main exercises:

> **1. Before Turning on your Computer**
>
> Read the course notes through section 1.1. Then, predict what the interpreter
> will print in response to evaluation of each of the following expressions.
> Assume that the sequence is evaluated in the order which is presented here.
>
> **2. Getting started with the Scheme Interpreter**
>
> Type in and evaluate (one by one) the expressions from section 1 of this
> assignment to see how well you predicted what the system would print. If the
> system gives a response that you do not understand, ask for help from a lab
> tutor or from the person sitting next to you.

```scheme
;; Answers are in the following format:
;;
;;     <expression>
;;     ;Value: <guess>
;;     ;;Value: <actual result (only if wrong)>
;;     ;;Why: <explanation (only if wrong)>

(- 8 9)
;Value: -1

(> 3.7 4.4)
;Value: #f

(- (if (> 3 4) 7 10) (/ 16 10))
;Value: 84/10
;;Value: 42/5
;;Why: The compiler reduces the fraction to its base form.

(define b 13)
;Value: b

13
;Value: 13

b
;Value: 13

>
;#object: >
;;Value 3: #[arity-dispatched-procedure 3]
;;Why: I was right that the operator reference was returned, but the output was
;;     different (#object is clojure). This means that it's the third used
;;     procedure, and it's an "arity dispatched procedure".
;;
;;     Best I can figure, "arity dispatched procedure" is a BIF with flexible
;;     arity: the procedure is dispatched to a specific implentation based on
;;     the arity. Maybe it's a higher-order BIF? Inflexible BIFs are called
;;     "compiled procedures" and list their namespace.

(define square (lambda (x) (* x x)))
;Value: square

square
;Value: (lambda (x) (* x x))
;;Value 4: #[compound-procedure 4 square]
;;Why: Similar to above, this is a reference to a compound procedure (a
;;     procedure using other procedures and building blocks) named "square".

(square 13)
;Value: 169

(square b)
;Value: 169

(square (square (/ b 1.3)))
;Value: 10000
;;Value: 10000.
;;Why: Because 1.3 is a floating point number, so is the result.

(define multiply-by-itself square)
;Value: multiply-by-itself

(multiply-by-itself b)
;Value: 169

(define a b)
;Value: a

(= a b)
;Value: #t

(if (= (* b a) (square 13))
    (< a b)
    (- a b))
;Value: #f

(cond ((>= a 2) b)
      ((< (square b) (multiply-by-itself a)) (/ 1 0))
      (else (abs (- (square a) b))))
;Value: 13
```

*followed `C-h t` edwin tutorial*

### Exercise 1: More debugging

> The code you loaded for problem set 1 also defined three other procedures,
> called `fold`, `spindle`, and `mutilate`. One of these procedures contains
> an error. Evaluate the expression `(fold 1 2)`. What is the error? How should
> the procedure be defined? (Notice that you can examine the code for a
> procedure using the `pp` ("pretty print") command. For example, evaluating
> the expression `(pp fold)` will print the definition of `fold`.)

The `fold` command is incorrect. This is the given function:

```scheme
;; bad function

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle x y)))))
```

There's a problem with this: in the last line, `spindle` is called with two
arguments, but it's only defined with one:

```scheme
(define spindle
  (lambda (w) (* w w)))
```

You could fix this by removing one of the arguments:

```scheme
;; good function

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle y)))))
```

### Exercise 2: Still more debugging

> The code you loaded also contains a buggy definition of a procedure meant to
> compute the factorials of positive integers:
> `n! = n * (n - 1) * (n - 2) * ... * 3 * 2 * 1`.
> Evaluate the expression `(fact 5)` (which is supposed to return 120). Use the
> debugger to find the bug and correct the definition. Compute the factorial
> of 243. Copy the corrected definition and the value of `(fact 243)` into your
> `ps1-ans.scm` buffer.

```scheme
;; corrected factorial function

(define fact
  (lambda (n)
    (if (= n 0)
        1 ; used to be `m`, and unbound variable
        (* n (fact (- n 1))))))
```

```scheme
(fact 243)
;Value: 5765107207340556485993259937898882438954461276974878528957851475379122\
;26660795447787952561780489668440613028916503471522241703645767996810695135226\
;27829674263760611513430078705299131943141237931254023079206025013708870881179\
;44245648331070851734647189855089998587919706094910660457118743215169181509054\
;13944789377156315207186998055591451670633898714567745386826936678840548225648\
;08996172787570544453816714281829286281216000000000000000000000000000000000000\
;0000000000000000000000
```

### Exercise 3: Defining a simple procedure

> The number of combinations of *n* things taken *k* at a time is given by
> `n! / (k! * (n - k)!)`. Define a procedure `(comb n k)` that computes the
> number of combinations, and find the number of combinations of 243 things
> taken 90 at a time. Include your procedure definition and your answer in
> `ps1-ans.scm`.

```scheme
(define comb
  (lambda (n k)
    (/ (fact n)
       (* (fact k)
          (fact (- n k))))))
```

```scheme
(comb 243 90)
;Value: 193404342391239489855973693417880600543891038618846567058277413638164
```

### Exercise 4: Practice with the editor

> Find the Free Software Foundation copyright notice at the end of the Edwin
> tutorial. Copy it into `ps1-ans.scm`.

```
This tutorial descends from a long line of Emacs tutorials
starting with the one written by Stuart Cracraft for the original Emacs.

This version of the tutorial, like GNU Emacs, is copyrighted, and
comes with permission to distribute copies on certain conditions:

Copyright (c) 1985 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

The conditions for copying Emacs itself are slightly different
but in the same spirit.  Please read the file COPYING and then
do give copies of GNU Emacs to your friends.
Help stamp out software obstructionism ("ownership") by using,
writing, and sharing free software!
```

### Exercise 5: Learning to use `Info` (SKIPPED)

> Start up the `info` program with the Edwin command `M-x info`. `Info` is a
> directory of useful information. You select a topic from the menu by typing
> `m` followed by the name of the topic. (You can type a few characters that
> begin the topic name, and then type a space, and Edwin will complete the
> name.) One of the `info` options (type `h`) gives you a brief tutorial in how
> to use the program. Use `info` to find the cost of a 12-inch pizza from
> Pizza Ring, and copy this to the answer buffer.

**Requires the MIT environment to complete**


### Exercise 6: Hacker Jargon (SKIPPED)

> The info `Jargon` entry is a collection of hacker terms that was eventually
> published in 1983 as the book _The Hacker's Dictionary_. The `New Jargon`
> info entry is an expanded version that was published in 1991 by MIT Press
> as _The New Hacker's Dictionary. The old Jargon file is structured as an
> `info` file, with submenus; the new version is a single text file, which you
> can search. Find the defintion of "Unix conspiracy" from the new jargon file,
> and find the definition of "Phase of the Moon" from either jargon file.
> Include these in the answer buffer.

**Requires the MIT environment to complete**

### Exercise 7: Scheme documentation

> The `Scheme` info entry is an on-line copy of the Scheme reference manual
> that was distributed with the notes. Find the description of "identifiers" in
> the documentation. What is the longest identifier in the list of example
> identifiers?

the-word-recursion-has-many-meanings

### Exercise 8: More documentation

> There is an Edwin command that you can use to automatically re-indent
> expressions. For example, if you have a procedure typed as
>
>   ```scheme
>   (define test-prodcedure
>   (lambda (a b)
>   (cond ((>= a 2) b)
>       ((< (square b)
>    (multiply-by-itself a))
>               (/ 1 0))
>   (else (abs (- (square a) b))))))
>   ```
>
> you can move the cursor to the beginning of the line with `define`, type in
> this Edwin command, and the expression will be automatically re-indented as:
>
>   ```scheme
>   (define test-procedure
>     (lambda (a b)
>       (cond ((>= a 2) b)
>             ((< (square b)
>                 (multiply-by-itself a))
>              (/ 1 0))
>             (else (abs (- (square a) b))))))
>   ```
>
> Find the description of this command. What is the command? Where did you find
> it?

The function is `indent-sexp` (`M-C-q` or `M-x indent-sexp`). I found it with
the by searching the `help` documentation with `C-h a` for `indent`. Several of
the names looked promising, so I looked at the descriptions/tried several out
in the `*scratch*` buffer until I found the right one.

### Exercise 9: Running Shell Commands

> If you run the Scheme interpreter within a *nix environment, the Edwin
> command `M-x shell` will create a shell buffer, which you can use to run
> various *nix programs. Some of the more interesting ones are `ls`, `date`,
> and `echo`. For example, typing
>
>     date
>
> will show you the current date and time (e.g. `Tue Jul 2 15:29:45 EDT 1996`).
> See if you can figure out what the other commands to, and copy your results
> to the answer buffer.

The `ls` command lists the contents of the local (or given) directory, and the
`echo` command prints (or "echos" back) its arguments.

```bash
1.1 $ ls ../..
1-building_abstractions_with_procedures 4-metalinguistic_abstraction
2-building_abstractions_with_data       5-computing_with_register_machines
3-modularity_objects_and_state          problem_sets

1.1 $ echo Hello, world\!
Hello, world!
```

### Exercise 10: Getting information from around the world (SKIPPED)

> You can also use the finger program to query computers on the Internet, all
> over the world. You can finger a particular name at some location, or just
> finger the location (e.g. `finger @mit.edu`) to get general information.
> Try fingering some of the following places:
>
>   * `cs.berkeley.edu`-a computer run by the computer science department at UC
>     Berkeley
>   * `idt.unit.no`-a computer at the Norwegian Institute of Technology, a part
>     of The University of Trondheim, Norway.
>   * `whitehouse.gov`-the White House
>
> See what else you can find. Include some piece of this in your answer file.

**These URLs are out of date, and this problem isn't very relevant**

### Exercise 11

> An *application* of expression *E* is an expression of the form
> (*E E_1 ... E_n*). This includes the case *n*=0, corresponding to an
> expression (*E*). A *Curried application* of *E* is either an application of
> *E* or an application of a Curried application of *E*. For each of the
> procedures defined below, give a Curried application of the procedure which
> evaluates to 3.
>
>   ```scheme
>   (define foo1
>     (lambda (x)
>       (* x x)))
>
>   (define foo2
>     (lambda (x y)
>       (/ x y)))
>
>   (define foo3
>     (lambda (x)
>       (lambda (y)
>         (/ x y))))
>
>   (define foo4
>     (lambda (x)
>       (x 3)))
>
>   (define foo5
>     (lambda (x)
>       (cond ((= x 2) (lambda () x))
>             (else (lambda () (* x 3))))))
>
>   (define foo6
>     (lambda (x)
>       (x (lambda (y) (y y)))))
>   ```
>
> Type in these definitions, and include a demonstration of your answers in the
> answer buffer.

```scheme
(foo1 (sqrt 3))

(foo2 3 1)

((foo3 3) 1)

(foo 4 (lambda (x) x))

((foo5 1))

(foo6 (lambda (x) 3))
```


## Reflections

* What exactly is an "arity dispatched procedure" (exercise 0)? What are the
  other primatives for procedures?

* Interesting facts about arithmatic base cases:
  ```scheme
  ;;Because of their left identity elements:

  (+)
  ;Value: 0

  (*)
  ;Value: 1

  ;; (-) and (/) only have right-identity elements (0 and 1), and since scheme is
  ;; left-associative, defaults don't make sense (think of `reduce`).
  ```

# Notes
## Chapter 1
### 1.1.8
#### Local Names
[Footnote 20](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#footnote_Temp_32) shows an important concept:

> Declarative and imperative descriptions are intimately related, as indeed are mathematics and computer science. For instance, to say that the answer produced by a program is "correct" is to make a declarative statement about the program. There is a large amount of research aimed at establishing techniques for proving that programs are correct, and much of the technical difficulty of this subject has to do with negotiating the transition between imperative statements (from which programs are constructed) and declarative statements (which can be used to deduce things). In a related vein, an important current area in programming-language design is the exploration of so-called very high-level languages, in which one actually programs in terms of declarative statements. The idea is to make interpreters sophisticated enough so that, given "what is" knowledge specified by the programmer, they can generate "how to" knowledge automatically. This cannot be done in general, but there are important areas where progress has been made. We shall revisit this idea in chapter 4.

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

#### Internal definitions and block structure
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

Clojure uses its _vector_ syntax (using `[`square brackets`]` instead of the `(`parentheses`)` used by lists) to define let bindings, and presumes that name/values appear in pairs. This makes it more visually appealing:

```clojure
(let [x 2
      y (+ x 3)]
  y)
```

It's some way ahead in the book, but these `let` blocks can be realized using lambdas (&lambda;). It turns out that you can do anything using &lambda;s, so this is not surprising.

### 1.2.1
#### Linear Recursion
Having watched the lectures, I got confused on iteration. I thought I was looking at recursion. At this point I knew how functions are called:

* Push parameters to the stack
* Push the current stack pointer
* Push the current instruction pointer
* Jump to the entry point of the function to be called

So when iteration was shown by calling a function recursively, I didn't understand how this could be called _iteration_. But the lectures didn't show this part from the text:

> In contrasting iteration and recursion, we must be careful not to confuse the notion of a recursive _process_ with the notion of a recursive _procedure_. When we describe a procedure as recursive, we are referring to the syntactic fact that the procedure definition refers (either directly or indirectly) to the procedure itself. But when we describe a process as following a pattern that is, say, linearly recursive, we are speaking about how the process evolves, not about the syntax of how a procedure is written. It may seem disturbing that we refer to a recursive procedure such as fact-iter as generating an iterative process. However, the process really is iterative: Its state is captured completely by its three state variables, and an interpreter need keep track of only three variables in order to execute the process.

This explains why it looked recursive and not iterative, but I would still have had issues with the use of the stack to call these functions. It was only later that I learnt about "Tail Call Optimization" (TCO). This was a technique pioneered by the people building Scheme. It means that a _recursive procedure_ need not use the function calling steps above, and can be executed as a _linear process_ instead. This is done by:

* Replace the values in the stack with the new values for the next iteration.
* Jump to the start of the function.

Clojure can optimize its tail calls, but it needs to be referred to manually, using the `recur` operation. This means that we can look at the difference between standard recursion vs. TCO recursion. We do this using a Java utility called **javap**. This prints the bytecode that the Java Virtual Machine actually runs. The same sort of thing can be done with Scheme, or C, or any language, by looking at the assembler code that the compiler generates.

##### Recursive function

Looking at the Clojure recursive function:

```clojure
(defn sum [a b]
  (if (= 0 a)
      b
      (sum (- a 1) (+ b 1))))
```

Then we see that this is turned into the following:

```
       0: lconst_0
       1: lload_0
       2: lcmp
       3: ifne          14
       6: lload_2
       7: invokestatic  #19                 // Method clojure/lang/Numbers.num:(J)Ljava/lang/Number;
      10: goto          38
      13: pop
      14: getstatic     #23                 // Field const__2:Lclojure/lang/Var;
      17: invokevirtual #29                 // Method clojure/lang/Var.getRawRoot:()Ljava/lang/Object;
      20: checkcast     #6                  // class clojure/lang/IFn$LLO
      23: lload_0
      24: lconst_1
      25: invokestatic  #33                 // Method clojure/lang/Numbers.minus:(JJ)J
      28: lload_2
      29: lconst_1
      30: invokestatic  #36                 // Method clojure/lang/Numbers.add:(JJ)J
      33: invokeinterface #39,  5           // InterfaceMethod clojure/lang/IFn$LLO.invokePrim:(JJ)Ljava/lang/Object;
      38: areturn
```

This looks very obtuse, but it is just a series of simple operations, which we can analyze.

Line 0 gets the constant `0`. Line 1 gets the first long number from the stack (the `a` argument). Line 2 compares `a` and `0`. If they're not equal, then line 3 jumps ahead to line 14 to do the recursion. Otherwise (when `a == 0`) line 6 gets the second long number from the stack (`long` values take up 2 spaces of the stack). Line 7 is a call that ensures it's a number. Then line 10 jumps to 38, which will return the last loaded value: `b`. So it's just done the following:  
`if (a == 0) then return b;`

Otherwise, `(a != 0)` so this is where it has to increment `a`, decrement `b` and recurse.

Lines 14-20 get the function we're calling (the `sum` function that we're currently in), and make sure it's what it's supposed to be. This is in preparation to call it.

Line 23 gets `a`, line 24 gets the number `1`, and line 25 subtracts it from `a`. This is the `(- a 1)`, and the result is left on the stack.

Line 28 gets `b`, line 29 gets `1`, and line 30 adds them. This is the `(+ b 1)`, and the result is also left on the stack.

Line 33 calls the function we got earlier, using the values on the stack as the arguments. This is the equivalent of:  
`sum(a-1, b+1)`  
The result is returned on line 38.

##### Iterative function
TCO can be obtained with a minor tweak to use `recur` instead of the function name:

```clojure
(defn sum [a b]
  (if (= 0 a)
      b
      (recur (- a 1) (+ b 1))))
```

The resulting process is:

```
       0: lconst_0
       1: lload_0
       2: lcmp
       3: ifne          14
       6: lload_2
       7: invokestatic  #19                 // Method clojure/lang/Numbers.num:(J)Ljava/lang/Number;
      10: goto          29
      13: pop
      14: lload_0
      15: lconst_1
      16: invokestatic  #23                 // Method clojure/lang/Numbers.minus:(JJ)J
      19: lload_2
      20: lconst_1
      21: invokestatic  #26                 // Method clojure/lang/Numbers.add:(JJ)J
      24: lstore_2
      25: lstore_0
      26: goto          0
      29: areturn
```

This starts similarly. Lines 0-10 check if `(a == 0)`, jumping to the end if they are equal.

Lines 14-16 do `(- a 1)`. Lines 19-21 do `(+ b 1)`.

This time, instead of leaving them at the top of the stack, line 24 stores the result of the addition into `b`, and line 25 stores the result of the subtraction into `a`. Line 26 then jumps back to the beginning.

##### Comparing with pseudocode
Here is a pseudocode rewriting of the recursive version (skipping some of the irrelevant steps):

```
  if (a == 0) return b;
  get function "sum";
  stack1 = (a - 1);
  stack2 = (b + 1);
  call sum(stack1, stack2);
```

Here is pseudocode for the iterative version:

```
  if (a == 0) return b;
  stack1 = (a - 1);
  stack2 = (b + 1);
  b = stack2;
  a = stack1;
  goto start;
```

You may notice that the iterative version looks a lot like a `while` loop. This is why languages like C, C++, Java, Javascript, Python, and Ruby all have looping control structures that look like this. They are very easy for the computer to convert them into simple operations that the machine knows how to execute.

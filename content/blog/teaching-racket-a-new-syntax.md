---
title: 'Teaching Racket a new syntax'
description: 'Growing the language with macros, until Haskell list comprehensions fit inside it'
pubDate: 'Sep 07 2026'
tags: ['paradigms', 'languages', 'racket', 'lisp', 'macros']
heroImage: '/images/racket.png'
---

<img src="/Racket-logo.svg" alt="Racket logo" style="float: right; margin-right: 1em; width: 10em;" width="512" height="512" />

In [One lang to rule them all](/blog/one-lang-to-rule-them-all) we walked through **three paradigms** in a single language.
Objects, logic and functions all found a **comfortable home** in Racket.

But one thing didn't survive the trip.
Haskell writes filtering like this

```haskell
[x * x | x <- [1..10], even x]
```

and Racket has **no such syntax**.
The closest we got was `for/list`, which works, but reads like a loop rather than like **set-builder notation**.

In most languages that would be the end of the story.
You write down what the language gives you and **move on**.

In a LISP you have another option: **write the syntax yourself**.

## Reflective Programming

Racket is a language that gives you **control over its own structure and behaviour**.
A key reason for this is its **homoiconic** design: in Racket, **code is represented using the same data structures that the language itself provides**.
Because programs are built from ordinary data—lists, symbols, and structured syntax objects, you can **inspect, construct, and transform code just as easily as any other value**.

This foundation enables Racket's powerful **macro system**, that lets you create **new syntax** that looks and behaves just like built-in constructs, giving you the tools to **design new languages** tailored to your domain.

While in most languages, code runs only **after** it's written, with Racket's macros code can also **run while it's being read**.
A macro is a function that **transforms code before it's executed**, by rewriting **syntax objects** into something Racket already understands.

That means you can define **new syntax constructs** without worrying about **variable capture** or **name clashes**.

## Understanding Macros

Before building our comprehension, let's get comfortable with **how macros are made**.

### The two times of a program

Every Racket program lives in **two moments**:

- **Expansion time**: the compiler reads your code, expands every macro, and keeps expanding until nothing but **core forms** remain.
- **Run time**: the expanded program actually executes.

A function runs at **run time** and receives **values**.
A macro runs at **expansion time** and receives **code**.

That single difference is the whole story.

### Your first macro

The simplest way to write one is with `define-syntax-rule`, which lets you define pattern-based rewrites.
For example:

```racket
#lang racket

(define-syntax-rule (when-not cond body ...)
  (when (not cond) body ...))

(when-not #f (displayln "Runs!"))
```

This macro expands `(when-not cond body ...)` into `(when (not cond) body ...)`.

The shape reads as **a pattern and a template**:

- On the left, the **pattern** describes how the new form looks when you write it.
- On the right, the **template** describes what it turns into.
- The **ellipsis** (`...`) means *"zero or more of the thing before me"*, and it appears in **both** sides: whatever `body ...` captured gets **spliced back** in the same order.

### Why not just a function?

A fair question. `when-not` looks like something a function could do:

```racket
(define (when-not/fn cond thunk)
  (unless cond (thunk)))
```

But a function **receives values**, which means its arguments are **already evaluated** before it gets a say.
That is why we had to wrap the body in a `thunk` above, and why the caller is forced to write `(lambda () ...)` at every use site.

Macros have **no such problem**, because they never see values: they see **unevaluated code** and decide **if** and **when** it runs.

```racket
(define-syntax-rule (my-or a b)
  (let ([tmp a])
    (if tmp tmp b)))

(my-or #t (begin (displayln "never printed") #f))
```

Short-circuiting, custom binding forms, delayed evaluation, embedded sub-languages: all of it needs **code**, not values.

### Watching the expansion

Macros stop being magic the moment you can **see** what they produce.

Load the file and ask the **REPL** what your macro turns into:

```racket
> (syntax->datum (expand-once #'(when-not #f (displayln "Runs!"))))
'(when (not #f) (displayln "Runs!"))
```

- `expand-once` performs **a single** expansion step.
- `syntax->datum` strips away the bookkeeping so we can read the result as plain data.

It has to run in the REPL (or with an explicit namespace), because expansion needs to know **which bindings are in scope**, and inside a module body that information isn't available at run time.

Keep this snippet nearby.
When a macro misbehaves, **printing its expansion** is almost always faster than guessing.
DrRacket ships a *Macro Stepper* that does the same thing interactively.

### Hygiene, for free

Notice that `my-or` introduced a variable named `tmp`.
In most macro systems (think C's preprocessor) that is a **landmine**:

```racket
(define tmp "my precious")
(my-or #f tmp)
```

If the expansion's `tmp` and the user's `tmp` were the same identifier, that call would return `#f` instead of `"my precious"`.

Racket returns `"my precious"`. 🎉

That property is called **hygiene**: identifiers introduced by a macro are **automatically kept distinct** from identifiers written by the caller.
The macro system tracks the **scope** an identifier came from, so the two `tmp`s simply are not the same variable.

You get it **by default**, and you have to opt out explicitly if you ever want to break it.

### When patterns are not enough

`define-syntax-rule` is a **one-pattern shortcut**.
As soon as a form has **several possible shapes**, or you want to **compute** something while expanding, you reach for the general version:

```racket
(define-syntax (name stx)
  ...)
```

Here `name` is bound to an **ordinary function** that receives the whole call as a **syntax object** (`stx`) and returns the code to replace it with.
Any Racket you want can run **in between**.

To take that syntax object apart, we use `syntax-parse`:

```racket
(require (for-syntax racket/base syntax/parse))

(define-syntax (my-when stx)
  (syntax-parse stx
    [(_ test:expr body:expr ...+)
     #'(if test (let () body ...) (void))]))
```

Three details worth naming:

- `_` matches the macro's own name, which we never care about.
- `test:expr` is a **syntax class annotation**: it says *"this must be an expression"*, and gives **decent error messages** when it isn't.
- `...+` means **one or more**, so `(my-when #t)` is rejected at compile time instead of silently doing nothing.

### The phase distinction

One last thing before we build.
The `(for-syntax ...)` in that `require` is not decoration.

Macro code runs **before** your program, so it needs its libraries **at expansion time**.
Racket calls these **phases**, and keeps them strictly separate:

- `(require syntax/parse)` gives you the bindings at **run time**, which is useless for a macro.
- `(require (for-syntax syntax/parse))` gives them to you at **expansion time**, which is what we want.
- `(begin-for-syntax ...)` lets you **define your own helpers** at expansion time.

That separation is what makes Racket's macros composable instead of a pile of textual substitution.

---

With that, we have every piece we need.
Our goal is **to embed** the Haskell syntax for **list-comprehension**

```haskell
[x * x | x <- [1..10], even x]
```

## Defining an Inclusive Range Function

First, we'll replicate Haskell's syntax for **creating inclusive ranges** (like `[1..10]`)

```racket
(define (.. a b . maybe-step)
  (define step
    (cond
      [(pair? maybe-step) (car maybe-step)]
      [(<= a b) 1]
      [else -1]))
  (cond
    [(zero? step) (error '.. "step must be non-zero")]
    [(and (> step 0) (> a b)) (in-range a a step)]
    [(and (< step 0) (< a b)) (in-range a a step)]
    [(> step 0) (in-range a (add1 b) step)]
    [else (in-range a (sub1 b) step)]))
```

Note that this one is **just a function**, no macro needed.
`..` is a perfectly ordinary identifier in Racket, so we get the name for free.

Here we basically set `step` as the value passed by the caller if present, or set it to `1` or `-1` for ascending or descending ranges.

Then we do a basic validation and go about creating ranges with the `in-range` built-in.

The result can be seen with

```racket
(for/list ([n (.. 10 2 -2)]) n)  ; '(10 8 6 4 2)
```

## Tackling the problem

A proposed syntax for list comprehension can be something like this:

```racket
(list-comp (* x x)
           [x <- (.. 1 10)]
           (even? x))
; '(4 16 36 64 100)
```

If we pay attention we can see that conceptually it is made of:

- The **body** (`(* x x)`) that describes what to produce
- Then **clauses**, that could either be:
  - **Generators**: `[x <- (.. 1 10)]`
  - **Guards**: `(even? x)`

We'll parse clauses and expand these into Racket's built-in comprehension form: the `for*/list` loop.

That target matters: we are **not** writing a loop by hand, we are **translating** our surface syntax into a form Racket already knows how to compile well.
Macros are at their best when they are a **thin layer of notation** over something solid.

### Starting from the parts

We'll define a **syntax class** `comp-clause` that understands both generators and guards.
Basically it behaves as a **mini parser**.

Inside a `begin-for-syntax` block (so it **runs at macro-expansion time**):

```racket
(begin-for-syntax
  (define-syntax-class comp-clause
    #:attributes (pieces binds?)
    #:datum-literals (<-)

    ;; Generator: [pat <- seq]
    (pattern [pat:expr <- seq:expr]
      #:with tmp (generate-temporary #'pat)
      #:with pieces #'([tmp seq]
         #:when (match tmp [pat #t] [_ #f])
         #:do [(match-define pat tmp)])
      #:with binds? #'#t)

    ;; Guard: bare expression
    (pattern g:expr
      #:with pieces #'(#:when g)
      #:with binds? #'#f)))
```

Here's what's happening:

- We define how to parse two clause types.
- Each clause (`q:comp-clause`) produces **attributes** that our main macro can access.
- `#:datum-literals (<-)` tells `syntax-parse` that `<-` is a **literal token** to match on, not a pattern variable to bind. Without it, `<-` would happily match anything.
- We declare that **generators** look like `[pattern <- sequence]`
  - We create a temporary variable (`tmp`) to hold each value from the sequence.
  - Then, we use `match` and `match-define` to destructure it.
- Guards are just booleans like `(even? x)`, so we translate them into `#:when` filters.
- The `pieces` attribute describes what goes into a `for*/list` comprehension later.
- `binds?` tracks whether any generator was found (so we can handle the case of only guards).

The **order of the patterns** is not an accident.
`syntax-parse` tries them **top to bottom**, and a bare `g:expr` would match a generator too, so the specific case has to come **first**.

#### A quick detour

You'll notice **syntax quoting** everywhere, like `#'([tmp seq])`.

This is like a regular quote (`'`), but it **preserves syntax objects** (which contain source locations, bindings and scope information), not raw data.

Let's see the difference:

```racket
(quote (x y))  ; -> '(x y)
#'(x y)        ; -> a syntax object representing the code (x y)
```

- The first is just **data**: Racket treats it like a list of symbols.
- The second is **syntax**: Racket knows **where** it came from in your program and **what** each identifier refers to.

Those source locations are also why **error messages point at your code** instead of at the macro's guts.

Using syntax quotes ensures your macro works **hygienically**, without **variable capture** or **namespace issues**.

Imagine you wrote a macro that introduces a variable `tmp` inside its expansion.
What happens if the user's code also has a `tmp`?

Without protection, your macro might accidentally shadow their variable, changing its meaning.

When you use syntax quoting (`#'`) and `generate-temporary`, Racket prevents that by ensuring the new identifiers you create don't interfere with user code.
So even if both you and the user have a variable called `tmp`, they remain distinct behind the scenes.

### A helping hand

After parsing multiple clauses, we'll have lists of syntax pieces like:

```racket
#'([x (.. 1 10)] #:when (even? x))
```

These come in as nested syntax objects, not flat lists.
To feed them into `for*/list`, we need to flatten them.

Similarly, we'll want to know whether the comprehension includes any generator clauses (to handle the case of guards-only comprehensions).

So inside the `begin-for-syntax`, after `define-syntax-class`, we'll add these helpers:

```racket
(begin-for-syntax
  ;; (define-syntax-class comp-clause
  ;; ...
  ;;       #:with binds? #'#f)))

  (define (flatten-pieces stx-list)
    (apply append (map syntax->list stx-list)))

  (define (any-true? stxes)
    (for/or ([s (in-list stxes)]) (syntax-e s))))
```

To clarify:

- `syntax->list` unwraps a syntax object that contains a list-like form (e.g., `#'([a b])`) into actual sub-syntaxes.
- `syntax-e` extracts the underlying datum (like `#t`, `#f`, or `'foo`) from a syntax object.

These are **plain functions**.
They just happen to live at **phase 1**, which is why they are inside `begin-for-syntax`.

### The solution

Finally we will rely heavily on Racket's advanced macro system, by accessing it with `syntax-parse`.

Basically we:

1. Collect the body and all clauses.
2. Expand the clauses into `for*/list` syntax.
3. Handle the case where no generators are present (guards only).

```racket
(define-syntax (list-comp stx)
  (syntax-parse stx
    [(_ body:expr q:comp-clause ...)
     (define flat (flatten-pieces (syntax->list #'(q.pieces ...))))
     (define has-bind? (any-true? (syntax->list #'(q.binds? ...))))
     (with-syntax ([(clauses ...) flat])
       (cond
         [has-bind?
          #'(for*/list (clauses ...)
              body)]
         [else
          ;; No generators: add a dummy binding so guards-only work
          #'(for*/list ([_ '(#t)]
                        clauses ...)
              body)]))]))
```

To unpack this:

- `q:comp-clause` pulls in every clause parsed by our syntax class.
- `q.pieces` is the **attribute** we attached in the syntax class, and the `...` gathers one per clause.
- We flatten their `pieces` into a single list (`flat`).
- We check whether any clause had a binding (`has-bind?`).
- Finally, we splice the clauses into a `for*/list` expression.

`with-syntax` introduces new syntax bindings for use inside the expansion.
It's like `let` for syntax objects: `flat` is an ordinary run-of-the-mill list living at expansion time, and `with-syntax` is what lets us **drop it into a template**.

At this point, **the macro is complete!**

For the record, the file's header needs these:

```racket
#lang racket
(require racket/match
         (for-syntax racket/base syntax/parse racket/syntax))
```

`racket/match` at **run time** (the expansion uses `match` and `match-define`), and `syntax/parse` plus `racket/syntax` at **expansion time** (for `syntax-parse` and `generate-temporary`).

### Battle testing it

Let's use Racket's unit tests module to play around with it:

```racket
(module+ test
  (require rackunit)

  ;; simple
  (check-equal?
   (list-comp x [x <- (.. 1 5)] (even? x))
   '(2 4))
  ;; pairs
  (check-equal?
   (list-comp (list x y) [x <- '(1 2)] [y <- '(3 4)])
   '((1 3) (1 4) (2 3) (2 4)))
  ;; variables
  (check-equal?
   (list-comp x [x <- (.. 1 10)] [let ([num 5]) (= num x)])
   '(5))
  ;; only the first part of a pair
  (check-equal?
   (list-comp a [(list a b) <- '((1 5) (4 2))] (= (+ a b) 6))
   '(1 4))
  ;; constants
  (check-equal?
   (list-comp 42 (< 1 2))
   '(42))
  ;; descending range
  (check-equal?
   (list-comp n [n <- (.. 10 2 -2)])
   '(10 8 6 4 2)))
```

And the result:

```text
❯ raco test list-comp.rkt
raco test: (submod (file "list-comp.rkt") test)
6 tests passed
```

Notice the second test: **two generators** nest exactly like they do in Haskell, and we never wrote a single line of nesting logic.
That came **for free** from expanding into `for*/list`.

### Going further

I hope this showcase can be useful to better understand programming paradigms and the power of homoiconic languages.

As an exercise you could try to extend this rule to add an `unless` guard:

```racket
(list-comp x
  [x <- (.. 1 10)]
  [unless (even? x)])
; -> '(1 3 5 7 9)
```

First you would need to declare it into the `#:datum-literals`, and then add a `pattern`.

Remember the ordering rule from before: the new pattern has to sit **above** the bare-expression guard, or it will never be reached.

Have fun trying!

---
title: 'One lang to rule them all: Part I - OOP'
description: ''
pubDate: 'Oct 09 2025'
heroImage: '../../assets/racket.png'
---

Recently, I’ve been teaching **Programming Paradigms** at my university.
The course is designed to give students a broad view of the three most influential paradigms in modern programming.

For each paradigm, we use a language that best represents its philosophy:

- 📦 **Object-Oriented Programming** with **Smalltalk Pharo**
- ➗ **Functional Programming** with **Haskell**
- 🧠 **Logic Programming** with **Prolog**

<img src="/headplosion.svg" alt="headplosion" style="float: left; margin-right: 1em; width: 12em;" />

Learning the syntax of different languages is valuable, but it comes with a challenge.
Students are not only switching between **interfaces and syntaxes**, they are also absorbing **entirely new ways of thinking** about programming.
And they must do this repeatedly throughout the four months of the course.

One idea has been floating around for years, but it was never taken seriously:

> **What if we used a single language that supports all three paradigms natively?** 🤔

The difficulty lies in finding such a language while still **preserving the essence** of each paradigm.
At first, this seems impossible.
Yet there is a solution close at hand.
It could even open the door to a **fourth paradigm** at the same time.

💡 That **solution** is

## The power of LISP

<img src="/Lisp_logo.svg" alt="kitty logo" style="float: right; margin-left: 1em; width: 10em;" />

**Lisp** (short for "**Lis**t **P**rocessing") is a **family of programming languages** sparked from **John McCarthy**’s research on **symbolic computation** in 1958.
The original implementation is the **second oldest** high-level language, closely after **Fortran**.

[Paul Graham](https://en.wikipedia.org/wiki/Paul_Graham_(programmer)) identified nine **important aspects** of Lisp that **distinguished** it from existing languages like **Fortran**. The **most relevant** for our adventure now are:

- **First-class functions**, that can be **passed** as arguments, **returned** from a function, and **stored** in variables.
- **Recursion**, meaning functions can **call themselves** to solve problems.
- **Programs made entirely of expressions** with no statements.
- A **symbol data type**, distinct from the string data type.
- Notation for **code as trees of symbols**, with **S-expression** using numerous **parentheses**.
- **Full language functionality** available at **load**, **compile** and **run time**.

For completion, the other historical ones are:

- **Conditionals beyond `goto`**, such as `if` or `cond`.
- **Variables treated as references** (pointers), with **types determined by value**.
- **Garbage collection** allowing **automatic memory management**.

Over time, a language as old as Lisp has produced many **dialects** and **variants**.
Each took its own approach while **preserving** the **core essence** of their common ancestor.

<img src="/Clojure_logo.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 10em;" />

Today, the most **widely used** Lisp is likely **Clojure**.
Because it runs on the **JVM**, it can tap into the **Java ecosystem**, and its community strives to align with **functional programming** principles to enhance stability.

Although Clojure is my favorite Lisp, our focus here will be placed on **Racket**.
Originating from the **Scheme** dialect, it was built as a platform for **programming language design** and **implementation**, which is why many describe it as the **Programmable Programming Language**.
The meaning of this claim will become clear later, trust me.

### Racket

Generally, Lisp code is made of **S-expressions** (or **symbolic expressions**), which are either **atoms** (numbers, strings, booleans, symbols, etc.) or **lists** (parenthesized sequences of S-expressions).

```racket
#lang racket

;; atoms
42
"hi"
#t
'a-symbol-literal

;; lists
(1 2 3)
("a" "b" "c")
(a b (c d) 5)
```

Under the hood, lists are **chains of pairs** (a.k.a. **`cons` cells**) like **linked lists**:

```racket
(cons 1 (cons 2 (cons 3 '())))  ; the same as '(1 2 3)
```

Because lists are built from pairs, you can retrieve the **first** element (the "head") with `car`, and the **rest** with `cdr`.
These operators are often called `first`/`rest` or `head`/`tail` in other high-level languages.

```racket
(car '(1 2 3))  ; -> 1
(cdr '(1 2 3))  ; -> '(2 3)
```

**Most code is written as S-expressions**, a list where the **first** element is the **operator** and the **rest** are **arguments**, like a **procedure application**.

```racket
(+ 1 2 3)                    ; apply + to 1, 2 and 3 -> 6
(string-append "ra" "cket")  ; -> "racket"
```

Not every list denotes a procedure call: some begin with a **keyword** (a **special form**) that has its own evaluation rules. For example:

```racket
(define x 10)         ; binding form (special form), not a function call
(lambda (n) (+ n 1))  ; creates an anonymous function (special form)
```

<details>
<summary>Because code is made of expressions, you can nest it, forming something resembling an AST (Abstract Syntax Tree):
</summary>

![](../../assets/Corrected_S-expression_tree_2.svg)
</details>

```racket
(* 2 (+ 3 4))  ; -> 14
```

When you prefix an expression with a **quote** (`'`), Racket doesn't evaluate it, instead it treats it as **literal data**.
Notice that although the surface **syntax is identical** to runnable code, the leading **quote** changes only the **interpretation**, treating the expression as data rather than something to execute.

```racket
'(+ 1 2 3)      ; a list of data: the symbol '+ and numbers 1 2 3
'(define x 10)  ; data describing some code, but not executed
```

You can inspect the data structure programmatically:

```racket
(list? '(+ 1 2 3))        ; is this a list? -> #t
(symbol? (car '(+ 1 2)))  ; is the first element the symbol '+? -> #t
```

In summary, here’s what **code vs data** look like:

```racket
#lang racket

;; data
'(+ 1 (* 2 3))  ; -> (list '+ 1 (list '* 2 3))

;; code
(+ 1 (* 2 3))  ; -> 7
```

Finally, the **first line** `#lang racket` is the **language declaration**: it tells Racket how to read, expand, and run the rest of the file.
Different `#lang`s provide different surface syntaxes and libraries.
Here we are using the **full Racket language**, but later we will pick another flavour of Racket that better suit the paradigm used.

## Object-Oriented Programming

Our domain is going to be about a vehicle dealership.

<img src="/OOP.drawio.svg" alt="kitty logo" style="width: 100%;" />

As illustrated above, we have a base class `Bike` with two subclasses: `Scooter` and `Electric`.
The computation of the final price is left to be implemented by the child classes as:

- Scooter: basePrice * coefficient
- Electric: batteryType lithium -> 250.000, lead -> 120.000, other -> 500.000

We also have a `Dealership` class that will contain a collection of bikes.
Here we will define the responsibilities of returning the brand of the first bike that has a lithium battery, and the quantity of scooters whose basePrice is between two values.

### Pharo

Using Pharo, we should begin implementing the `Bike` class, since it's the only one with no dependencies:

```smalltalk
Object << #Bike
    slots: { #brand . #origin };
    package: 'Vehicles'

Bike >> brand
   ^ brand

Bike >> brand: aString
   brand := aString

Bike >> origin
   ^ origin

Bike >> origin: aString
   origin := aString

Bike >> finalPrice
   ^ self subclassResponsibility

Bike >> initialize
   super initialize.
   brand := ''.
   origin := ''.

Bike class >> brand: aString origin: anotherString
   ^ self new
      brand: aString;
      origin: anotherString;
      yourself
```

Fortunately, it's simple to translate this code into Racket:

```racket
(define Moto<%>
  (interface ()
    get-marca
    get-origen
    precio-final))

(define Moto%
  (class* object% (Moto<%>)
    (init-field [marca "Yamaha"] [origen "Japan"])

    (super-new)

    (abstract precio-final)

    (define/public (get-marca) marca)

    (define/public (get-origen) origen)))
```

It's not necessary, but for completeness sake I implemented the interface.
The `<%>` and `%` at the end of the declarations are naming conventions for interfaces and classes respectively.

<img src="/Racket-logo.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 10em;" />

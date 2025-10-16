---
title: 'One lang to rule them all: Part I - OOP'
description: ''
pubDate: 'Oct 09 2025'
heroImage: '../../assets/racket.png'
---

<img src="/Racket-logo.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 10em;" />

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

Let's use an old exam to illustrate the migration from one language to the other:

<img src="/OOP.drawio.svg" alt="kitty logo" style="width: 100%;" />

We’ll start with a simple hierarchy of bikes:

- A base class `Bike`.
- Two subclasses:
  - `Scooter`, where the final price is the base price multiplied by a coefficient.
  - `Electric`, where the final price depends on the battery type:
    - `250,000` for lithium batteries
    - `120,000` for lead batteries
    - `500,000` for anything else

We also define a `Dealership` class, which contains a collection of bikes and adds some business logic:

- Get the brand of the first bike with a lithium battery.
- Count the number of scooters whose base price is within a given range.

### Defining the Base Class

We should begin implementing the `Bike` class, since it's the only one with no dependencies.

```smalltalk
Object << #Bike
  slots: { #brand . #origin };
  package: 'Vehicles'

Bike >> initialize
  super initialize.
  brand := 'Yamaha'.
  origin := 'Japan'

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
```

Here we define two attributes (`brand`, `origin`) and a placeholder method `finalPrice`, marked abstract using `subclassResponsibility`, whose implementation is left to subclasses.

In Racket, we usually start by declaring an interface.

```racket
(define Bike<%>
  (interface ()
    get-brand
    set-brand!
    get-origin
    set-origin!
    get-final-price))
```

- The `<%>` suffix indicates an interface.
- Methods ending in `!` are mutators (state-modifying), a strong Racket convention.

Now the class itself (by convention they end in `%`):

```racket
(define Bike%
  (class* object% (Bike<%>)
    (init-field [brand "Yamaha"] [origin "Japan"])

    (super-new)

    (define/public (get-brand) brand)
    (define/public (set-brand! b) (set! brand b))

    (define/public (get-origin) origin)
    (define/public (set-origin! o) (set! origin o))

    (abstract get-final-price)))
```

- `class*` takes a parent class (`object%`) and a list of interfaces (`Bike<%>`).
- `init-field` declares fields with optional default values.
- `define/public` declares public methods (our getters and setters).
- `abstract` marks methods that must be implemented in subclasses.

### Defining subclasses

In Racket, we don’t need an interface here, just inherit from `Bike%`:

```racket
(define Scooter%
  (class Bike%
    (init-field [base-price 0] [coefficient 1.0])

    (super-new)

    (define/public (get-base-price) base-price)
    (define/public (set-base-price! b) (set! base-price b))

    (define/public (get-coefficient) coefficient)
    (define/public (set-coefficient! c) (set! coefficient c))

    (define/override (get-final-price) (* base-price coefficient))))  ; final price = base × coefficient
```

- We use `class` (not `class*`), since no interfaces are added.
- `define/override` implements the abstract method from `Bike%`.

---

The `Electric` subclass differs only in how it calculates the price:

```racket
(define Electric%
  (class Bike%
    (init-field [battery-type 'lithium])

    (super-new)

    (define/public (get-battery-type) battery-type)
    (define/public (set-battery-type! b) (set! battery-type b))

    (define/override (get-final-price)
      (case battery-type
        ['lithium 250000]
        ['lead 120000]
        [else 500000]))))
```

Here we use `case`, a convenient pattern-matching form.

### The `Dealership` Class

First let's tackle the structure of our `Dealership` class.

```smalltalk
Object << #Dealership
  slots: { #name . #bikes };
  package: 'Vehicles'

Dealership >> initialize
  super initialize.
  name := 'BikeWorld'.
  bikes := OrderedCollection new

Dealership >> addBike: aBike
  bikes add: aBike
```

We represent the collection of bikes as a list.
Adding a bike is done with `cons`, which adds an element to the front of a list.

```racket
(define Dealership%
  (class object%
    (init-field [name "BikeWorld"] [bikes '()])

    (super-new)

    (define/public (get-bikes) bikes)
    (define/public (add-bike bike) (set! bikes (cons bike bikes)))))
```

#### Intermission (How Racket Handles Message Passing)

Before diving into the business logic, let's pause to understand how Racket handles **message passing**.

In Racket’s class system, you invoke a method on an object with `send`:

```racket
(send <object> <method-name> arg ...)
```

So in our domain we could write something like this:

```racket
(send d add-bike (new Scooter% [brand "Vespa"] [base-price 1000] [coefficient 1.2]))
(send bike get-final-price)
```

We can also use `send` with `this` (the current instance) to call another method of the same object, or `super` to call the superclass's version:

```racket
(define/public (price-with-tax)
  (define price (send this get-final-price))
  (* price 1.21))
```

#### Get the brand of the first bike with a lithium battery

In Pharo, this method finds the first electric bike with a lithium battery and returns its brand.

```smalltalk
Dealership >> getBrandFirstLithium
  ^ bikes
    detect: [ :bike |
      (bike isKindOf: Electric)
        and: [ bike batteryType = 'lithium' ] ]
    ifFound: [ :bike | bike brand ]
    ifNone: []
```

To migrate this to Racket we use the `for/first` form, which iterates over a list and returns the first value for which the body is not `#f`.
`is-a?` checks class membership to avoid calling the wrong method at runtime.

```racket
(define/public (get-brand-first-lithium)
  (for/first ([bike (in-list bikes)]
              #:when (and (is-a? bike Electric%) (eq? (send bike get-battery-type) 'lithium)))
    (send bike get-brand)))
```

#### Count scooters within a price range

Finding this value in Pharo reads almost like English.
We select all the bikes with a base price between two values, and then we retrieve the size of the resulting collection.

```smalltalk
Dealership >> countScootersBetween: minimum and: maximum
  ^ bikes
    select: [ :bike |
      (bike isKindOf: Scooter)
        and: [ bike basePrice between: minPrice and: maxPrice ] ]
    size
```

In Racket, we can use `for/sum` to iterate over the list and sum up `1` for each bike that meets the criteria.

```racket
(define/public (count-scooters-between minimum maximum)
  (for/sum
    ([bike (in-list bikes)])
    (if (and (is-a? bike Scooter%) (<= minimum (send bike get-base-price) maximum)) 1 0)))
```

### Putting it all together

Here's a quick demo:

```racket
(define d (new Dealership%))

(send d add-bike (new Scooter% [base-price 50000] [coefficient 1.2]))
(send d add-bike (new Electric% [brand "Tesla"] [battery-type 'lithium]))

(displayln (send d get-brand-first-lithium))  ; "Tesla"
(displayln (send d count-scooters-between 40000 60000))  ; 1
```

Despite different syntax, idioms and conventions, we can appreciate many OOP concepts both in Racket and Pharo.

The main pain point here is losing how close to natural language Smalltalk reads, but the concepts are surprisingly close.

## Logic Programming

Here I created a small use case to showcase Logic Programming.

This is the **courses** table:

| Code | Name                    | Capacity | Hours | Category                         |
|------|-------------------------|----------|-------|----------------------------------|
| C101 | Programming 1           | 80       | 6     | [Mandatory, Development]         |
| C102 | Programming Paradigms   | 50       | 4     | [Mandatory, Development]         |
| C103 | Artificial Intelligence | 15       | 3000  | [Elective, Development, Science] |
| C104 | Computer Networks       | 1        | 800   | [Mandatory, Support]             |

Here we got the **rooms** table:

| Code  | Name            | Capacity |
|-------|-----------------|----------|
| R1    | Main Hall       | 120      |
| R2    | Laboratory 1    | 40       |
| R3    | Laboratory 2    | 35       |
| R4    | Conference Room | 60       |

And finally, this is the **dictations** table:

| Code | Room |
|------|------|
| 1    | R3   |
| 2    | R2   |
| 3    | R4   |
| 4    | R1   |

If we were to translate this to **Prolog**, we would have:

```prolog
% curso(codigoCurso, nombre, cupo, horasSemanales, categorias)
curso("C101", "Programacion I", 80, 6, ["Obligatoria", "Desarrollo"]).
curso("C102", "Paradigmas de Programacion", 50, 4, ["Obligatoria", "Desarrollo"]).
curso("C103", "Inteligencia Artificial", 30, 5, ["Electiva", "Ciencia", "Desarrollo"]).
curso("C104", "Redes de Computadoras", 40, 5, ["Obligatoria", "Soporte"]).

% aula(codigoAula, nombreAula, capacidad)
aula(1,"Aula Magna",120).
aula(2,"Laboratorio 1",40).
aula(3,"Laboratorio 2",35).
aula(4,"Sala de Conferencias",60).

% Relación curso-aula (cada curso se dicta en un aula determinada)
dictado("C101",1).
dictado("C102",2).
dictado("C103",4).
dictado("C104",3).
```

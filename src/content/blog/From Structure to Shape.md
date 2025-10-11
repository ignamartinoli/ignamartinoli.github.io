---
title: 'From Structure to Shape'
description: 'A new way of seeing code'
pubDate: 'Aug 25 2025'
heroImage: '../../assets/Shutterstock_1316477501-2048x1366.png'
---

The **words we use** shape the way we **think**.
This is known as the **Sapir-Whorf hypothesis** among linguists.
The **structure of a language channels thought** rather than merely reflecting it.

In *The Humble Programmer*, Edsger W. Dijkstra encapsulated a similar concept regarding **tools**:

> *"The tools we use have a profound (and devious!) influence on our thinking habits, and, therefore, on our thinking abilities."*

The **paradigms** and **abstractions** we select in **programming** influence the **strategies** we use and the **solutions** we can envision, much like the **grammar** and **vocabulary** of a **language** influence our **perception** and **reasoning**.
The frameworks we adopt subtly **channel the way we approach and tackle problems**, often in ways we only notice when we step outside them.

In this way, by examining how **each particular paradigm interprets the same concept** we can see how altering the way our programs are organized can result in **more straightforward, sophisticated, or powerful solutions**.

<!-- TODO: insert image -->

A clear example is **polymorphism**, where a single **interface** can flexibly express many different **behaviors** depending on **context**.

## Understanding Polymorphism

<img src="/python_logo_icon_168886.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 8em;" />

Normally, the concept of **polymorphism** is introduced in the context of **object-oriented programming (OOP)**, where it describes the ability of **different objects** to respond differently to the **same method call** following **class hierarchies**.

For instance, even though a `draw` method may be defined in a `Shape` interface, calling `draw()` on a `Circle` or a `Rectangle` results in **completely different behaviour**:

```python
class Shape:
    def area(self):
        pass

class Circle(Shape):
    def __init__(self, r):
        self.r = r
    def area(self):
        return 3.14159 * self.r**2

class Rectangle(Shape):
    def __init__(self, w, h):
        self.w, self.h = w, h
    def area(self):
        return self.w * self.h

shapes = [Circle(2), Rectangle(3,4)]
for s in shapes:
    print(s.area())
````

In this example, the polymorphism arises from the **inheritance hierarchy**: the interface `area` is the same, but the **concrete implementation depends on the runtime type** of the object.

<!-- TODO: insert a class diagram -->

This particular form of polymorphism, called **subtype polymorphism** or **inheritance polymorphism**, is familiar to many programmers. It demonstrates how a **single interface** can abstract over **several concrete types**.

---

However, **class hierarchies** are not the only context in which polymorphism occurs

<img src="/Haskell-Logo.svg" alt="kitty logo" style="float: right; margin-left: 1em; width: 8em;" />

Inheritance is not even required with **ad-hoc polymorphism**; instead, a function will behave differently based on the **type of its argument**.

**Type-class** languages, such as **Haskell**, serve as examples of this approach:

```haskell
class Shape a where
    area :: a -> Double

data Circle = Circle Double -- newtype
data Rectangle = Rectangle Double Double

instance Shape Circle where
    area (Circle r) = pi * r^2

instance Shape Rectangle where
    area (Rectangle w h) = w * h

main = do
    print . area $ Circle 2
    print . area $ Rectangle 3 4
```

In this case, `area` behaves differently for `Circle` and `Rectangle`, yet **no class hierarchy or runtime type checks** are needed.
The behavior is **directly linked to the type** itself.

This method extends the idea **beyond runtime type dispatch** by demonstrating that polymorphism can **exist outside of the OOP concept**.

---

<img src="/Julia_Programming_Language.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 8em;" />

**Multiple dispatch** builds on this idea further by choosing the implementation based on the **types of all the arguments**, which is a more general form of ad-hoc polymorphism.

Languages like **Julia** make this approach elegant and natural:

<!-- TODO: create a more fun example with multiple arguments -->

```julia
struct Circle
    radius::Float64
end

struct Rectangle
    width::Float64
    height::Float64
end

function area(c::Circle)
    π * c.radius^2
end

function area(r::Rectangle)
    r.width * r.height
end

struct Circle; r::Int; end
struct Square; s::Int; end

intersect(a::Circle, b::Circle) = "circle-circle"
intersect(a::Circle, b::Square) = "circle-square"

intersect(Circle(3), Square(4))  # "circle-square"

```

When calling `area`, **Julia automatically dispatches** the call to the right method based on the type of the arguments.
Again, unlike OOP polymorphism, multiple dispatch **does not rely on inheritance**.

### An unexpected generalization

All those examples demonstrate that polymorphism is fundamentally about **adapting behavior to the form of data**, rather than adhering to a particular class structure.

<img src="/APL_(programming_language)_logo.svg" alt="kitty logo" style="float: right; margin-left: 1em; width: 10em;" />

**Rank polymorphism**, which is common in **array-oriented languages** like **APL**, **J**, or **NumPy**, takes this principle one step further.

Instead of adapting to types, it adapts to the **dimensionality or rank of data structures**.
Operations like **addition or multiplication** seamlessly **work on scalars, vectors, and matrices** without explicit overloading, **generalizing the concept** of polymorphism **from types to shapes**.

## A (family of) Programming Languages

**APL**, which stands for "**A** **P**rogramming **L**anguage", was the first **array-oriented programming language**, created by **Kenneth Iverson** in the 1960s, based on his **mathematical notation**.

It reflects Iverson’s main idea of **notation as a tool of thought**. He believed that programming languages should not only tell machines what to do, but also **help people think** and express ideas **clearly and mathematically**.

Because APL was both a **language** and a **philosophy**, it inspired many dialects as developers adapted it to different goals and platforms.
In this post, we will use what can be seen as APL's spiritual successor: **BQN**.

Created by **Marshall Lochbaum**, this modern array language builds on APL’s ideas with clearer, more **regular syntax**, **functional programming support**, namespaces and structured error handling, while staying true to Iverson's vision.

## Getting our hands dirty

Promethee problem

## Conclusion

Both capture the same point: the programming language you use doesn’t just let you express solutions — it also **shapes how you conceive problems and what solutions you even notice are possible**.

  | Type                      | How Dispatch Happens         | Example Language |
  | ------------------------- | ---------------------------- | ---------------- |
  | **Subtype / Inheritance** | Runtime type of object       | Python, Java     |
  | **Ad-Hoc / Type-based**   | Compile-time type resolution | Haskell          |
  | **Multiple Dispatch**     | Types of all arguments       | Julia            |
  | **Rank / Shape-based**    | Shape or dimensionality      | APL              |

> **"A language that doesn’t affect the way you think about programming, is not worth knowing."**
>
> -- *Alan Perlin*, first recipient of the Turing Award

## Translation to other domains

```python
print({'LLNNNLL':'Argentina','LLLNLNN':'Brasil','LLNNNN':'Bolivia','LLLLNNN':'Paraguay','LLLNNNN':'Uruguay'}.get(''.join('L' if x.isalpha() else 'N' for x in input())))
```

```bqn
"ERROR" ("LLNNNLL"‿"LLLNLNN"‿"LLNNNN"‿"LLLLNNN"‿"LLLNNNN" •HashMap "Argentina"‿"Brasil"‿"Bolivia"‿"Paraguay"‿"Uruguay").Get "LN" ⊏˜ input ∊ '0' + ↕10
```

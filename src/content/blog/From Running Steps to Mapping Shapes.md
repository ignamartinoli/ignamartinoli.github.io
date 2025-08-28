---
title: 'From Counting Steps to Mapping Shapes'
description: 'A new way of seeing code'
pubDate: 'Aug 25 2025'
heroImage: '../../assets/glassmorphic_matrix.webp'
---

The **words we use** shape the way we **think**.
Linguists call this the **Sapir-Whorf hypothesis**.
The **structure of language** does not just reflect thought, it **channels it**.

Edsger W. Dijkstra captured a similar idea about **tools** in *The Humble Programmer*:

> *"The tools we use have a profound (and devious!) influence on our thinking habits, and, therefore, on our thinking abilities."*

Just as the **grammar** and **vocabulary** of a **language** guide our **perception** and **reasoning**, the **paradigms** and **abstractions** we choose in **programming** shape the **strategies** we employ and the **solutions** we can envision.
The frameworks we adopt subtly **channel the way we approach and tackle problems**, often in ways we only notice when we step outside them.
<!-- NOTE: maybe change to "often without our noticing." -->

A clear example is **polymorphism**, where a single **interface** can flexibly express many different **behaviors** depending on **context**.
Exploring how a **specific paradigm interprets this concept**, we can see how changing the way we structure our programs can lead to **simpler, more elegant, or more powerful solutions**.

## Understanding Polymorphism

<img src="/python_logo_icon_168886.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 10em;" />

**Polymorphism** is commonly introduced in the context of **object-oriented programming (OOP)**, where it describes the ability of **different objects** to respond differently to the **same method call** following **class hierarchies**.

For example, a `Shape` interface may define a `draw` method, but invoking `draw()` on a `Circle` or a `Rectangle` executes **completely different behavior**:

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
    print(s.area())  # Dispatch based on object type
````

In this example, the polymorphism arises from the **inheritance hierarchy**: the interface `area` is the same, but the **concrete implementation depends on the runtime type** of the object.

This form of polymorphism, called **subtype polymorphism** or **inheritance polymorphism**, is familiar to many programmers. It illustrates how a **single interface** can abstract over **multiple concrete types**.

---

However, polymorphism is **not limited to class hierarchies**.

<img src="/Haskell-Logo.svg" alt="kitty logo" style="float: right; margin-left: 1em; width: 10em;" />

In **ad-hoc polymorphism**, a function can behave differently depending on the **type of its argument**, without requiring inheritance.
Languages with **type classes**, like **Haskell**, demonstrate this approach:

```haskell
class Shape a where
    area :: a -> Double

newtype Circle = Circle Double
data Rectangle = Rectangle Double Double

instance Shape Circle where
    area (Circle r) = pi * r^2

instance Shape Rectangle where
    area (Rectangle w h) = w * h

main = do
    print . area $ Circle 2
    print . area $ Rectangle 3 4
```

Here, `area` behaves differently for `Circle` and `Rectangle`, yet **no class hierarchy or runtime type checks** are needed.
The behavior is attached **directly to the type** itself.

This approach shows that polymorphism can **exist outside OOP**, expanding the concept beyond **runtime type dispatch**.

---

<img src="/Julia_Programming_Language.svg" alt="kitty logo" style="float: left; margin-right: 1em; width: 10em;" />

Taking this idea further, **Multiple Dispatch** generalizes ad-hoc polymorphism by selecting the implementation based on the **types of all arguments**.

Languages like **Julia** make this approach elegant and natural

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
```

When calling `area`, **Julia automatically dispatches** to the appropriate method depending on the argument type.
Unlike OOP polymorphism, multiple dispatch **does not rely on inheritance**.

### Beyond the concept (outside our the comfort zone)

All those examples demonstrate that polymorphism is fundamentally about **adapting behavior to the form of data**, rather than adhering to a particular class structure.

<img src="/APL_(programming_language)_logo.svg" alt="kitty logo" style="float: right; margin-left: 1em; width: 10em;" />

**Rank polymorphism**, which is common in **array-oriented languages** like **APL**, **J**, or **NumPy**, takes this principle one step further.

Instead of adapting to types, it adapts to the **dimensionality or rank of data structures**.
Operations like **addition or multiplication** seamlessly **work on scalars, vectors, and matrices** without explicit overloading, **generalizing the concept** of polymorphism **from types to shapes**.

## APL

### COMING NEXT...

<!--
### BQN

BQN is an APL dialect by Marshall Lockbaum that was designed with the objective.

## Problem in BQN

TODO

## Translation to other domains

```python
print({'LLNNNLL':'Argentina','LLLNLNN':'Brasil','LLNNNN':'Bolivia','LLLLNNN':'Paraguay','LLLNNNN':'Uruguay'}.get(''.join('L' if x.isalpha() else 'N' for x in input())))
```

## Conclusion

Both capture the same point: the programming language you use doesn’t just let you express solutions — it also **shapes how you conceive problems and what solutions you even notice are possible**.


  | Type                      | How Dispatch Happens         | Example Language |
  | ------------------------- | ---------------------------- | ---------------- |
  | **Subtype / Inheritance** | Runtime type of object       | Python, Java     |
  | **Ad-Hoc / Type-based**   | Compile-time type resolution | Haskell          |
  | **Multiple Dispatch**     | Types of all arguments       | Julia            |
  | **Rank / Shape-based**    | Shape or dimensionality      | NumPy, APL       |

> **"A language that doesn’t affect the way you think about programming, is not worth knowing."**
>
> -- *Alan Perlin*, first recipient of the Turing Award
-->

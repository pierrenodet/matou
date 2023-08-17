---
layout: page
---

The core component of matou is the `Matrix[M, N, A]` where `M` and `N` represent the number of rows and columns of the matrix and `A` the type of the matrix components.

## Creating matrices

To create matrices, use all methods provided in `matou.Matrix` companion object.

```scala
import matou.*

// Identity matrix of shape 2,2
val id2 = Matrix.identity[2, Double]

// Range matrix of shape 2,4
val range = Matrix.tabulate[2, 4, Double](i => i)

// Create a matrix from values
val block = Matrix((1, 2), (3, 4))

// Create a block matrix from blocks
val blocks = Matrix((block, block), (block, block))

// Matrices concatenation
val concat = (block.join(block)).fork(block.join(block))
```

## Generic operations

Key methods have been implemented to manipulate matrices in a typesafe manner:

- `scalar` operator to apply a function `f: A => B` to each matrix element (map),
- `hadamard` operator to apply a function `f: (A, B) => C` to each element of two matrices (map2, zip),
- `kronecker` (with `khatrirao` and `facesplitting`) operator to apply a function `f: (A, Matrix) => Matrix` to each element of a first matrix with a second matrix (flatMap, flatten),
- `matmul` operator to compose matrices (andThen, compose),
- `directsum` (with `hcat` and `vcat`) to build new matrices.

For each of these operators, the `mul`, `add` and `zero` can be provided accordingly to the canonical operations of the field of the matrix elements.

See [[matou.Matrix]] for the complete API.

## get, update, and slices

To retrieve elements or make views from matrices, two methods are available, the typesafe or unsafe operation:

```scala sc-name:<GetUpdateSlice.scala>
import matou.*

val a = Matrix.tabulate[2, 2, Int](i => i)

// Will get the first element of the matrix given the usual linear indexing and throw a compile-time error if the required element is out of bound.
a[0]

// Will throw a runtime error if out of bounds.
a(0)
```

For exemple, the following snippet will throw a compile-time error:

```scala sc-compile-with:<GetUpdateSlice.scala> sc:fail
a[4]
```

For slices, a convenient syntax is available thanks to an import:

```scala sc-compile-with:<GetUpdateSlice.scala>
import matou.Slice.*
import matou.Bound.*

// Will select the first row of the matrix and all columns
a.slice[0 :: 0, :::]

// Equivalent syntax
a.slice[0 :: 0, ** :: **]
```

## Math operators

All usual matrix operators commonly used in linear algebra are provided with a unicode syntax to make your code look like your latex.

See [[matou.MathOps]] for all operators.

## Broadcasting

Broadcasting is implemented in terms of a typeclass `Broadcast[F,G]{type H}` where `F`, `G`, and `H` are higher kinded type representing matrices with a given shape. `F` and `G` represent the two shapes of the input matrices, and `H` represents the shape of the output matrix.

Especially, instances of this typeclass represent the different rules of broadcasting:
* binary functions can be applied to matrices of the same shape
* matrices of different shapes can be streched
* matrices of different dimensions can be padded

In our case, instead of the multi-dimensional array case, the third rule is only used to broadcast scalars to matrices.

All broadcasted operators are prepended with a colon `:` to emphasize that the operator rules are less strict (on the matrices shape) than the traditional ones.

```scala
import matou.BroadcastOps.*
import matou.*

val r = Matrix.tabulate[4, 5, Int](i => i)

scalar(2) :* r :== r :* scalar(2)
```

## Lifting functions to matrices

Lifting usual scala functions to a matrix representation is possible under the condition that the input and output types are `Enumerable`.

```scala
trait Enumerable[A]:
  type C <: Int
  def elements: IndexedSeq[A]
```

The type member `C` represents the cardinality of an enumerable set at the type level, which allows to statically computes the shape of the matrix representation of a lifted function.

```scala
//{
import matou.Enumerable
import matou.Matrix
//}
inline def lift[A, B, CA <: Int, CB <: Int](f: A => B)(using
    Enumerable.Aux[A, CA],
    Enumerable.Aux[B, CB]
): Matrix[CA, CB, Double] = ???
```

Then the matrix is filled with ones and zeros accordingly to `f`.

Some instances of `Enumerable` are already implemented (such as for tuples of enumerable, functions from and to enumerable, ...), and by using the right import, `matou` allows you to do this:

```scala
import matou.Enumerable.*

// Monty Hall problem
case object Win
case object Lose

type Outcome = Either[Win.type, Lose.type]

// Switching doors after one losing door has been revealed
def switch(outcome: Outcome) =
  outcome match
    case Left(Win)   => Right(Lose)
    case Right(Lose) => Left(Win)

lift(switch)
```

## Category of matrices

TODO

See [[matou.CategoryOps]] for all operators.

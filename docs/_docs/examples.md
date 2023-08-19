---
layout: page
---

Some examples have been implemented in [matou.examples](https://github.com/pierrenodet/matou/tree/main/examples/src/main/scala/matou/examples)

## Conway's Game of Life

Here is a way to compute the next iteration of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) using convolution:

```scala
//{
import matou.Matrix
import matou.MathOps.*
import scala.util.Random
//}
val cells = Matrix.fill[1000, 1000, Int](Random.nextInt(2))

val kernel = Matrix(
    (1, 1, 1),
    (1, 0, 1),
    (1, 1, 1)
  )

val neighborhood = cells ⋆ kernel

cells.hadamard(neighborhood)((cell, neighbors) =>
    if neighbors == 2 then cell
    else if neighbors == 3 then 1
    else 0
)
```

## Sum of Dice

Here are two ways to compute a sum of dice, one using [convolutions](https://en.wikipedia.org/wiki/Convolution_of_probability_distributions), the other using matrices composition:

```scala
case class BoundedNatural[L <: Int, U <: Int](value: Int)

//{
import scala.compiletime.ops.int.*
import matou.Enumerable

object BoundedNatural:
  extension [L1 <: Int, U1 <: Int](bn: BoundedNatural[L1, U1])
    def +[L2 <: Int, U2 <: Int](bn2: BoundedNatural[L2, U2]) =
      BoundedNatural[L1 + L2, U1 + U2](bn.value + bn2.value)

  given [L <: Int: ValueOf, U <: Int: ValueOf]: Enumerable[BoundedNatural[L, U]]
  with
    type C = U - L + 1
    def elements: IndexedSeq[BoundedNatural[L, U]] =
      (valueOf[L] to valueOf[U]).map(BoundedNatural[L, U](_))

//}
type Die[N <: Int] = BoundedNatural[1, N]

//{
import matou.CategoryOps.*
import matou.Enumerable.*
import matou.Vector
import matou.MathOps.*
import matou.BroadcastOps.*

//}
val d6 = Vector.fill[6, Double](1.0 / 6)
val `2d6` = (d6 &&& d6)

val sumOf2d6 = lift((d1: Die[6], d2: Die[6]) => d1 + d2) >>> `2d6`

sumOf2d6 :== (d6.pad[0, 2, 0, 3](0) ⋆ d6)
```

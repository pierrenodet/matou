---
layout: index
---

Exploring typesafe and composable matrices with Scala.

```scala
import matou.CategoryOps.*
import matou.Enumerable.*
import matou.Vector

// Monty Hall problem
case object Win
case object Lose

type Outcome = Either[Win.type, Lose.type]

// Choosing one winning door out of 3
val firstChoice = Vector(1.0 / 3, 2.0 / 3)

// Switching doors after one losing door has been revealed
def switch(outcome: Outcome) =
  outcome match
    case Left(Win)   => Right(Lose)
    case Right(Lose) => Left(Win)

val secondChoice = lift(switch)

secondChoice >>> firstChoice
```

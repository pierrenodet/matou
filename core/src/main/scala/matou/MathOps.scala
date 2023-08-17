/*
 * Copyright 2023 Pierre Nodet
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package matou

import scala.compiletime.ops.int.*

import math.Fractional.Implicits.infixFractionalOps

object MathOps:

  inline def transpose[M <: Dimension, N <: Dimension, A](
      a: Matrix[M, N, A]
  ) = a.t
  inline def vec[M <: Dimension, N <: Dimension, A](
      a: Matrix[M, N, A]
  ) = a.vectorize
  inline def diag[M <: Dimension, N <: Dimension, A](
      a: Matrix[M, N, A]
  )(using M =:= N) =
    a.diagonal

  inline def tr[M <: Dimension, N <: Dimension, A: Numeric](
      a: Matrix[M, N, A]
  )(using
      M =:= N
  ) =
    a.trace

  extension [M <: Dimension, N <: Dimension, A: Numeric](
      a: Matrix[M, N, A]
  )

    inline def unary_-               = a.map(summon[Numeric[A]].negate(_))
    inline def +(b: Matrix[M, N, A]) =
      a.map2(b)(summon[Numeric[A]].plus(_, _))
    inline def -(b: Matrix[M, N, A]) =
      a.map2(b)(summon[Numeric[A]].minus(_, _))
    inline def ⊙(b: Matrix[M, N, A]) =
      a.map2(b)(summon[Numeric[A]].times(_, _))

    inline def *[P <: Dimension](b: Matrix[N, P, A]) =
      a.matmul(b, None)(
        summon[Numeric[A]].times(_, _),
        summon[Numeric[A]].plus(_, _),
        Numeric[A].zero
      )
    inline def ∘[P <: Dimension](b: Matrix[P, M, A]) = b * a

    inline def ⋆[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    )(using P <= M =:= true, Q <= N =:= true) =
      a.convolve(b)(
        summon[Numeric[A]].times(_, _),
        summon[Numeric[A]].plus(_, _),
        summon[Numeric[A]].zero
      )

    inline def ⊗[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) =
      a.kronecker(b, None)(summon[Numeric[A]].times(_, _))
    inline def ⊕[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) =
      a.directsum(b)(identity, identity, summon[Numeric[A]].zero)

    inline def ∙[P <: Dimension, Q <: Dimension](
        b: Matrix[M, Q, A]
    ) =
      a.facesplitting(b, None)(summon[Numeric[A]].times(_, _))
    inline def ∗[P <: Dimension, Q <: Dimension](
        b: Matrix[P, N, A]
    ) =
      a.khatrirao(b, None)(summon[Numeric[A]].times(_, _))

    inline def +=(b: Matrix[M, N, A]) =
      a.map2InPlace(b)(summon[Numeric[A]].plus(_, _))
    inline def -=(b: Matrix[M, N, A]) =
      a.map2InPlace(b)(summon[Numeric[A]].minus(_, _))
    inline def ⊙=(b: Matrix[M, N, A]) =
      a.map2InPlace(b)(summon[Numeric[A]].times(_, _))

    inline def sum: A               = a.reduce(summon[Numeric[A]].plus(_, _))
    inline def trace(using M =:= N) = a.diagonal.sum

    inline def ⋅(b: Matrix[M, N, A]) =
      a.frobenius(b)(
        summon[Numeric[A]].times(_, _),
        summon[Numeric[A]].plus(_, _),
        summon[Numeric[A]].zero
      )

  end extension

  extension [
      M <: Dimension,
      N <: Dimension,
      A: Ordering
  ](
      a: Matrix[M, N, A]
  )

    inline def min: A = a.reduce((min, a) => summon[Ordering[A]].min(a, min))
    inline def max: A = a.reduce((max, a) => summon[Ordering[A]].max(a, min))

    inline def argmin: Int =
      a.fold((a[0], 0, 0)) { case ((min, argmin, count), a) =>
        if summon[Ordering[A]].lt(a, min) then (a, count, count + 1)
        else (min, argmin, count + 1)
      }._2
    inline def argmax: Int =
      a.fold((a[0], 0, 0)) { case ((max, argmax, count), a) =>
        if summon[Ordering[A]].gt(a, max) then (a, count, count + 1)
        else (max, argmax, count + 1)
      }._2

  end extension

  extension [M <: Dimension, N <: Dimension, A: Fractional](
      a: Matrix[M, N, A]
  )
    inline def ⊘(b: Matrix[M, N, A])  =
      a.map2(b)(summon[Numeric[A]].div(_, _))
    inline def ⊘=(b: Matrix[M, N, A]) =
      a.map2InPlace(b)(summon[Numeric[A]].div(_, _))

    inline def mean: A = a.sum / Fractional[A].fromInt(a.size)

  extension [M <: Dimension, N <: Dimension, A](
      a: Matrix[M, N, A]
  )
    inline def ===[Q <: Dimension](b: Matrix[M, Q, A]) = a.fork(b)
    inline def |||[P <: Dimension](b: Matrix[P, N, A]) = a.join(b)

end MathOps

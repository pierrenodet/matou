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

import Matrix.*
import MathOps.*
import compiletime.ops.int.*

object CategoryOps:

  inline def id[M <: Dimension, A: Numeric] = identity[M, A]

  inline def diagonal[M <: Dimension, A: Numeric] = id[M, A] &&& id[M, A]
  inline def Δ[M <: Dimension, A: Numeric]        = diagonal[M, A]

  inline def ir1[M <: Dimension, A: Numeric] = id[M, A] &&& ones[M, M, A]
  inline def ir2[M <: Dimension, A: Numeric] = ones[M, M, A] &&& id[M, A]

  inline def pr1[M <: Dimension, A: Numeric] = ir1[M, A].t
  inline def pr2[M <: Dimension, A: Numeric] = ir2[M, A].t

  inline def codiagonal[M <: Dimension, A: Numeric] = id[M, A] ||| id[M, A]
  inline def ∇[M <: Dimension, A: Numeric]          = codiagonal[M, A]

  inline def i1[M <: Dimension, A: Numeric] = id[M, A] === zeros[M, M, A]
  inline def i2[M <: Dimension, A: Numeric] = zeros[M, M, A] === id[M, A]

  inline def p1[M <: Dimension, A: Numeric] = i1[M, A].t
  inline def p2[M <: Dimension, A: Numeric] = i2[M, A].t

  extension [M <: Dimension, N <: Dimension, A: Numeric](
      a: Matrix[M, N, A]
  )

    // Compose, Category, Choice
    inline def andThen[P <: Dimension](b: Matrix[N, P, A]) =
      a.matmul(b, None)(
        summon[Numeric[A]].times(_, _),
        summon[Numeric[A]].plus(_, _),
        Numeric[A].zero
      )
    inline def <<<[P <: Dimension](b: Matrix[N, P, A])     =
      a.andThen(b)

    inline def compose[P <: Dimension](b: Matrix[P, M, A]) =
      b.andThen(a)
    inline def >>>[P <: Dimension](b: Matrix[P, M, A])     =
      a.compose(b)

    inline def choice[P <: Dimension](
        b: Matrix[P, N, A]
    ) = a.vcat(b)(a => a, b => b)

    // Arrow

    // lift : see enumerable

    inline def product[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) =
      a.kronecker(b, None)(summon[Numeric[A]].times(_, _))
    inline def split[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) = a.product(b)
    inline def ***[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) = a.product(b)

    // TODO: find a name for that
    inline def kek[P <: Dimension](b: Matrix[P, N, A]) =
      a.khatrirao(b)(summon[Numeric[A]].times(_, _))
    inline def ###[P <: Dimension](b: Matrix[P, N, A]) =
      a.kek(b)

    inline def merge[Q <: Dimension](
        b: Matrix[M, Q, A]
    ) = a.facesplitting(b)(summon[Numeric[A]].times(_, _))
    inline def &&&[Q <: Dimension](
        b: Matrix[M, Q, A]
    ) = a.merge(b)

    // ArrowChoice
    inline def coproduct[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) =
      a.directsum(b)(a => a, b => b, summon[Numeric[A]].zero)
    inline def choose[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) = a.coproduct(b)
    inline def +++[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    ) = a.coproduct(b)

    // Selective
    inline def select[P <: Dimension, Q <: Dimension](
        b: Matrix[P, Q, A]
    )(using P + Q =:= N) =
      (b ||| id) >>> a.reshape[M * N / (P + Q), P + Q]

  end extension

end CategoryOps

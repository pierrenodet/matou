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

import scala.compiletime.*
import scala.compiletime.ops.int.*

type Power[X <: Int, Y <: Int] <: Int = (X, Y) match
  case (X, 0)    => 1
  case (X, 1)    => X
  case (X, S[y]) => X * Power[X, y]

trait Enumerable[A]:
  type C <: Int
  def elements: IndexedSeq[A]

  transparent inline def cardinality: C = constValue[C]
  def apply(i: Int): A                  = elements(i)

object Enumerable:

  type Aux[A, C0] = Enumerable[A] { type C = C0 }

  transparent inline def apply[A](using e: Enumerable[A]) = e

  given [L, R, CL <: Int, CR <: Int](using
      Enumerable.Aux[L, CL],
      Enumerable.Aux[R, CR]
  ): Enumerable[Either[L, R]] with

    type C = CL + CR

    def elements: IndexedSeq[Either[L, R]] = Enumerable[L].elements
      .map(Left(_))
      .concat(Enumerable[R].elements.map(Right(_)))

  given [A, B, CA <: Int, CB <: Int](using
      Enumerable.Aux[A, CA],
      Enumerable.Aux[B, CB]
  ): Enumerable[A => B] with

    type C = Power[CA, CB]

    def elements: IndexedSeq[A => B] = List
      .fill(Enumerable[A].elements.size)(Enumerable[B].elements)
      .flatten
      .combinations(Enumerable[A].elements.size)
      .flatMap(_.permutations)
      .map(Enumerable[A].elements.zip(_).toMap)
      .toIndexedSeq

  given [H, T <: Tuple, CH <: Int, CT <: Int](using
      Enumerable.Aux[H, CH],
      Enumerable.Aux[T, CT]
  ): Enumerable[H *: T] with

    type C = CH * CT

    def elements =
      for
        h <- Enumerable[H].elements
        t <- Enumerable[T].elements
      yield (h *: t)

  given [A: ValueOf]: Enumerable[A] with
    type C = 1
    def elements = IndexedSeq(valueOf[A])

  given Enumerable[Unit] with
    type C = 1
    def elements = IndexedSeq(())

  given Enumerable[Nothing] with
    type C = 0
    def elements = IndexedSeq.empty

  inline def lift[A, B, CA <: Int, CB <: Int](f: A => B)(using
      Enumerable.Aux[A, CA],
      Enumerable.Aux[B, CB]
  ) =
    Matrix.tabulate[CA, CB, Double]((i, j) =>
      if Enumerable[B].elements(j) == f(Enumerable[A].elements(i)) then 1
      else 0
    )

  inline def lift[H, T <: Tuple, B, CA <: Int, CB <: Int](f: (H *: T) => B)(
      using
      Enumerable.Aux[H *: T, CA],
      Enumerable.Aux[B, CB]
  ) =
    Matrix.tabulate[CA, CB, Double]((i, j) =>
      if Enumerable[B].elements(j) == f(Enumerable[H *: T].elements(i)) then 1
      else 0
    )

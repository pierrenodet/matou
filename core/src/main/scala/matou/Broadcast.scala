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

type Scalar[A] = A
object Scalar:
  inline def apply[A](inline a: A): Scalar[A] = a

trait Broadcast[F[_], G[_]]:

  type H[_]

  transparent inline def broadcast[A, B, C](
      inline a: F[A],
      inline b: G[B],
      inline out: Option[H[C]] = None
  )(inline f: (A, B) => C): H[C]

  transparent inline def broadcastInPlace[A, B, C](
      inline a: F[A],
      inline b: G[B]
  )(
      inline f: (A, B) => C
  )(using ev: F[A] =:= H[C]) = broadcast(a, b, Some(ev(a)))(f)

class BroadcastExpand:

  inline given expandColVectorRight[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, N, A], [B] =>> Matrix[M, 1, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, N, A],
        inline b: Matrix[M, 1, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) =
      val c     = inline out match
        case Some(c) => c.asTransposed[false]
        case None    => Matrix.empty[M, N, C]
      val aiter = a.columns
      val citer = c.columns
      while aiter.hasNext && citer.hasNext do
        val aitem = aiter.next
        val citem = citer.next
        aitem.hadamard(b, Some(citem))(f)
      c

  inline given expandRowVectorRight[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, N, A], [B] =>> Matrix[1, N, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, N, A],
        inline b: Matrix[1, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) =

      val c     = inline out match
        case Some(c) => c.asTransposed[false]
        case None    => Matrix.empty[M, N, C]
      val aiter = a.rows
      val citer = c.rows
      while aiter.hasNext && citer.hasNext do
        val aitem = aiter.next
        val citem = citer.next
        aitem.hadamard(b, Some(citem))(f)
      c

  inline given expandRowVectorLeft[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[1, N, A], [B] =>> Matrix[M, N, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[1, N, A],
        inline b: Matrix[M, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) =
      val c     = inline out match
        case Some(c) => c.asTransposed[false]
        case None    => Matrix.empty[M, N, C]
      val biter = b.rows
      val citer = c.rows
      while biter.hasNext && citer.hasNext do
        val bitem = biter.next
        val citem = citer.next
        bitem.hadamard(a, Some(citem))((b, a) => f(a, b))
      c

  inline given expandColVectorLeft[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, 1, A], [B] =>> Matrix[M, N, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, 1, A],
        inline b: Matrix[M, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) =
      val c     = inline out match
        case Some(c) => c.asTransposed[false]
        case None    => Matrix.empty[M, N, C]
      val biter = b.columns
      val citer = c.columns
      while biter.hasNext && citer.hasNext do
        val bitem = biter.next
        val citem = citer.next
        bitem.hadamard(a, Some(citem))((b, a) => f(a, b))
      c

  inline given expandOneElementLeft[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[1, 1, A], [B] =>> Matrix[M, N, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[1, 1, A],
        inline b: Matrix[M, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = b.scalar(out)(b => f(a.value, b))

  inline given expandOneElementRight[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, N, A], [B] =>> Matrix[1, 1, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, N, A],
        inline b: Matrix[1, 1, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = a.scalar(out)(a => f(a, b.value))

  inline given expandScalarLeft[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Scalar[A], [B] =>> Matrix[M, N, B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Scalar[A],
        inline b: Matrix[M, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = b.scalar(out)(b => f(a, b))

  inline given expandScalarRight[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, N, A], [B] =>> Scalar[B]] with
    type H[C] = Matrix[M, N, C]

    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, N, A],
        inline b: Scalar[B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = a.scalar(out)(a => f(a, b))

object Broadcast extends BroadcastExpand:

  type Aux[F[_], G[_], H0[_]] = Broadcast[F, G] { type H[C] = H0[C] }

  inline given matrixSameSizeCanBroadcast[
      M <: Dimension,
      N <: Dimension
  ]: Broadcast[[A] =>> Matrix[M, N, A], [B] =>> Matrix[M, N, B]] with
    type H[C] = Matrix[M, N, C]
    transparent inline def broadcast[A, B, C](
        inline a: Matrix[M, N, A],
        inline b: Matrix[M, N, B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = a.hadamard(b, out)(f)

  inline given scalarsCanBroadcast: Broadcast[Scalar, Scalar] with
    type H[C] = Scalar[C]
    transparent inline def broadcast[A, B, C](
        inline a: Scalar[A],
        inline b: Scalar[B],
        inline out: Option[H[C]]
    )(inline f: (A, B) => C) = f(a, b)

end Broadcast

object BroadcastOps:

  transparent inline def scalar[A](inline a: A) = Scalar.apply(a)

  extension [F[_], A: Numeric](inline a: F[A])

    transparent inline def :+[G[_]](inline b: G[A])(using Broadcast[F, G]) =
      summon[Broadcast[F, G]].broadcast(a, b, None)(
        summon[Numeric[A]].plus(_, _)
      )

    transparent inline def :-[G[_]](inline b: G[A])(using Broadcast[F, G]) =
      summon[Broadcast[F, G]].broadcast(a, b, None)(
        summon[Numeric[A]].minus(_, _)
      )

    transparent inline def :*[G[_]](inline b: G[A])(using Broadcast[F, G]) =
      summon[Broadcast[F, G]].broadcast(a, b, None)(
        summon[Numeric[A]].times(_, _)
      )

    transparent inline def :+=[G[_], H[_]](
        inline b: G[A]
    )(using Broadcast.Aux[F, G, H], F[A] =:= H[A]) =
      summon[Broadcast[F, G]].broadcastInPlace(a, b)(
        summon[Numeric[A]].plus(_, _)
      )

    transparent inline def :-=[G[_], H[_]](
        inline b: G[A]
    )(using Broadcast.Aux[F, G, H], F[A] =:= H[A]) =
      summon[Broadcast[F, G]].broadcastInPlace(a, b)(
        summon[Numeric[A]].minus(_, _)
      )

    transparent inline def :*=[G[_], H[_]](
        inline b: G[A]
    )(using Broadcast.Aux[F, G, H], F[A] =:= H[A]) =
      summon[Broadcast[F, G]].broadcastInPlace(a, b)(
        summon[Numeric[A]].times(_, _)
      )
  end extension

  extension [F[_], A: Fractional](inline a: F[A])

    transparent inline def :/[G[_]](inline b: G[A])(using Broadcast[F, G]) =
      summon[Broadcast[F, G]].broadcast(a, b, None)(
        summon[Fractional[A]].div(_, _)
      )
    transparent inline def :/=[G[_], H[_]](
        inline b: G[A]
    )(using Broadcast.Aux[F, G, H], F[A] =:= H[A]) =
      summon[Broadcast[F, G]].broadcastInPlace(a, b)(
        summon[Fractional[A]].div(_, _)
      )

  extension [F[_], A](inline a: F[A])
    transparent inline def :==[G[_]](inline b: G[A])(using Broadcast[F, G]) =
      summon[Broadcast[F, G]].broadcast(a, b, None)(_ == _)

end BroadcastOps

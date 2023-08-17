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
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

import Slice.*

type Dimension = Int
type Index     = Int & Singleton

final case class Matrix[
    M <: Dimension,
    N <: Dimension,
    A
] private (
    val data: Array[A],
    val offset: Int,
    val rstride: Int,
    val cstride: Int
):

  def strides = (rstride, cstride)

  inline def shape: (M, N)    = constValueTuple
  inline def size: M * N      = constValue
  inline def isSquare: M == N = constValue

  inline given vm: ValueOf[M] = ValueOf[M](m)
  inline given vn: ValueOf[N] = ValueOf[N](n)

  val m: M = valueOf[M]
  val n: N = valueOf[N]

  type transposed <: Boolean & Singleton

  inline def linearized(inline i: Int, inline j: Int): Int =
    inline erasedValue[transposed] match
      case _: true  => offset + j * cstride + i * rstride
      case _: false => offset + i * cstride + j * rstride

  inline def boundcheck[I <: Index, J <: Index] =
    inline if erasedValue[I < 0 || I >= M || J < 0 || J >= N] then
      error(
        "Index (" + codeOf(constValue[I]) + ", " + codeOf(
          constValue[J]
        ) + ") out of bound for matrix with shape (" + codeOf(
          m
        ) + ", " + codeOf(n) + ")"
      )

  inline def boundcheck[I <: Index] =
    inline if erasedValue[I < 0 || I >= M * N] then
      error(
        "Index " + codeOf(
          constValue[I]
        ) + " out of bound for matrix with size " + codeOf(constValue[M * N])
      )

  inline def apply[I <: Index, J <: Index]: A =
    boundcheck[I, J]
    apply(constValue[I], constValue[J])

  inline def apply(inline i: Int, inline j: Int): A =
    data.asInstanceOf[Array[A]].apply(linearized(i, j))

  inline def apply[I <: Index]: A =
    boundcheck[I]
    apply(constValue[I])

  inline def apply(inline i: Int): A = apply(i / n, i % n)

  inline def update[I <: Index, J <: Index](inline a: A): Unit =
    boundcheck[I, J]
    update(constValue[I], constValue[J], a)

  inline def update(
      inline i: Int,
      inline j: Int,
      inline a: A
  ): Unit =
    data.asInstanceOf[Array[A]].update(linearized(i, j), a)

  inline def update[I <: Index](inline a: A): Unit =
    boundcheck[I]
    update(constValue[I], a)

  inline def update(inline i: Int, inline a: A): Unit =
    update(i / n, i % n, a)

  inline def value(using M =:= 1, N =:= 1): A = apply[0, 0]

  inline def asTransposed[T <: Boolean & Singleton]
      : Matrix[M, N, A] { type transposed = T } = this.asInstanceOf

  inline def t
      : Matrix[N, M, A] { type transposed = ![Matrix.this.transposed] } =
    this.asInstanceOf

  inline def reshape[
      M1 <: Dimension,
      N1 <: Dimension
  ] =

    inline if erasedValue[M1 * N1 != M * N] then
      error(
        "Can't reshape matrix of shape (" + codeOf(m) + ", " + codeOf(
          n
        ) + ") to shape (" +
          codeOf(constValue[M1])
          + ", " + codeOf(constValue[N1]) + ")"
      )

    if cstride == n && valueOf[![transposed]] then
      Matrix
        .unsafe[M1, N1, A](
          data,
          offset,
          rstride,
          valueOf[N1]
        )
    else Matrix.tabulate[M1, N1, A](i => this(i))

  inline def vectorize = reshape[M * N, 1]

  inline def slice[S1 <: Slice, S2 <: Slice] =

    summonInline[WellFormed[S1, 0, M] =:= true]
    summonInline[WellFormed[S2, 0, N] =:= true]

    Matrix
      .unsafe[
        Sliced[S1, M],
        Sliced[S2, N],
        A
      ](data, constValue[Lower[S2, 0]] + constValue[Lower[S1, 0]] * n, 1, n)

  inline def slice[S1 <: Slice, S2 <: Slice](s1: S1, s2: S2) =
    Matrix
      .unsafe[
        Sliced[S1, M],
        Sliced[S2, N],
        A
      ](data, constValue[Lower[S2, 0]] + constValue[Lower[S1, 0]] * n, 1, n)

  inline def diagonal(using M =:= N) = Matrix
    .unsafe[N, 1, A](data, 0, 1, n + 1)

  inline def indices = new Iterator[(Int, Int)]:
    val s       = Matrix.this.size
    var i       = 0
    def hasNext = i < s
    def next()  =
      val index = (i / n, i % n)
      i += 1
      index

  inline def elements = new Iterator[A]:
    var i       = 0
    def hasNext = i < Matrix.this.size
    def next()  =
      val element = apply(i)
      i += 1
      element

  inline def columns =
    new Iterator[Matrix[M, 1, A] { type transposed = false }]:
      var j       = 0
      def hasNext = j < n
      def next()  =
        val column = Matrix.unsafe[M, 1, A](data, j, n, n)
        j += 1
        column

  inline def rows =
    new Iterator[Matrix[1, N, A] { type transposed = false }]:
      var i       = 0
      def hasNext = i < m
      def next()  =
        val row = Matrix.unsafe[1, N, A](data, i * n, 1, n)
        i += 1
        row

  inline def copy =
    val out = InlineArray.ofSize[A](data.size)
    System.arraycopy(data, 0, out, 0, data.size)
    Matrix
      .unsafe[M, N, A](out, offset, rstride, cstride)
      .asTransposed[transposed]

  inline def show: String = elements
    .grouped(n)
    .map(_.mkString("(", ",", ")"))
    .reduce(_ + "\n" + _)

  inline def pad[
      P1 <: Dimension,
      Q1 <: Dimension,
      P2 <: Dimension,
      Q2 <: Dimension
  ](
      a: A
  ) =
    val p1 = valueOf[P1]
    val q1 = valueOf[Q1]
    Matrix.tabulate[M + P1 + P2, N + Q1 + Q2, A]((i, j) =>
      if i - p1 < 0 || j - q1 < 0 || i - p1 >= m || j - q1 >= n then a
      else this(i - p1, j - q1)
    )

  inline def scalar[B](
      inline out: Option[Matrix[M, N, B]] = None
  )(
      inline f: A => B
  ) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M, N, B]

    var i = 0
    while i < size do
      result(i) = f(this(i))
      i += 1
    result

  inline def map[B](inline f: A => B)     = scalar(None)(f)
  inline def mapInPlace(inline f: A => A) = scalar(Some(this))(f)
  inline def copyto[T <: Boolean & Singleton](inline out: Matrix[M, N, A] {
    type transposed = T
  }) =
    if (strides == out.strides) && (offset == out.offset) && (data.size == out.data.size) && (valueOf[
        transposed
      ] == valueOf[T])
    then
      System.arraycopy(data, 0, out.data, 0, data.size)
      out
    else scalar(Some(out))(identity)

  inline def hadamard[B, C](
      that: Matrix[M, N, B],
      inline out: Option[Matrix[M, N, C]] = None
  )(inline f: (A, B) => C) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M, N, C]

    var i = 0
    while i < size do
      result(i) = f(this.apply(i), that.apply(i))
      i += 1
    result

  inline def map2[B, C](that: Matrix[M, N, B])(
      inline f: (A, B) => C
  ) =
    hadamard(that, None)(f)
  inline def map2InPlace[B](that: Matrix[M, N, B])(
      inline f: (A, B) => A
  ) =
    hadamard(that, Some(this))(f)

  inline def zip[B](
      that: Matrix[M, N, B]
  ) =
    map2(that)((a, b) => (a, b))

  inline def matmul[P <: Dimension, B, C](
      that: Matrix[N, P, B],
      inline out: Option[Matrix[M, P, C]] = None
  )(
      inline mul: (A, B) => C,
      inline add: (C, C) => C,
      inline zero: C
  ) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M, P, C]

    val p = that.n

    var i = 0
    while i < m do
      var j = 0
      while j < p do
        result(i, j) = zero
        var k = 0
        while k < n do
          result(i, j) = add(result(i, j), mul(this(i, k), that(k, j)))
          k += 1
        j += 1
      i += 1

    result

  end matmul

  inline def kronecker[
      P <: Dimension,
      Q <: Dimension,
      B,
      C
  ](
      that: Matrix[P, Q, B],
      inline out: Option[Matrix[M * P, N * Q, C]] = None
  )(
      inline mul: (A, B) => C
  ) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M * P, N * Q, C]

    val (p, q) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        var k = 0
        while k < p do
          var l = 0
          while l < q do
            result(i * p + k, j * q + l) = mul(this(i, j), that(k, l))
            l += 1
          k += 1
        j += 1
      i += 1

    result

  end kronecker

  inline def flatten[
      P <: Dimension,
      Q <: Dimension,
      B
  ](using
      A <:< Matrix[P, Q, B]
  ) =
    Matrix.tabulate[M * P, N * Q, B]((i, j) =>
      this(i / valueOf[P], j / valueOf[Q])(
        i % valueOf[P],
        j % valueOf[Q]
      )
    )

  inline def flatMap[
      P <: Dimension,
      Q <: Dimension,
      B,
      Nested <: Matrix[
        P,
        Q,
        B
      ]
  ](
      inline f: A => Nested
  ) =
    val out = Matrix.empty[M * P, N * Q, B]

    val (p, q) = (valueOf[P], valueOf[Q])
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        val nested = f(this(i, j))
        var k      = 0
        while k < p do
          var l = 0
          while l < q do
            out(i * p + k, j * q + l) = nested(k, l)
            l += 1
          k += 1
        j += 1
      i += 1

    out

  inline def khatrirao[P <: Dimension, B, C](
      that: Matrix[P, N, B],
      inline out: Option[Matrix[M * P, N, C]] = None
  )(inline mul: (A, B) => C) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M * P, N, C]

    val (p, _) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        var k = 0
        while k < p do
          result(i * p + k, j) = mul(this(i, j), that(k, j))
          k += 1
        j += 1
      i += 1

    result

  end khatrirao

  inline def facesplitting[Q <: Dimension, B, C](
      that: Matrix[M, Q, B],
      inline out: Option[Matrix[M, N * Q, C]] = None
  )(inline mul: (A, B) => C) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M, N * Q, C]

    val (_, q) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        var l = 0
        while l < q do
          result(i, j * q + l) = mul(this(i, j), that(i, l))
          l += 1
        j += 1
      i += 1

    result

  end facesplitting

  inline def directsum[
      P <: Dimension,
      Q <: Dimension,
      B,
      C
  ](
      that: Matrix[P, Q, B],
      inline out: Option[Matrix[M + P, N + Q, C]] = None
  )(inline i1: A => C, inline i2: B => C, inline zero: C) =

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M + P, N + Q, C]

    val (p, q) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        result(i, j) = i1(this(i, j))
        j += 1
      while j < n + q do
        result(i, j) = zero
        j += 1
      i += 1
    while i < m + p do
      var j = 0
      while j < n do
        result(i, j) = zero
        j += 1
      while j < q + n do
        result(i, j) = i2(that(i - m, j - n))
        j += 1
      i += 1
    result

  inline def hcat[Q <: Dimension, B, C](
      that: Matrix[M, Q, B]
  )(inline i1: A => C, inline i2: B => C) =

    val out = Matrix.empty[M, N + Q, C]

    val (p, q) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        out(i, j) = i1(this(i, j))
        j += 1
      while j < n + q do
        out(i, j) = i2(that(i, j - n))
        j += 1
      i += 1

    out

  inline def fork[Q <: Dimension](
      that: Matrix[M, Q, A]
  ) =
    hcat(that)(identity, identity)

  inline def vcat[P <: Dimension, B, C](
      that: Matrix[P, N, B]
  )(inline i1: A => C, inline i2: B => C) =

    val out = Matrix.empty[M + P, N, C]

    val (p, q) = that.shape
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        out(i, j) = i1(this(i, j))
        j += 1
      i += 1
    while i < m + p do
      var j = 0
      while j < n do
        out(i, j) = i2(that(i - m, j))
        j += 1
      i += 1
    out

  inline def join[P <: Dimension](
      that: Matrix[P, N, A]
  ) =
    vcat(that)(identity, identity)

  inline def convolve[
      P <: Dimension,
      Q <: Dimension,
      B,
      C
  ](
      kernel: Matrix[P, Q, B],
      inline out: Option[Matrix[M, N, C]] = None
  )(inline mul: (A, B) => C, inline add: (C, C) => C, inline zero: C)(using
      P <= M =:= true,
      Q <= N =:= true
  ) =

    val ci = kernel.m / 2
    val cj = kernel.n / 2

    val result = inline out match
      case Some(result) => result.asTransposed[false]
      case None         => Matrix.empty[M, N, C]

    val (p, q) = kernel.shape

    var i = 0
    while i < m do
      var j = 0
      while j < n do
        result(i, j) = zero
        var k = 0
        while k < p do
          var l = 0
          while l < q do
            val ii = i + (k - ci)
            val jj = j + (l - cj)
            if ii >= 0 & ii < m & jj >= 0 & jj < n then
              result(i, j) = add(result(i, j), mul(this(ii, jj), kernel(k, l)))
            l += 1
          k += 1
        j += 1
      i += 1

    result

  end convolve

  inline def fold[B](z: B)(inline f: (B, A) => B): B =
    var acc = z
    var i   = 0
    while i < size do
      acc = f(acc, this(i))
      i += 1
    acc

  inline def reduce(inline f: (A, A) => A): A =
    var acc = this(0)
    var i   = 1
    while i < size do
      acc = f(acc, this(i))
      i += 1
    acc

  inline def forall(inline p: A => Boolean): Boolean =
    var res = true
    var i   = 0
    while res & i < size do
      res = p(this(i))
      i += 1
    res

  inline def foreach[U](inline f: A => U): Unit =
    var i = 0
    while i < size do
      f(this(i))
      i += 1

  inline def frobenius[B, C](
      that: Matrix[M, N, B]
  )(inline mul: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var acc = zero
    var i   = 0
    while i < size do
      acc = add(acc, mul(this(i), that(i)))
      i += 1
    acc

end Matrix

object Matrix:

  inline def unsafe[
      M <: Dimension,
      N <: Dimension,
      A
  ](
      data: Array[A],
      inline offset: Int = 0,
      inline rstride: Int = 1,
      inline cstride: Int = valueOf[N],
      inline check: Boolean = true
  ) =
    inline if check then
      inline if erasedValue[![M > 0]] then error("M is not positive")
      inline if erasedValue[![N > 0]] then error("N is not positive")
    Matrix[M, N, A](data, offset, rstride, cstride).asTransposed[false]

  inline def empty[
      M <: Dimension,
      N <: Dimension,
      A
  ] =
    unsafe[M, N, A](
      InlineArray.ofSize[A](valueOf[M] * valueOf[N]),
      check = false
    )

  inline def tabulate[
      M <: Dimension,
      N <: Dimension,
      A
  ](
      inline f: (Int, Int) => A
  ) =
    val result = Matrix.empty[M, N, A]
    val m      = result.m
    val n      = result.n
    var i      = 0
    while i < m do
      var j = 0
      while j < n do
        result(i, j) = f(i, j)
        j += 1
      i += 1
    result

  inline def tabulate[
      M <: Dimension,
      N <: Dimension,
      A
  ](
      inline f: Int => A
  ) =
    val result = Matrix.empty[M, N, A]
    val size   = result.size
    var i      = 0
    while i < size do
      result(i) = f(i)
      i += 1
    result

  type AsTuple[X] <: NonEmptyTuple = X match
    case NonEmptyTuple => X & NonEmptyTuple

  inline def apply[L <: NonEmptyTuple](
      aa: L
  )(using Tuple.Head[L] <:< NonEmptyTuple)(using
      Tuple.Union[L] =:= Tuple.Head[L]
  ): Matrix[Tuple.Size[L], Tuple.Size[AsTuple[Tuple.Head[L]]], Tuple.Head[
    AsTuple[Tuple.Head[L]]
  ]] { type transposed = false } = Matrix
    .tabulate[
      Tuple.Size[L],
      Tuple.Size[AsTuple[Tuple.Head[L]]],
      Tuple.Head[
        AsTuple[Tuple.Head[L]]
      ]
    ]((i, j) =>
      aa.productElement(i)
        .asInstanceOf[AsTuple[Tuple.Head[L]]]
        .productElement(j)
        .asInstanceOf[Tuple.Head[AsTuple[Tuple.Head[L]]]]
    )

  inline def fill[
      M <: Dimension,
      N <: Dimension,
      A
  ](
      inline a: => A
  ) = Matrix.tabulate[M, N, A](_ => a)

  inline def scalar[A](a: A) = Matrix.fill[1, 1, A](a)

  inline def zeros[
      M <: Dimension,
      N <: Dimension,
      A: Numeric
  ] = fill[M, N, A](summon[Numeric[A]].zero)

  inline def ones[
      M <: Dimension,
      N <: Dimension,
      A: Numeric
  ] = fill[M, N, A](summon[Numeric[A]].one)

  inline def identity[N <: Dimension, A: Numeric] = Matrix
    .tabulate[N, N, A]((i, j) =>
      if i == j then summon[Numeric[A]].one else summon[Numeric[A]].zero
    )

end Matrix

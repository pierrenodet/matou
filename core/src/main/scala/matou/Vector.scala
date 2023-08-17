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

type Vector[N <: Int, A] = Matrix[1, N, A] { type transposed = false }

object Vector:
  inline def unsafe[N <: Dimension, A](
      raw: Array[A],
      inline offset: Int = 0,
      inline stride: Int = valueOf[N]
  ) = Matrix.unsafe[1, N, A](raw, offset, stride, 1)

  inline def empty[N <: Dimension, A] =
    Matrix.empty[1, N, A]

  inline def tabulate[N <: Dimension, A](
      inline f: Int => A
  ) =
    Matrix.tabulate[1, N, A](f)

  inline def apply[L <: NonEmptyTuple](
      aa: L
  )(using
      Tuple.Union[L] =:= Tuple.Head[L],
      ValueOf[Tuple.Size[L]]
  ) = Vector
    .tabulate[Tuple.Size[L], Tuple.Head[L]](i =>
      aa.productElement(i).asInstanceOf[Tuple.Head[L]]
    )

  inline def fill[N <: Dimension, A](
      inline a: => A
  ) = Matrix
    .fill[1, N, A](a)

  inline def scalar[A](a: A) = Matrix.scalar(a)

  inline def zeros[N <: Dimension, A: Numeric] = Matrix
    .zeros[1, N, A]

  inline def ones[N <: Dimension, A: Numeric] = Matrix
    .ones[1, N, A]

end Vector

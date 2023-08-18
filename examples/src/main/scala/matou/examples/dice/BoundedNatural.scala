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

package matou.examples.dice

import scala.compiletime.ops.int.*
import matou.Enumerable

case class BoundedNatural[L <: Int, U <: Int](value: Int)

object BoundedNatural:
  extension [L1 <: Int, U1 <: Int](bn: BoundedNatural[L1, U1])
    def +[L2 <: Int, U2 <: Int](bn2: BoundedNatural[L2, U2]) =
      BoundedNatural[L1 + L2, U1 + U2](bn.value + bn2.value)

  given [L <: Int: ValueOf, U <: Int: ValueOf]: Enumerable[BoundedNatural[L, U]]
  with
    type C = U - L + 1
    def elements: IndexedSeq[BoundedNatural[L, U]] =
      (valueOf[L] to valueOf[U]).map(BoundedNatural[L, U](_))

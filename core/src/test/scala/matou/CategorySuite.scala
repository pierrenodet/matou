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

import org.scalacheck.Prop.*

import MathOps.*
import BroadcastOps.*
import CategoryOps.*

class CategorySuite extends munit.ScalaCheckSuite:

  property("coproduct bifunctor") {
    forAll {
      (
          a: Matrix[3, 3, Double] { type transposed = false },
          b: Matrix[3, 3, Double] { type transposed = false }
      ) =>
        ((i1[3, Double] >>> a ||| i2[3, Double] >>> b) :== (a ⊕ b))
          .forall(identity)
    }
  }

  property("product bifunctor") {
    forAll {
      (
          a: Matrix[3, 3, Double] { type transposed = false },
          b: Matrix[3, 3, Double] { type transposed = false }
      ) =>
        (((ir1[3, Double] >>> a) ### (ir2[3, Double] >>> b)) :== (a ⊗ b))
          .forall(identity)
    }
  }

end CategorySuite

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

class KroneckerSuite extends munit.ScalaCheckSuite:

  property("kroenecker is flatMap with elementwise multiply") {
    forAll {
      (
          a: Matrix[3, 3, Double] { type transposed = false },
          b: Matrix[3, 3, Double] { type transposed = false }
      ) => (a ⊗ b :== a.flatMap(_ => b :* a)).forall(identity)
    }
  }

  property("transpose slyusar") {
    forAll {
      (
          a: Matrix[3, 3, Int] { type transposed = false },
          b: Matrix[3, 3, Int] { type transposed = false }
      ) => ((a ∙ b).t :== a.t ∗ b.t).forall(identity)
    }
  }

end KroneckerSuite

class KroneckerTestSuite extends munit.FunSuite:

  test("wikipedia column-wise kronecker product") {
    val C  = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
    val D  = C.t.copy
    val CD = Matrix(
      (1, 8, 21),
      (2, 10, 24),
      (3, 12, 27),
      (4, 20, 42),
      (8, 25, 48),
      (12, 30, 54),
      (7, 32, 63),
      (14, 40, 72),
      (21, 48, 81)
    )

    assertEquals((C ∗ D).show, CD.show)
  }

  test("wikipedia face-splitting product") {
    val C  = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
    val D  = C.t.copy
    val CD = Matrix(
      (1, 4, 7, 2, 8, 14, 3, 12, 21),
      (8, 20, 32, 10, 25, 40, 12, 30, 48),
      (21, 42, 63, 24, 48, 72, 27, 54, 81)
    )
    assertEquals((C ∙ D).show, CD.show)

  }

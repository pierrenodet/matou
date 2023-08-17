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

class ForkJoinSuite extends munit.ScalaCheckSuite:

  property("fork is hcat with identity") {
    forAll {
      (
          a: Matrix[4, 3, Double] { type transposed = false },
          b: Matrix[4, 3, Double] { type transposed = false }
      ) => (a.fork(b) :== a.hcat(b)(identity, identity)).forall(identity)
    }
  }

  property("join is vcat with identity") {
    forAll {
      (
          a: Matrix[4, 3, Double] { type transposed = false },
          b: Matrix[4, 3, Double] { type transposed = false }
      ) => (a.join(b) :== a.vcat(b)(identity, identity)).forall(identity)
    }
  }

  property("join is vcat with identity for transposed") {
    forAll {
      (
          a: Matrix[4, 3, Double] { type transposed = false },
          b: Matrix[4, 3, Double] { type transposed = false }
      ) =>
        (a.t.join(b.t) :== a.t.vcat(b.t)(identity, identity)).forall(identity)
    }
  }

  property("fork is hcat with identity for transposed") {
    forAll {
      (
          a: Matrix[4, 3, Double] { type transposed = false },
          b: Matrix[4, 3, Double] { type transposed = false }
      ) =>
        (a.t.fork(b.t) :== a.t.hcat(b.t)(identity, identity)).forall(identity)
    }
  }

end ForkJoinSuite

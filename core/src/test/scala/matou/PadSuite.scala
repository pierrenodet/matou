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

import MathOps.*

class PadSuite extends munit.FunSuite:

  test("test pad 1 1 1 1") {
    val mat = Matrix.identity[2, Int]
    val pad = Matrix((0, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 0))

    assertEquals(
      mat.pad[1, 1, 1, 1](0).show,
      pad.show
    )
  }

  test("test pad 0 1 1 1") {
    val mat = Matrix.identity[2, Int]
    val pad = Matrix((0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 0))

    assertEquals(
      mat.pad[0, 1, 1, 1](0).show,
      pad.show
    )
  }

  test("test pad 1 0 1 1") {
    val mat = Matrix.identity[2, Int]
    val pad = Matrix((0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 0))

    assertEquals(
      mat.pad[1, 0, 1, 1](0).show,
      pad.show
    )
  }

  test("test pad 1 1 0 1") {
    val mat = Matrix.identity[2, Int]
    val pad = Matrix((0, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0))

    assertEquals(
      mat.pad[1, 1, 0, 1](0).show,
      pad.show
    )
  }

  test("test pad 1 1 1 0") {
    val mat = Matrix.identity[2, Int]
    val pad = Matrix((0, 0, 0), (0, 1, 0), (0, 0, 1), (0, 0, 0))

    assertEquals(
      mat.pad[1, 1, 1, 0](0).show,
      pad.show
    )
  }

end PadSuite

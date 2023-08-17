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

import Slice.*

class CopySuite extends munit.FunSuite:

  test("test copy slice") {
    val mat = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))

    assertEquals(
      mat.slice[0 :: 1, 0 :: 1].copy.elements.toList,
      mat.slice[0 :: 1, 0 :: 1].elements.toList
    )
  }

  test("test copy row") {
    val mat = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))

    assertEquals(
      mat.columns.next.copy.elements.toList,
      mat.columns.next.elements.toList
    )
  }

  test("test copyto") {
    val mat = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))
    val out = mat.slice[0 :: 2, 0 :: 1].copy

    mat.slice[1 :: 3, 1 :: 2].copyto(out)

    assertEquals(out.show, mat.slice[1 :: 3, 1 :: 2].show)
  }

end CopySuite

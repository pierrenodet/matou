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
import BroadcastOps.*
import Slice.*

class ViewSuite extends munit.FunSuite:

  test("test rows") {
    val mat  = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))
    val rows = List(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, 9),
      Vector(10, 11, 12)
    )

    assertEquals(
      mat.rows.toList.flatMap(_.elements.toList),
      rows.flatMap(_.elements.toList)
    )
  }

  test("test cols") {
    val mat  = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val cols = List(
      Vector(1, 5, 9),
      Vector(2, 6, 10),
      Vector(3, 7, 11),
      Vector(4, 8, 12)
    )

    assertEquals(
      mat.columns.toList.flatMap(_.elements.toList),
      cols.flatMap(_.elements.toList)
    )
  }

  test("test indices apply equals elements") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.indices.map((i, j) => mat(i, j)).toList,
      mat.elements.toList
    )
  }

  test("test row broadcast") {
    val mat = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))
    val row = Vector(1, 1, 1)

    assertEquals(
      (mat :+ row).elements.toList,
      mat.rows.map(_ + row).flatMap(_.elements).toList
    )
  }

  test("test col broadcast") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val col = Vector(1, 1, 1).t

    assertEquals(
      (mat :+ col).columns.flatMap(_.elements).toList,
      mat.columns.map(_ + col).flatMap(_.elements).toList
    )
  }

  test("test normal broadcast") {
    val mat  = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val mat2 = mat

    assertEquals((mat :+ mat2).elements.toList, (mat + mat2).elements.toList)
  }

  test("test scalar broadcast") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      (scalar(2) :* mat).elements.toList,
      mat.map(_ * 2).elements.toList
    )
  }

  test("test slice1") {
    val mat       = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val trueSlice = Matrix((1, 2, 3, 4), (5, 6, 7, 8))

    val slice = mat.slice[0 :: 1, :::]

    assertEquals(slice.elements.toList, trueSlice.elements.toList)
  }

  test("test slice2") {
    val mat       = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val trueSlice = Matrix((7, 8), (11, 12))

    val slice = mat.slice[1 :: 2, 2 :: 3]

    assertEquals(slice.elements.toList, trueSlice.elements.toList)
  }

  test("test safe slice compile error") {
    val mat       = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val trueSlice = Matrix((1, 2, 3, 4), (5, 6, 7, 8))

    compileErrors("mat.slice[0 :: 10, :::]")
  }

  test("test diag") {
    val mat  = Matrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
    val diag = Vector(1, 5, 9)

    assertEquals(mat.diagonal.elements.toList, diag.elements.toList)
  }

  test("test transpose") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))
    val t   = Matrix((1, 5, 9), (2, 6, 10), (3, 7, 11), (4, 8, 12))

    assertEquals(mat.t.elements.toList, t.elements.toList)
  }

  test("test reshape to col") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(mat.reshape[1, 12].elements.toList, mat.elements.toList)
  }

  test("test reshape to row") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(mat.reshape[12, 1].elements.toList, mat.elements.toList)
  }

  test("test reshape slice") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.slice[0 :: 1, 0 :: 1].reshape[4, 1].elements.toList,
      mat.slice[0 :: 1, 0 :: 1].elements.toList
    )
  }

  test("test reshape transposed") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(mat.t.reshape[1, 12].elements.toList, mat.t.elements.toList)
  }

  test("test reshape row") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.rows.next.reshape[4, 1].elements.toList,
      mat.rows.next.elements.toList
    )
  }

  test("test reshape col") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.columns.next.reshape[1, 3].elements.toList,
      mat.columns.next.elements.toList
    )
  }

  test("test vectorize") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.vectorize.elements.toList,
      mat.elements.toList
    )
  }

  test("test vectorize transpose") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.t.vectorize.reshape[4, 3].elements.toList,
      mat.t.elements.toList
    )
  }

  test("test vectorize slice") {
    val mat = Matrix((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12))

    assertEquals(
      mat.slice[1 :: 2, 1 :: 2].vectorize.elements.toList,
      mat.slice[1 :: 2, 1 :: 2].elements.toList
    )

  }

end ViewSuite

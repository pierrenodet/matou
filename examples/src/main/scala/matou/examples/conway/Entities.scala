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

package matou.examples.conway

import matou.Matrix

import matou.examples.conway.PlainText.pt
import matou.examples.conway.RLE.rle
import matou.Dimension

object Entities:

  val glider = Matrix(
    (0, 0, 1),
    (1, 0, 1),
    (0, 1, 1)
  ).pad[1, 1, 1, 1](0)

  val block = Matrix.ones[2, 2, Int].pad[1, 1, 1, 1](0)

  val gosper =
    Matrix
      .unsafe[26, 47, Int](
        pt""".O.............................................
            |..O............................................
            |OOO............................................
            |...............................................
            |...............................................
            |...............................................
            |...............................................
            |...............................................
            |...............O...............................
            |...............OOOO............................
            |................OOOO..........OO...............
            |.....OO.........O..O.........O.O...............
            |.....OO.........OOOO........OOO........OO......
            |...............OOOO........OOO.........OO......
            |...............O............OOO................
            |.............................O.O...............
            |..............................OO...............
            |...............................................
            |...............................................
            |...............................................
            |...............................................
            |...............................................
            |...............................................
            |.............................................OO
            |............................................OO.
            |..............................................O"""
      )
      .pad[12, 2, 12, 1](0)

  val xp25 = Matrix
    .unsafe[21, 24, Int](rle"""x = 24, y = 21, rule = B3/S23
    |2bo3b2o5b2o3bo$$bobo2bo7bo2bobo$$bobo4bo3bo4bobo$$2bob5o3b5obo$$4bo11bo$$3bo3b2o3b
    |2o3bo$$2bo4bo5bo4bob2o$$2b2o13b2obo2bo$$20bob2o$$20bo$$18bobo$$18b2o$$7bo5bo$$6b3o3b
    |3o2$$2b2o$$bobo$$bo13b2o$$2o5b2o6bo$$7b2o7b3o$$18bo!""")
    .pad[2, 0, 2, 1](0)

  val pulsar = Matrix
    .unsafe[13, 13, Int](rle"""x = 13, y = 13, rule = B3/S23
    |2b3o3b3o2b2$$o4bobo4bo$$o4bobo4bo$$o4bobo4bo$$2b3o3b3o2b2$$2b3o3b3o2b$$o4bob
    |o4bo$$o4bobo4bo$$o4bobo4bo2$$2b3o3b3o!""")
    .pad[1, 1, 1, 1](0)

  extension [M <: Dimension, N <: Dimension, A](
      a: Matrix[M, N, A]
  )(using M =:= N)
    inline def rotate =
      var i = 0
      while i < a.n / 2 do
        var j = i
        while j < a.n - i - 1 do
          val tmp = a(i, j)
          a(i, j) = a(j, a.n - i - 1)
          a(j, a.n - i - 1) = a(a.n - i - 1, a.n - j - 1)
          a(a.n - i - 1, a.n - j - 1) = a(a.n - j - 1, i)
          a(a.n - j - 1, i) = tmp
          j += 1
        i += 1
      a.asTransposed[a.transposed]

end Entities

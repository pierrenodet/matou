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

import matou.CategoryOps.*
import matou.Enumerable.*
import matou.Vector
import matou.MathOps.*

type Die[N <: Int] = BoundedNatural[1, N]

@main
def sumOf2d6 =

  val d6 = Vector.fill[6, Double](1.0 / 6)

  val `2d6` = lift((d1: Die[6], d2: Die[6]) => d1 + d2) >>> (d6 &&& d6)

  println(`2d6`.show)
  println((d6.pad[0, 3, 0, 3](0) ⋆ d6).show)

/*
 * Copyright 2023 Example
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

package matou.examples.montyhall

import matou.CategoryOps.*
import matou.Enumerable.*
import matou.Vector

@main
def montyhall =

  case object Win
  case object Lose

  type Outcome = Either[Win.type, Lose.type]

  val firstChoice = Vector(1.0 / 3, 2.0 / 3)

  def switch(outcome: Outcome) =
    outcome match
      case Left(Win)   => Right(Lose)
      case Right(Lose) => Left(Win)

  val secondChoice = lift(switch)

  println((secondChoice >>> firstChoice).show)

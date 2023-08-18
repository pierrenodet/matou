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

object RLE:

  val header  = raw"x=([0-9]+),y=([0-9]+),rule=.*".r
  val pattern = raw"([0-9]*)([ob$$])".r

  extension (sc: StringContext)
    def rle(args: Any*) =
      val string  = sc.s(args*).stripMargin.trim
      val lines   = string.split("\n").map(_.filterNot(_.isWhitespace))
      val (x, y)  = lines.head match
        case header(x, y) => (x.toInt, y.toInt)
      val folded  = lines.tail.reduce(_ + _).replace("!", "$")
      val matches = pattern.findAllIn(folded)
      val data    = matches.flatMap(s =>
        s match
          case pattern(n, t) =>
            val nn = if n == "" then 1 else n.toInt
            List.fill(nn)(t)
      )
      data
        .foldLeft((0, List.empty[String])) { case ((c, acc), t) =>
          if t != "$" then (c + 1, t :: acc)
          else (0, List.fill(x - c)("b") ++ acc)
        }
        ._2
        .reverse
        .map(s =>
          s match
            case "b" => 0
            case "o" => 1
        )
        .toArray

end RLE

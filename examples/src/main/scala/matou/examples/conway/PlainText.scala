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

object PlainText:

  extension (sc: StringContext)
    def pt(args: Any*) = sc
      .s(args*)
      .stripMargin
      .trim
      .replaceAll("\n", "")
      .split("")
      .map(c =>
        c match
          case "." => 0
          case "O" => 1
      )

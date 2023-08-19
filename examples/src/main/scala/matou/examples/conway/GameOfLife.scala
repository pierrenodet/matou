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

import processing.core.PApplet
import processing.core.PConstants
import matou.CategoryOps.*
import matou.Kernels
import matou.Matrix
import matou.examples.conway.Entities.rotate

import scala.compiletime.ops.int.*
import scala.util.Random

class GameOfLife extends PApplet:

  val x       = 2000
  val y       = 1000
  val scaling = 1f

  val ENGINE = PConstants.JAVA2D

  println("Preallocating neighborhood")

  val neighborhood = Matrix.zeros[2000, 1000, Int]
  val kernel       = Kernels.neighbors

  println("Generating Entities")

  val gliderOfGliders       = Entities.glider
    .product(Entities.glider)
    .coproduct(
      Entities.glider
        .product(Entities.glider)
    )
  val alignedPulsars        = Entities.pulsar
    .coproduct(Entities.pulsar)
    .coproduct(Entities.pulsar)
    .pad[2, 2, 3, 3](0)
  val diagonalOfOscillators = Entities.xp25
    .coproduct(Entities.xp25)
  val zeros50x50            = Matrix.zeros[50, 50, Int]

  println("Generating the Game")

  val game = Matrix
    .fill[2000 / 50, 1000 / 50, Double](Random.nextDouble)
    .flatMap(i =>
      val entity =
        if i > 0.6 && i < 0.7 then gliderOfGliders
        else if i > 0.7 && i < 0.8 then alignedPulsars
        else if i > 0.8 && i < 0.9 then diagonalOfOscillators
        else if i > 0.9 then Entities.gosper
        else zeros50x50
      val copied = entity.copy
      for i <- 0 to Random.nextInt(4) do copied.rotate
      copied
    )

  override def settings() =
    size((x * scaling).toInt, (y * scaling).toInt, ENGINE)

  override def setup() =
    noStroke()
    fill(0)

  override def draw(): Unit =

    println(frameRate)

    game.convolve(kernel, Some(neighborhood))(_ * _, _ + _, 0)
    game.hadamard(neighborhood, Some(game))((cell, neighbors) =>
      if neighbors == 2 then cell
      else if neighbors == 3 then 1
      else 0
    )

    background(255)
    var k = 0
    game.foreach(cell =>
      if cell == 1 then
        val i = k / game.n
        val j = k % game.n
        square(i * scaling, j * scaling, scaling)
      k += 1
    )

  end draw

end GameOfLife

@main
def app(): Unit =
  PApplet.main("matou.examples.conway.GameOfLife")

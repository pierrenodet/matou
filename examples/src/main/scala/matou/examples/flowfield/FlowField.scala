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

// /*
//  * Copyright 2022 Example
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at
//  *
//  *     http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */

package matou.examples.flowfield

import matou.Vector
import processing.core.PApplet

import scala.util.Random

class FlowField extends PApplet:

  val steps          = 1
  val scale          = 1f
  val gradSmoothness = 0.005f
  val viscosity      = 1.0f
  val rnd            = 0.0
  val range          = 2

  val thickness = 3f
  val alpha     = 100
  val c1        = color(204, 102, 0, alpha);
  val c2        = color(0, 102, 153, alpha);

  val seed   = 42
  val random = new Random(seed)

  val w = 1000f
  val h = 1000f

  val particles =
    Vector.fill[1000, Particle[Float]](
      Particle(
        random.between(0f, w),
        random.between(0f, h)
      )
    )
  val pusher    = Vector.zeros[2, Float]
  var t         = 0f

  override def settings() = size(w.toInt, h.toInt)

  override def setup() =

    frameRate(300)

    noiseSeed(seed)

    background(0)
    noStroke()

  override def draw(): Unit =

    println(frameRate)

    t = t + gradSmoothness

    (1 to steps).foreach(_ =>
      particles.foreach(particle =>
        if random.nextDouble() > rnd then
          val angle = noise(
            particle.position[0] / scale * gradSmoothness,
            particle.position[1] / scale * gradSmoothness,
            t
          ) * range * math.Pi
          pusher.update[0](math.cos(angle).toFloat)
          pusher.update[1](math.sin(angle).toFloat)
        else
          pusher.update[0](random.between(-1f, 1f))
          pusher.update[1](random.between(-1f, 1f))
        particle.push(pusher).viscosity(viscosity).move
        val x = particle.position[0]
        val y = particle.position[1]
        particle.position.update[0](
          if x > w then x - w else if x < 0 then x + w else x
        )
        particle.position.update[1](
          if y > h then y - h else if y < 0 then y + h else y
        )
      )
    )

    val c = lerpColor(c1, c2, (math.sin(t.toDouble).toFloat + 1) / 2f);
    fill(c)
    particles
      .foreach(particle =>
        circle(
          particle.position[0],
          particle.position[1],
          thickness
        )
      )
  end draw

end FlowField

@main def app(): Unit =
  PApplet.main("matou.flowfield.FlowField")

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

package matou.examples.flowfield

import matou.MathOps.*
import matou.Vector

import math.Numeric.Implicits.infixNumericOps

type Vec2[A] = Vector[2, A]

final case class Particle[T: Numeric](
    position: Vec2[T],
    velocity: Vec2[T],
    acceleration: Vec2[T]
):

  inline def push(inline force: Vec2[T]) =
    this.acceleration += force
    this

  inline def viscosity(inline k: T) =
    this.acceleration.hadamard(this.velocity, Some(this.acceleration))((a, v) =>
      a - k * v
    )
    this

  inline def move =
    this.velocity += this.acceleration
    this.position += this.velocity
    this.acceleration.mapInPlace(_ => summon[Numeric[T]].zero)
    this

object Particle:

  inline def apply[T: Numeric](inline x: T, inline y: T): Particle[T] =
    Particle(
      Vector(x, y),
      Vector.zeros[2, T],
      Vector.zeros[2, T]
    )

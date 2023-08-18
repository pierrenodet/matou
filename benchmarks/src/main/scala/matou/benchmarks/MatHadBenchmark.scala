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

package matou.benchmark

import matou.InlineArray.*
import matou.Matrix
import matou.MathOps
import matou.MathOps.*
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.util.Random

import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps

@State(Scope.Thread) @BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class MatHadBenchmark:

  @Param(Array("8", "256", "1024"))
  var size: Int = _

  var a: Array[Double] = _
  var b: Array[Double] = _

  val sa1 = Matrix.fill[8, 8, Double](1)
  val sa2 = Matrix.fill[256, 256, Double](1)
  val sa3 = Matrix.fill[1024, 1024, Double](1)
  val sb1 = Matrix.fill[8, 8, Double](1)
  val sb2 = Matrix.fill[256, 256, Double](1)
  val sb3 = Matrix.fill[1024, 1024, Double](1)

  @Setup(Level.Trial)
  def init() =
    a = Array.fill[Double](size * size)(1)
    b = Array.fill[Double](size * size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() =

    val c = Array.ofDim[Double](size * size)

    var i = 0
    while i < size * size do
      c(i) = a(i) * b(i)
      i += 1
    c

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baselineInPlace() =

    var i = 0
    while i < size * size do
      b(i) *= a(i)
      i += 1
    b

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1.hadamard(sb1)(_ * _)
    else if size == 256 then sa2.hadamard(sb2)(_ * _)
    else sa3.hadamard(sb3)(_ * _)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedInPlace() =
    if size == 8 then sa1.map2InPlace(sb1)(_ * _)
    else if size == 256 then sa2.map2InPlace(sb2)(_ * _)
    else sa3.map2InPlace(sb3)(_ * _)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedGeneric() =
    if size == 8 then sa1 ⊙ sb1
    else if size == 256 then sa2 ⊙ sb2
    else sa3 ⊙ sb3

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedGenericInPlace() =
    if size == 8 then sa1 ⊙= sb1
    else if size == 256 then sa2 ⊙= sb2
    else sa3 ⊙= sb3

end MatHadBenchmark

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

package matou.benchmark

import matou.BroadcastOps.*
import matou.InlineArray.*
import matou.Matrix
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
class MatKroBenchmark:

  @Param(Array("8", "256", "1024"))
  var size: Int        = _
  var a: Array[Double] = _

  val sa1 = Matrix.fill[8, 1, Double](1)
  val sa2 = Matrix.fill[256, 1, Double](1)
  val sa3 = Matrix.fill[1024, 1, Double](1)

  @Setup(Level.Trial)
  def init() =
    a = Array.fill[Double](size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() =
    a.flatMap(aa => a.map(_ * aa))

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1.kronecker(sa1.t)(_ * _)
    else if size == 256 then sa2.kronecker(sa2.t)(_ * _)
    else sa3.kronecker(sa3.t)(_ * _)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedFlatMap() =
    if size == 8 then sa1.flatMap(a => sa1.t :* scalar(a))
    else if size == 256 then sa2.flatMap(a => sa2.t :* scalar(a))
    else sa3.flatMap(a => sa3.t :* scalar(a))

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedMapFlatten() =
    if size == 8 then sa1.map(a => sa1.t :* scalar(a)).flatten
    else if size == 256 then sa2.map(a => sa2.t :* scalar(a)).flatten
    else sa3.map(a => sa3.t :* scalar(a)).flatten

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedKR() =
    if size == 8 then sa1.khatrirao(sa1)(_ * _)
    else if size == 256 then sa2.khatrirao(sa2)(_ * _)
    else sa3.khatrirao(sa3)(_ * _)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedFS() =
    if size == 8 then sa1.t.facesplitting(sa1.t)(_ * _)
    else if size == 256 then sa2.t.facesplitting(sa2.t)(_ * _)
    else sa3.t.facesplitting(sa3.t)(_ * _)

end MatKroBenchmark

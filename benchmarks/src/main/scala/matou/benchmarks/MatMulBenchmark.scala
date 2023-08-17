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

import matou.Matrix
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.util.Random
import matou.MathOps.*
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps
import matou.MathOps
import matou.InlineArray.*
import dev.ludovic.netlib.blas.JavaBLAS

@State(Scope.Thread) @BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class MatMulBenchmark:

  @Param(Array("3", "256", "1024"))
  var size: Int = _

  var a: Array[Double] = _

  val blas: BLAS  = BLAS.getInstance()
  val jblas: BLAS = JavaBLAS.getInstance()

  val sa1 = Matrix.fill[3, 3, Double](1)
  val sa2 = Matrix.fill[256, 256, Double](1)
  val sa3 = Matrix.fill[1024, 1024, Double](1)

  val kernel = Matrix.fill[3, 3, Double](1)

  @Setup(Level.Trial)
  def init() = a = Array.fill[Double](size * size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 3 then sa1.matmul(sa1)(_ * _, _ + _, 0d).map2InPlace(sa1)(_ + _)
    else if size == 256 then
      sa2.matmul(sa2)(_ * _, _ + _, 0d).map2InPlace(sa2)(_ + _)
    else sa3.matmul(sa3)(_ * _, _ + _, 0d).map2InPlace(sa3)(_ + _)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedOp() =
    if size == 3 then
      val kek = (sa1 * sa1) += sa1
    else if size == 256 then (sa2 * sa2) += sa2
    else (sa3 * sa3) += sa3

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedAST() =
    if size == 3 then sa1 ⋆ kernel
    else if size == 256 then sa2 ⋆ kernel
    else sa3 ⋆ kernel

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedConvolve() =
    if size == 3 then sa1.convolve(kernel)(_ * _, _ + _, 0d)
    else if size == 256 then sa2.convolve(kernel)(_ * _, _ + _, 0d)
    else sa3.convolve(kernel)(_ * _, _ + _, 0d)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def blasgemm() = blas
    .dgemm("N", "N", size, size, size, 1.0, a, size, a, size, 1, a, size)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def jblasgemm() = jblas
    .dgemm("N", "N", size, size, size, 1.0, a, size, a, size, 1, a, size)

end MatMulBenchmark

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
import matou.MathOps
import matou.MathOps.*
import matou.InlineArray.*
import dev.ludovic.netlib.blas.BLAS
import dev.ludovic.netlib.blas.JavaBLAS
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
class MatSumBenchmark:

  @Param(Array("8", "1024", "8192"))
  var size: Int = _

  var a: Array[Double] = _
  val blas: BLAS       = BLAS.getInstance()
  val jblas: BLAS      = JavaBLAS.getInstance()

  val sa1 = Matrix.fill[8, 1, Double](1)
  val sa2 = Matrix.fill[1024, 1, Double](1)
  val sa3 = Matrix.fill[8192, 1, Double](1)

  @Setup(Level.Trial)
  def init() = a = Array.fill[Double](size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() =

    var i   = 0
    var dot = 0d
    while i < size do
      dot += a(i)
      i += 1
    dot

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def unrolled() =
    var i  = 0
    var s0 = 0d;
    var s1 = 0d;
    var s2 = 0d;
    var s3 = 0d;
    var s4 = 0d;
    var s5 = 0d;
    var s6 = 0d;
    var s7 = 0d;
    while i < size do
      // s0 = Math.fma(a(i + 0), b(i + 0), s0);
      // s1 = Math.fma(a(i + 1), b(i + 1), s1);
      // s2 = Math.fma(a(i + 2), b(i + 2), s2);
      // s3 = Math.fma(a(i + 3), b(i + 3), s3);
      // s4 = Math.fma(a(i + 4), b(i + 4), s4);
      // s5 = Math.fma(a(i + 5), b(i + 5), s5);
      // s6 = Math.fma(a(i + 6), b(i + 6), s6);
      // s7 = Math.fma(a(i + 7), b(i + 7), s7);
      s0 += a(i + 0)
      s1 += a(i + 1)
      s2 += a(i + 2)
      s3 += a(i + 3)
      s4 += a(i + 4)
      s5 += a(i + 5)
      s6 += a(i + 6)
      s7 += a(i + 7)
      i += 8
    s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7
  end unrolled

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1.sum else if size == 1024 then sa2.sum else sa3.sum

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedIndices() =
    if size == 8 then
      var acc = 0d
      val it  = sa1.indices
      while it.hasNext do
        val (i, j) = it.next
        acc += sa1(i, j)
      acc
    else if size == 1024 then
      var acc = 0d
      val it  = sa2.indices
      while it.hasNext do
        val (i, j) = it.next
        acc += sa2(i, j)
      acc
    else
      var acc = 0d
      val it  = sa3.indices
      while it.hasNext do
        val (i, j) = it.next
        acc += sa3(i, j)
      acc

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedElements() =
    if size == 8 then
      var acc = 0d
      val it  = sa1.elements
      while it.hasNext do acc += it.next
      acc
    else if size == 1024 then
      var acc = 0d
      val it  = sa2.elements
      while it.hasNext do acc += it.next
      acc
    else
      var acc = 0d
      val it  = sa3.elements
      while it.hasNext do acc += it.next
      acc

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def blassum() = blas.dasum(size, a, 1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def jblassum() = jblas.dasum(size, a, 1)

end MatSumBenchmark

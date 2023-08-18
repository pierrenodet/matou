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

import matou.Matrix
import matou.MathOps.*
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.util.Random
import matou.MathOps
import dev.ludovic.netlib.blas.JavaBLAS

@State(Scope.Thread) @BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class MatDotBenchmark:

  @Param(Array("8", "1024", "8192"))
  var size: Int = _

  var a: Array[Double] = _
  var b: Array[Double] = _
  val blas: BLAS       = BLAS.getInstance()
  val jblas: BLAS      = JavaBLAS.getInstance()

  val sa1 = Matrix.fill[8, 1, Double](1.0)
  val sa2 = Matrix.fill[1024, 1, Double](1.0)
  val sa3 = Matrix.fill[8192, 1, Double](1.0)

  val sb1 = Matrix.fill[8, 1, Double](1.0)
  val sb2 = Matrix.fill[1024, 1, Double](1.0)
  val sb3 = Matrix.fill[8192, 1, Double](1.0)

  @Setup(Level.Trial)
  def init() =
    a = Array.fill[Double](size)(1.0)
    b = Array.fill[Double](size)(1.0)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() =

    var i   = 0
    var dot = 0d
    while i < size do
      dot = dot + a(0 + 1 * i) * b(0 + 1 * i)
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
      s0 += a(i + 0) * b(i + 0);
      s1 += a(i + 1) * b(i + 1);
      s2 += a(i + 2) * b(i + 2);
      s3 += a(i + 3) * b(i + 3);
      s4 += a(i + 4) * b(i + 4);
      s5 += a(i + 5) * b(i + 5);
      s6 += a(i + 6) * b(i + 6);
      s7 += a(i + 7) * b(i + 7);
      i += 8
    s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7
  end unrolled

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1.frobenius(sb1)(_ * _, _ + _, 0d)
    else if size == 1024 then sa2.frobenius(sb2)(_ * _, _ + _, 0d)
    else sa3.frobenius(sb3)(_ * _, _ + _, 0d)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedTransposed() =
    if size == 8 then sa1.t.frobenius(sb1.t)(_ * _, _ + _, 0d)
    else if size == 1024 then sa2.t.frobenius(sb2.t)(_ * _, _ + _, 0d)
    else sa3.t.frobenius(sb3.t)(_ * _, _ + _, 0d)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedDot() =
    if size == 8 then sa1 ⋅ sb1
    else if size == 1024 then sa2 ⋅ sb2
    else sa3 ⋅ sb3

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedMulT() =
    if size == 8 then (sa1.t * sb1).value
    else if size == 1024 then (sa2.t * sb2).value
    else (sa3.t * sb3).value

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def blasdot() = blas.ddot(size, a, 1, b, 1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def jblasdot() = jblas.ddot(size, a, 1, b, 1)
end MatDotBenchmark

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
import matou.Matrix
import matou.MathOps.*
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*

import java.util.Arrays
import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Thread) @BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class MatEqualBenchmark:

  @Param(Array("8", "1024", "8192"))
  var size: Int = _

  var a: Array[Double] = _
  var b: Array[Double] = _

  val sa1 = Matrix.fill[8, 8, Double](1)
  val sa2 = Matrix.fill[1024, 1024, Double](1)
  val sa3 = Matrix.fill[8192, 8192, Double](1)

  val sb1 = Matrix.fill[8, 8, Double](1)
  val sb2 = Matrix.fill[1024, 1024, Double](1)
  val sb3 = Matrix.fill[8192, 8192, Double](1)

  @Setup(Level.Trial)
  def init() =
    a = Array.fill[Double](size * size)(1)
    b = Array.fill[Double](size * size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() = Arrays.equals(a, b)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def incr() =

    var i  = 0
    var eq = true
    while i < size do
      eq = eq & a(i) == b(i)
      i += 1
    eq

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1 :== sb1
    else if size == 1024 then sa2 :== sb2
    else sa3 :== sb3

end MatEqualBenchmark

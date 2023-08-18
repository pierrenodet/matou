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
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.util.Random
import matou.MathOps.*
import matou.MathOps

@State(Scope.Thread) @BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class MatArgmaxBenchmark:

  @Param(Array("8", "1024", "8192"))
  var size: Int = _

  var a: Array[Double] = _

  val sa1 = Matrix.fill[8, 1, Double](1)
  val sa2 = Matrix.fill[1024, 1, Double](1)
  val sa3 = Matrix.fill[8192, 1, Double](1)

  @Setup(Level.Trial)
  def init() = a = Array.fill[Double](size)(1)

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baseline() = a.indices.maxBy(a)

  @Benchmark
  @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sized() =
    if size == 8 then sa1.argmax
    else if size == 1024 then sa2.argmax
    else sa3.argmax

  @Benchmark
  @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedMaxBy() =
    if size == 8 then sa1.indices.maxBy(sa1(_, _))
    else if size == 1024 then sa2.indices.maxBy(sa2(_, _))
    else sa3.indices.maxBy(sa3(_, _))
end MatArgmaxBenchmark

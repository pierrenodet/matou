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
import matou.MathOps
import matou.MathOps.*
import dev.ludovic.netlib.blas.BLAS
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Thread) @BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class MatApplyBenchmark:

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
  def baselineApply(blackhole: Blackhole) =

    var i = 0
    while i < size do
      blackhole.consume(a(i))
      i += 1

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedApply(blackhole: Blackhole) =

    if size == 8 then
      var i = 0
      while i < size do
        blackhole.consume(sa1(i))
        i += 1
    if size == 1024 then
      var i = 0
      while i < size do
        blackhole.consume(sa2(i))
        i += 1
    if size == 8192 then
      var i = 0
      while i < size do
        blackhole.consume(sa3(i))
        i += 1

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedApplyMat(blackhole: Blackhole) =

    if size == 8 then
      var i = 0
      while i < size do
        blackhole.consume(sa1(i, 0))
        i += 1
    if size == 1024 then
      var i = 0
      while i < size do
        blackhole.consume(sa2(i, 0))
        i += 1
    if size == 8192 then
      var i = 0
      while i < size do
        blackhole.consume(sa3(i, 0))
        i += 1

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedForEach(blackhole: Blackhole) =

    if size == 8 then sa1.foreach(blackhole.consume(_))
    if size == 1024 then sa2.foreach(blackhole.consume(_))
    if size == 8192 then sa3.foreach(blackhole.consume(_))

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def baselineUpdate() =

    var i = 0
    while i < size do
      a(i) = 1.0
      i += 1

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedUpdate() =

    if size == 8 then
      var i = 0
      while i < size do
        sa1(i) = 1.0
        i += 1
    if size == 1024 then
      var i = 0
      while i < size do
        sa2(i) = 1.0
        i += 1
    if size == 8192 then
      var i = 0
      while i < size do
        sa3(i) = 1.0
        i += 1

  @Benchmark @Fork(1)
  @Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
  def sizedUpdateMat() =

    if size == 8 then
      var i = 0
      while i < size do
        sa1(i, 0) = 1.0
        i += 1
    if size == 1024 then
      var i = 0
      while i < size do
        sa2(i, 0) = 1.0
        i += 1
    if size == 8192 then
      var i = 0
      while i < size do
        sa3(i, 0) = 1.0
        i += 1

end MatApplyBenchmark

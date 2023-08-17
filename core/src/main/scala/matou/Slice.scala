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

package matou

import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

import Bound.*
import Slice.*

type Bound = Index | **

object Bound:
  sealed trait **
  case object ** extends **

sealed trait Slice

object Slice:
  final case class ::[+B1 <: Bound, +B2 <: Bound](
      lower: B1,
      upper: B2
  ) extends Slice
  sealed trait ::: extends Slice
  case object :::  extends :::

  extension [B1 <: Bound](b1: B1)
    def ::[B2 <: Bound](b2: B2): B1 :: B2 = Slice.::(b1, b2)

  type WellFormed[S <: Slice, L <: Int, U <: Int] = S match
    case :::      => true
    case ** :: ** => true
    case ** :: u  =>
      (L <= u) && (u < U) match
        case true => true
    case l :: **  =>
      (L <= l) && (l < U) match
        case true => true
    case l :: u   =>
      (L <= l) && (l <= u) && (u < U) match
        case true => true

  type Sliced[S <: Slice, N <: Dimension] <: Dimension = S match
    case :::      => N
    case ** :: ** => N
    case ** :: u  => u + 1
    case l :: **  => N - l
    case l :: u   => u - l + 1

  type Lower[S <: Slice, Min <: Index] <: Int = S match
    case :::     => Min
    case ** :: u => Min
    case l :: u  => l + 0

  type Upper[S <: Slice, Max <: Index] <: Int = S match
    case :::     => Max
    case l :: ** => Max
    case l :: u  => u + 0

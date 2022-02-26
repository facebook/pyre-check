(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is just a namespace adapter *)

include Abstract.Domain
module AbstractBucketedElementSetDomain = Abstract.BucketedElementSetDomain
module AbstractElementSetDomain = Abstract.ElementSetDomain
module AbstractFlatDomain = Abstract.FlatDomain
module AbstractInvertedSetDomain = Abstract.InvertedSetDomain
module AbstractOverUnderSetDomain = Abstract.OverUnderSetDomain
module AbstractMapDomain = Abstract.MapDomain
module AbstractProductDomain = Abstract.ProductDomain
module AbstractSetDomain = Abstract.SetDomain
module AbstractSimpleDomain = Abstract.SimpleDomain
module AbstractToppedSetDomain = Abstract.ToppedSetDomain
module AbstractTreeDomain = Abstract.TreeDomain

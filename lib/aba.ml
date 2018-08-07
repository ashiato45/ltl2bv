open Core
open Set


type 'a altFormula =
  | AFAtomic of 'a
  | AFTrue
  | AFFalse
  | AFAnd of 'a altFormula*'a altFormula
  | AFOr of 'a altFormula*'a altFormula

                             

module type ABA = sig
  module AlphSet: Set.S
  module StateSet: Set.S
  type t = {
      alph: AlphSet.t;
      states: StateSet.t;
      trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
      init: StateSet.Elt.t;
      final: StateSet.t}

  val is_valid: StateSet.Elt.t altFormula -> StateSet.t -> bool             

  module type TOBEUCHI' = sig
    module BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t
    module Beuchi: Beuchi.BEUCHI
           with module StateSet = BeuchiStateSet
           with module AlphSet = AlphSet
                                    
    val convert: t -> Beuchi.t
  end

  module type TOBEUCHI = functor (BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t)
                                   (Beuchi: Beuchi.BEUCHI with module StateSet = BeuchiStateSet
                                    with module AlphSet = AlphSet) ->
                         TOBEUCHI'
                         with module BeuchiStateSet = BeuchiStateSet
                         with module Beuchi = Beuchi
  module ToBeuchi: TOBEUCHI

end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> ABA 
                                                                 with module AlphSet = AlphSet
                                                                 with module StateSet = StateSet


                                                                                          

module Make = (functor (AlphSet: Set.S) (StateSet: Set.S) -> struct
                 module AlphSet = AlphSet
                 module StateSet = StateSet
                 type t = {
                     alph: AlphSet.t;
                     states: StateSet.t;
                     trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
                     init: StateSet.Elt.t;
                     final: StateSet.t}

                 let rec is_valid fml_ assg_ =
                   match fml_ with
                   | AFAtomic x -> StateSet.mem assg_ x
                   | AFTrue -> true
                   | AFFalse -> false
                   | AFAnd (x, y) -> (is_valid x assg_) && (is_valid y assg_)
                   | AFOr (x, y) -> (is_valid x assg_) || (is_valid y assg_)
                         

  module type TOBEUCHI' = sig
    module BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t
    module Beuchi: Beuchi.BEUCHI
           with module StateSet = BeuchiStateSet
           with module AlphSet = AlphSet
                                    
    val convert: t -> Beuchi.t
  end

  module type TOBEUCHI = functor (BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t)
                                   (Beuchi: Beuchi.BEUCHI with module StateSet = BeuchiStateSet
                                    with module AlphSet = AlphSet) ->
                         TOBEUCHI'
                         with module BeuchiStateSet = BeuchiStateSet
                         with module Beuchi = Beuchi

                 module ToBeuchi = functor (BeuchiStateSet: Set.S with type Elt.t = StateSet.t * StateSet.t)
                                                  (Beuchi: Beuchi.BEUCHI with module StateSet = BeuchiStateSet
                                                   with module AlphSet = AlphSet) -> struct
                                     module BeuchiStateSet = BeuchiStateSet
                                     module Beuchi = Beuchi
                                                       
                                     let convert aba_ =
                                       let alph = aba_.alph in
                                       let two_s = aba_.states
                                                   |> StateSet.to_list
                                                   |> Util.power_list
                                                   |> List.map ~f:StateSet.of_list in
                                       let states = Util.make_pairs two_s two_s |> BeuchiStateSet.of_list in
                                       let trans (u, v) a =
                                         let cond xy =
                                           u
                                           |> StateSet.to_list
                                           |> List.map ~f:(fun t ->
                                                  aba_.trans t a)
                                           |> List.for_all ~f:(fun afml_ -> is_valid afml_ xy) in
                                         if StateSet.is_empty u then (
                                           states
                                           |> BeuchiStateSet.filter ~f:(fun (x, y) ->
                                                  let ycond = cond y in
                                                  ycond
                                                )
                                           |> BeuchiStateSet.map ~f:(fun (x, y) ->
                                                  let u'= StateSet.diff y aba_.final in
                                                  let v' = StateSet.inter y aba_.final in
                                                  (u', v')
                                                )
                                         ) else (
                                           states
                                           |> BeuchiStateSet.filter ~f:(fun (x, y) ->
                                                  let xcond = cond x in
                                                  let ycond = cond y in
                                                  xcond && ycond
                                                )
                                           |> BeuchiStateSet.map ~f:(fun (x, y) ->
                                                  let u' = StateSet.diff x aba_.final in
                                                  let v' = StateSet.union y (StateSet.inter x aba_.final) in
                                                  (u', v')
                                                )
                                         )
                                       in 
                                       let init = BeuchiStateSet.singleton (StateSet.singleton aba_.init, StateSet.empty) in
                                       let final = two_s |> List.map ~f:(fun x -> (StateSet.empty, x)) |> BeuchiStateSet.of_list in
                                       {Beuchi.alph=alph; states=states; trans=trans; init=init; final=final}
                                   end
               end: MAKE)


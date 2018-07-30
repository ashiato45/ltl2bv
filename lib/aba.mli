open Core
open Set

type 'a altFormula = AFTrue
                   | AFFalse
                   | AFAnd
                   | AFOr

(* module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> sig
 *                      type t = {
 *                          alph: AlphSet.t;
 *                          states: StateSet.t;
 *                          trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.Elt.t altFormula;
 *                          init: StateSet.Elt.t;
 *                          final: StateSet.t}
 *                          
 *                    end *)
module type ABA = sig
  type ast
  type sst
  type aset
  type sset
  type t = {
      alph: ast;
      states: sst;
      trans: sset -> aset -> sset altFormula;
      init: sset;
      final: sst}
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> ABA 
                                                                 with type ast = AlphSet.t 
                                                                 with type sst = StateSet.t
                                                                 with type aset = AlphSet.Elt.t
                                                                 with type sset = StateSet.Elt.t


module Make: MAKE

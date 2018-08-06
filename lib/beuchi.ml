open Core
open Set
open Sexplib
open Map

module type BEUCHI = sig
  module AlphSet: Set.S
  module StateSet: Set.S
  type t = {
      alph: AlphSet.t;
      states: StateSet.t;
      trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.t;
      init: StateSet.Elt.t;
      final: StateSet.t}
  val to_dot: t -> string
end

module type MAKE = functor (AlphSet: Set.S) (StateSet: Set.S) -> BEUCHI
                                                                 with module AlphSet = AlphSet
                                                                 with module StateSet = StateSet

module Make = (functor (AlphSet: Set.S) (StateSet: Set.S) -> struct
                 module AlphSet = AlphSet
                 module StateSet = StateSet
                 type t = {
                     alph: AlphSet.t;
                     states: StateSet.t;
                     trans: StateSet.Elt.t -> AlphSet.Elt.t -> StateSet.t;
                     init: StateSet.Elt.t;
                     final: StateSet.t}
                 let to_dot autom_ =
                   let module StateSetElt = struct
                       include StateSet.Elt
                       let compare x y = StateSet.compare (StateSet.singleton x) (StateSet.singleton y)
                     end in
                   let module IdMap = Map.Make(StateSetElt) in
                   let states = autom_.states |> StateSet.to_list in
                   let idmap = List.foldi ~init:IdMap.empty
                                 ~f:(fun i acc v ->
                                   IdMap.set ~key:v ~data:i acc
                                 ) states in
                   let state_to_str s =
                     "S"^(s |> IdMap.find_exn idmap |> string_of_int) in
                     (* s |> [%sexp_of: StateSet.t] |> Sexp.to_string *)
                   let text =
                     "digraph beuchi {\n
                      node [shape=doublecircle];\n" in
                   let doublecircles =
                     String.concat ~sep:" "
                       (autom_.final |> StateSet.to_list |> List.map ~f:state_to_str) in
                   let text = text ^ doublecircles ^ "\n" in
                   let text = text ^ "node [shape=circle];\n" in
                   let nodes =
                     Util.make_pairs states (autom_.alph |> AlphSet.to_list)
                     |> List.map ~f:(fun (s, a) ->
                            autom_.trans s a
                            |> StateSet.to_list
                            |> List.map ~f:(fun s_to -> Printf.sprintf "%s -> %s [label=\"%s\"];" (s |> state_to_str) (s_to |> state_to_str) (a |> [%sexp_of: AlphSet.Elt.t] |> Sexp.to_string))
                            |> String.concat ~sep:"\n"
                          )
                     |> String.concat ~sep:"\n"
                   in
                   let text = text ^ nodes ^ "\n" in
                   let text = text ^ "}\n" in
                   text
               end: MAKE)


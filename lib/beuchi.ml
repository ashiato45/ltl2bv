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
      init: StateSet.t;
      final: StateSet.t}
  val to_dot: Stdio.Out_channel.t -> t -> unit             
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
                     init: StateSet.t;
                     final: StateSet.t}
                 let to_dot fmt autom_ =
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
                   Printf.fprintf fmt 
                     "digraph beuchi {\n
                      node [shape=doublecircle];\n";
                   let print_list printer fmt xs =
                     List.iter ~f:(fun x -> Printf.fprintf fmt "%a " printer x) xs in
                   let print_string fmt s = Printf.fprintf fmt "%s" s in
                   Printf.fprintf fmt "%a\n" (print_list print_string) (autom_.final |> StateSet.to_list |> List.map ~f:state_to_str);
                   Printf.fprintf fmt "node [shape=circle];\n";
                   let alphlist = autom_.alph |> AlphSet.to_list in
                   states
                   |> List.iter ~f:(fun s ->
                          alphlist
                          |> List.iter ~f:(fun a ->
                                 autom_.trans s a
                                 |> StateSet.to_list
                                 |> List.iter ~f:(fun s_to ->
                                                  Printf.fprintf fmt "%s -> %s [label=\"%s\"];\n" (s |> state_to_str) (s_to |> state_to_str) (a |> [%sexp_of: AlphSet.Elt.t] |> Sexp.to_string)
                                      )
                               )
                        );
                   autom_.init
                   |> StateSet.to_list
                   |> List.iter ~f:(fun sto ->
                          Printf.fprintf fmt "START -> %s;\n" (sto |> state_to_str)
                        );
                   Printf.fprintf fmt "}\n";
               end: MAKE)


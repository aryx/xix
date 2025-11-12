(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Profiling instrumentation.
 *
 * Depending on profile_kind it can either profile the number of times
 * a function is called or the time spent in a functions (see also
 * Exec_file.profile_kind).
 *
 * Better than 5l/vl/il:
 *  - As opposed to 5l/vl/... we are able to factorize code across archs
 *    by introducing new virtual instructions in Ast_asm.ml: Load, Store, Add,
 *    and Call!!
 *
 * TODO: handle ProfileTime and Trace 
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* polymorphic! work across archs! *)
let rewrite (conf : Exec_file.profile_kind) (rTMP : A.register) (syms : T.symbol_table) (cg : 'i T.code_graph) : 'i T.code_graph * T.data list =
  Logs.info (fun m -> m "Adding profiling instrumentation %s" 
        (Exec_file.show_profile_kind conf));
  let data = ref [] in
 
  (match conf with
  | Exec_file.ProfileCount -> ()
  | _ -> failwith "profiling mode not handled yet"
  );

  (* start at 1 cos __mcount+0 will contain the size of it so one
   * can then easily iterate over it in _mainp()
   *)
  let count = ref 1 in

  let mcount : A.global = 
    A.{ name = "__mcount"; priv = None; signature = None } 
  in

  cg |> T.iter (fun n ->
      match n.instr with
      (* less: could look for NOPROF? *)
      | T.TEXT (ent, (attrs : A.attributes), _size) ->
          if attrs.no_prof
          then ()
          else begin
          data |> Stack_.push 
            (T.DATA (mcount, !count * 4, 4 (* size *), 
              A.Address (A.Global (ent, 0))));

          (* LATER: in 6l/8l it can do in one ADD 1, __mcount+8(SB) operation
           * so maybe we should run a peephole optimizer in o6l for
           * the virtual instructions?
           *)
          let rec n1 = T.{
           instr = T.Virt (A.Load (A.Global (mcount, !count * 4 + 4), rTMP));
           next = Some n2;
           branch = None; n_loc = n.n_loc; real_pc = - 1;
          }
          and n2 = T.{
           (* less: in vl they use ADDU but in 5l regular ADD but matter?
            * should be same machine opcode in the end no because of 
            * 2-complement arch?
            *)
           instr = T.Virt (A.Add (A.S, 1, rTMP));
           next = Some n3;
           branch = None; n_loc = n.n_loc; real_pc = - 1;
          }
          and n3 = T.{
           instr = T.Virt (A.Store (rTMP, A.Global (mcount, !count * 4 + 4)));
           next = n.next;
           branch = None; n_loc = n.n_loc; real_pc = - 1;
          }
          in
          n.next <- Some n1;

          count := !count + 2;
          end
      | T.WORD _ | T.Virt _ | T.I _ ->
          ()
  );
  let v : T.value = T.lookup_global mcount syms in
  (match v.section with
  | T.SXref -> v.section <- T.SData (!count * 4)
  | _ -> failwith (spf "redefinition of %s" mcount.name)
  );
  (* add __mcount+0 and then List.rev !data 
   * even though order should not matter as Datagen will accept
   * DATA in any order as long as the offset is set correctly.
   *)
  let mcount_0 =
    T.DATA (mcount, 0, 4 (* size *), A.Int !count)
  in

  cg, mcount_0 :: List.rev !data

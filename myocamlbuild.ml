(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)
open Ocamlbuild_plugin

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)
;;

Unix.putenv "OCAMLFIND_IGNORE_DUPS_IN"
  ((String.trim @@ syscall "opam config var lib") ^ "/ocaml/compiler-libs")
;;

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->
           let env = BaseEnvLight.load ~allow_empty:true ~filename:(Pathname.basename BaseEnvLight.default_filename) () in

           (* Determine extension of CompiledObject: best *)
           let native_suffix =
             if BaseEnvLight.var_get "is_native" env = "true"
             then "native" else "byte"
           in

           (* Sedlex extensions *)
           flag ["ocaml"; "compile"; "ppx_sedlex"] &
             S [A "-ppx"; A ("sedlex/src/syntax/ppx_sedlex." ^ native_suffix)];

	   flag ["ocaml"; "compile"; "ppx_monadic"] &
	     S [A "-ppx"; A ((String.trim @@ syscall "opam config var ppx_monadic:lib") ^ "/ppx_monadic")];
	   
           flag ["menhir"; "ocaml"] & S [A "--infer"; A "--table"; A "--inspection";] (* A "--trace"; A "--explain"]; *)

       | _ -> ())


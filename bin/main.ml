open Core
open Appel.Ast
(* open Parser *)

(*
Lexical analysis - does this form valid tokens
Syntax analysis - do the tokens have a valid structure
Semantic analysis - does the syntax have accurate meanings
*)

(* let test_program program_name =
  try
      let s = In_channel.read_all program_name |> Lexing.from_string in
      let _ = Parser.program Lexer.read s in
      true
  with
  | _ -> false
;; *)

(* let s = [
  "merge.tig" ;  "test14.tig";  "test20.tig";  "test27.tig";  "test33.tig";  "test4.tig" ;  "test46.tig";  "test8.tig";
"queens.tig";  "test15.tig";  "test21.tig";  "test28.tig";  "test34.tig";  "test40.tig";  "test47.tig";  "test9.tig";
"test1.tig" ;  "test16.tig";  "test22.tig";  "test29.tig";  "test35.tig";  "test41.tig";  "test48.tig";
"test10.tig";  "test17.tig";  "test23.tig";  "test3.tig" ;  "test36.tig";  "test42.tig";  "test49.tig";
"test11.tig";  "test18.tig";  "test24.tig";  "test30.tig";  "test37.tig";  "test43.tig";  "test5.tig";
"test12.tig";  "test19.tig";  "test25.tig";  "test31.tig";  "test38.tig";  "test44.tig";  "test6.tig";
"test13.tig";  "test2.tig" ;  "test26.tig";  "test32.tig";  "test39.tig";  "test45.tig";  "test7.tig"
] *)

let () =

  (* List.iter s ~f:(fun program_name ->
    let path = "sample_programs/" ^ program_name in
    if test_program path then
      printf "Program %s is valid.\n" program_name
    else
      printf "Program %s is invalid.\n" program_name
  )
    ;;

    let failed = List.count s ~f:(fun filename -> not (test_program ("sample_programs/" ^ filename))) in
    printf "Number of failed programs: %d\n" failed
  ;; *)

  let lexed = In_channel.read_all "sample_programs/merge.tig" |> Lexing.from_string in
  try
  Appel.Parser.program Appel.Lexer.read lexed |> show_expr |> printf "%s\n"
  with
  | e ->
    let pos = lexed.Lexing.lex_curr_p in
    printf "Error at line %d, column %d: %s\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      (Exn.to_string e)
;;
  (* let e = BinOp (Int 1, Add, Int 2) in
  printf "%s\n" (show_expr e) *)

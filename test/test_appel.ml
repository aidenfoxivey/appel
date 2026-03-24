let parse_file path =
  let s = open_in path |> Lexing.from_channel in
  Appel.Parser.program Appel.Lexer.read s
;;

let test_parses path () =
  try ignore (parse_file path) with
  | e -> Alcotest.failf "Failed to parse %s: %s" path (Printexc.to_string e)
;;

let test_does_not_parse path () =
  try
    let _ = parse_file path in
    Alcotest.fail "Expected to fail parsing, but it succeeded"
  with
  | _ -> ()
;;

let make name =
  Alcotest.test_case name `Quick (test_parses ("../sample_programs/" ^ name))
;;

let make_fails name =
  Alcotest.test_case name `Quick (test_does_not_parse ("../sample_programs/" ^ name))
;;

let () =
  Alcotest.run
    "appel"
    [ ( "parsing"
      , [ make "merge.tig"
        ; make "queens.tig"
        ; make "test1.tig"
        ; make "test2.tig"
        ; make "test3.tig"
        ; make "test4.tig"
        ; make "test5.tig"
        ; make "test6.tig"
        ; make "test7.tig"
        ; make "test8.tig"
        ; make "test9.tig"
        ; make "test10.tig"
        ; make "test11.tig"
        ; make "test12.tig"
        ; make "test13.tig"
        ; make "test14.tig"
        ; make "test15.tig"
        ; make "test16.tig"
        ; make "test17.tig"
        ; make "test18.tig"
        ; make "test19.tig"
        ; make "test20.tig"
        ; make "test21.tig"
        ; make "test22.tig"
        ; make "test23.tig"
        ; make "test24.tig"
        ; make "test25.tig"
        ; make "test26.tig"
        ; make "test27.tig"
        ; make "test28.tig"
        ; make "test29.tig"
        ; make "test30.tig"
        ; make "test31.tig"
        ; make "test32.tig"
        ; make "test33.tig"
        ; make "test34.tig"
        ; make "test35.tig"
        ; make "test36.tig"
        ; make "test37.tig"
        ; make "test38.tig"
        ; make "test39.tig"
        ; make "test40.tig"
        ; make "test41.tig"
        ; make "test42.tig"
        ; make "test43.tig"
        ; make "test44.tig"
        ; make "test45.tig"
        ; make "test46.tig"
        ; make "test47.tig"
        ; make "test48.tig" (* As far as I know, test49 is the only syntax error *)
        ; make_fails "test49.tig"
        ] )
    ]
;;

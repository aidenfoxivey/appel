let parse_file path =
  let s = open_in path |> Lexing.from_channel in
  Appel.Parser.program Appel.Lexer.read s
;;

let test_parses path () =
  try ignore (parse_file path) with
  | e -> Alcotest.failf "Failed to parse %s: %s" path (Printexc.to_string e)
;;

let () =
  Alcotest.run
    "appel"
    [ ( "parsing"
      , [ Alcotest.test_case
            "merge.tig"
            `Quick
            (test_parses "../sample_programs/merge.tig")
        ; Alcotest.test_case
            "test14.tig"
            `Quick
            (test_parses "../sample_programs/test14.tig")
        ] )
    ]
;;

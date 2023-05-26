open Core
open OUnit2

let test_parser _ =
  let string_of_ast ast = Format.asprintf "%a" JsonParsing.JsonAst.Json.pp ast in
  let json_string =
    {|
    {
      "a": [],
      "b": {
        "c": "",
        "d": 2001,
        "e": "f"
      },
      "g": [{
        "h": "i"
      }]
    }
  |}
  in
  assert_equal
    ~printer:string_of_ast
    (JsonParsing.JsonAst.Json.from_string_exn json_string)
    {
      JsonParsing.JsonAst.Node.location =
        {
          JsonParsing.JsonAst.Location.start =
            { JsonParsing.JsonAst.Location.line = 11; column = 8 };
          stop = { JsonParsing.JsonAst.Location.line = 11; column = 8 };
        };
      value =
        `Assoc
          [
            ( "a",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 3; column = 12 };
                    stop = { JsonParsing.JsonAst.Location.line = 3; column = 12 };
                  };
                value = `List [];
              } );
            ( "b",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 7; column = 14 };
                    stop = { JsonParsing.JsonAst.Location.line = 7; column = 16 };
                  };
                value =
                  `Assoc
                    [
                      ( "c",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 5; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 5; column = 15 };
                            };
                          value = `String "";
                        } );
                      ( "d",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 6; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 6; column = 17 };
                            };
                          value = `Float 2001.;
                        } );
                      ( "e",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 7; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 7; column = 16 };
                            };
                          value = `String "f";
                        } );
                    ];
              } );
            ( "g",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 11; column = 7 };
                    stop = { JsonParsing.JsonAst.Location.line = 11; column = 7 };
                  };
                value =
                  `List
                    [
                      {
                        JsonParsing.JsonAst.Node.location =
                          {
                            JsonParsing.JsonAst.Location.start =
                              { JsonParsing.JsonAst.Location.line = 10; column = 14 };
                            stop = { JsonParsing.JsonAst.Location.line = 10; column = 16 };
                          };
                        value =
                          `Assoc
                            [
                              ( "h",
                                {
                                  JsonParsing.JsonAst.Node.location =
                                    {
                                      JsonParsing.JsonAst.Location.start =
                                        { JsonParsing.JsonAst.Location.line = 10; column = 14 };
                                      stop = { JsonParsing.JsonAst.Location.line = 10; column = 16 };
                                    };
                                  value = `String "i";
                                } );
                            ];
                      };
                    ];
              } );
          ];
    }


let () = "jsonParsing" >::: ["parser" >:: test_parser] |> Test.run

(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

let base_command_line_arguments =
  Command.Spec.(
    empty
    +> flag "-verbose" no_arg ~doc:"Turn on verbose logging"
    +> flag "-version" (optional string) ~doc:"VERSION Pyre version"
    +> flag
      "-logging-sections"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"SECTION1,... Comma-separated list of logging sections."
    +> flag "-debug" no_arg ~doc:"Turn on debug mode"
    +> flag "-strict" no_arg ~doc:"Turn on strict mode"
    +> flag "-declare" no_arg ~doc:"Turn on declare mode"
    +> flag "-show-error-traces" no_arg ~doc:"Outputs additional error information"
    +> flag "-infer" no_arg ~doc:"Outputs extra information and errors for inference purposes"
    +> flag
      "-recursive-infer"
      no_arg
      ~doc:"Recursively run infer until no new annotations are generated."
    +> flag "-analyze" no_arg ~doc:"Analyze the code after type checking"
    +> flag "-sequential" no_arg ~doc:"Turn off parallel processing (parallel on by default)."
    +> flag
      "-workers"
      (optional_with_default 4 int)
      ~doc:"Number of workers to use in parallel processing."
    +> flag
      "-log-identifier"
      (optional_with_default "" string)
      ~doc:"IDENTIFIER Add given identifier to logged samples."
    +> flag
      "-project-root"
      (optional_with_default "/" string)
      ~doc:"ROOT Only check sources under this root directory."
      ~aliases:["-type-check-root"]
    +> flag
      "-search-path"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"DIRECTORY1,... Directories containing stubs and external modules to include"
    +> anon (maybe_with_default "." ("source-root" %: string)))

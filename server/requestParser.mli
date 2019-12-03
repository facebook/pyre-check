val parse_and_translate
  :  configuration:Configuration.Analysis.t ->
  state:State.t ->
  request:Yojson.Safe.t ->
  Protocol.Request.t option

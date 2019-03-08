(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module SharedMemory = Memory


type rule = {
  sources: Sources.t list;
  sinks: Sinks.t list;
  code: int;
  name: string;
  message_format: string;  (* format *)
}


type t = {
  sinks: string list;
  sources: string list;
  features: string list;
  rules: rule list;
}


(* There's only a single taint configuration *)
let key = "root"


module SharedConfig = SharedMemory.WithCache
    (String)
    (struct
      type nonrec t = t
      let prefix = Prefix.make ()
      let description = "Taint configuration"
    end)


let get () =
  match SharedConfig.get(key) with
  | None ->
      {
        sources = [
        ];
        sinks = [
        ];
        features = [
        ];
        rules = [
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.RemoteCodeExecution ];
            code = 5001;
            name = "Possible shell injection.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.Test; Sources.UserControlled ];
            sinks = [ Sinks.Test ];
            code = 5002;
            name = "Test flow.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.Thrift ];
            code = 5003;
            name = "User controlled data to thrift.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.Thrift ];
            sinks = [ Sinks.RemoteCodeExecution ];
            code = 5004;
            name = "Thrift return data to remote code execution.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.SQL ];
            code = 5005;
            name = "User controlled data to SQL execution.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.Cookies; Sources.PII; Sources.Secrets ];
            sinks = [ Sinks.Logging ];
            code = 5006;
            name = "Restricted data being logged.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.XMLParser ];
            code = 5007;
            name = "User data to XML Parser.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.XSS ];
            code = 5008;
            name = "XSS";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.Demo ];
            sinks = [ Sinks.Demo ];
            code = 5009;
            name = "Demo flow.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.GetAttr ];
            code = 5010;
            name = "User data to getattr.";
            message_format = "Attacker may control at least one argument to getattr(,)."
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.FileSystem ];
            code = 5011;
            name = "User data to filesystem operation.";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.RequestSend ];
            code = 5012;
            name = "Potential Server-side request forgery (SSRF)";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.IdentityCreation ];
            code = 5013;
            name = "User-controlled identity creation (experimental)";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
          {
            sources = [ Sources.UserControlled ];
            sinks = [ Sinks.ODS ];
            code = 5014;
            name = "User-controlled ODS key";
            message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
          };
        ];
      }
  | Some configuration ->
      configuration

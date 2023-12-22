(*
 * Copyright (C) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Tools like 'bear' can be used to generate this file,
   however they require to rebuild the C files with dune caching disabled,
   and cannot be added to dune itself (it would invoke dune recursively to build files).

   Also the output from 'bear' doesn't work well with generated headers,
   it needs an `-iquote .` flag to make 'clangd' look in the right folder.

   This tool expects 'dune rules' on input and will output a compilation database.
*)

open Sexplib

let debug = ref false

let output_directory = ref ""

let () =
  let usage_msg =
    Printf.sprintf "Usage: dune rules | %s [-d] [-o directory]"
      Sys.executable_name
  in
  let speclist =
    [
      ("-d", Arg.Set debug, "Enable debug output")
    ; ( "-o"
      , Arg.Set_string output_directory
      , "Output directory for compile_commands.json"
      )
    ]
  in
  Arg.parse speclist ignore usage_msg

(* Applicative syntactic sugar for {!module:Option}. *)

let ( let+ ) t f = Option.map f t

let ( and+ ) a b = match (a, b) with Some a, Some b -> Some (a, b) | _ -> None

(** [cwd] is the absolute path of the current working directory *)
let cwd = Fpath.(Sys.getcwd () |> v)

(** [project_root] is the relative path from the current directory to the project root.

  The project root is the directory that contains `dune-project`.
*)
let project_root =
  let rec find_dune_project path =
    if Fpath.is_root Fpath.(cwd // path |> normalize) then
      (* avoid infinite ../../../ tests *)
      failwith
        "Cannot find `dune-project` in any of the parent directories. This \
         command must be run inside a dune project."
    else if Sys.file_exists Fpath.(path / "dune-project" |> to_string) then (
      if !debug then
        Format.eprintf "Found dune-project at %a@.CWD is %a@." Fpath.pp path
          Fpath.pp cwd ;
      path
    ) else
      find_dune_project (Fpath.parent path)
  in
  find_dune_project Fpath.(v ".")

let () =
  if !debug then Format.eprintf "Project root: %a@." Fpath.pp project_root

(** [project_path rule_path] returns [rule_path] relative to the current directory. *)
let project_path path = Fpath.(project_root // v path |> normalize)

module File = struct
  type t = Fpath.t

  (** [of_sexp sexp] parses a file dependency.

     [(File (In_source_tree example/dune/foostubs.c))]
   *)
  let of_sexp =
    let open Sexp in
    function
    | List [Atom "File"; List [Atom _; Atom path]] ->
        Some (project_path path)
    | file ->
        if !debug then Format.eprintf "Could not parse: %a@." Sexp.pp_hum file ;
        None
end

module Target = struct
  type t = Fpath.t

  let fpath_of_sexp s = s |> Conv.string_of_sexp |> project_path

  (** [of_sexp sexp] parses a Target.
  
     [(files (file1.c))]
     [(In_build_dir file1.c)]
  *)
  let of_sexp =
    let open Sexp in
    function
    | List [Atom "files"; List files] ->
        Conv.list_of_sexp fpath_of_sexp (List files)
    | List (Atom "In_build_dir" :: files) ->
        Conv.list_of_sexp fpath_of_sexp (List files)
    | sexp ->
        if !debug then
          Format.eprintf "Could not parse target: %a@." Sexp.pp_hum sexp ;
        []
end

module Action = struct
  type t =
    | ChdirRun of {dir: Fpath.t; run: string list}
    | Copy of {source: Fpath.t; target: Fpath.t}

  (** [of_sexp sexp] parses an action.
  
   [(chdir  _build/default/example/dune (run /usr/lib64/ccache/gcc ...))]
   [(copy example/Makefile/foostubs.c _build/default/example/Makefile/foostubs.c)]
  *)
  let of_sexp_list =
    let open Sexp in
    function
    | [Atom "chdir"; Atom dir; List (Atom "run" :: run)] ->
        Some
          (ChdirRun
             {
               dir= Fpath.v dir
             ; run= Conv.list_of_sexp Conv.string_of_sexp (List run)
             }
          )
    | [Atom "copy"; Atom source; Atom target] ->
        let source_dir, source_file = Fpath.(v source |> split_base) in
        Some
          (Copy
             {
               source=
                 Fpath.(
                   (source_dir // project_root |> normalize) // source_file
                   |> normalize
                 )
             ; target= project_path target
             }
          )
    | sexp ->
        if !debug then
          Format.eprintf "@[<v2>Unknown action:@,%a@]@." Sexp.pp_hum (List sexp) ;
        None
end

module Rule = struct
  type t = {deps: File.t list; targets: Target.t list; action: Action.t}

  (** [of_sexp sexp] parses a dune rule.

    [((deps (...)) (targets (...)) (action (...)))]
    [((deps (...)) (targets (...)) (context ...) (action (...)))]

    See also [dune rules --help]
  *)
  let of_sexp =
    let open Sexp in
    function
    | ( List
          [
            List [Atom "deps"; List deps]
          ; List [Atom "targets"; List targets]
          ; List [Atom "action"; List action]
          ]
      | List
          [
            List [Atom "deps"; List deps]
          ; List [Atom "targets"; List targets]
          ; List (Atom "context" :: _)
          ; List [Atom "action"; List action]
          ] ) as sexp ->
        if !debug then Format.eprintf "@[<v2>Parsing rule:@,%a@]@." pp_hum sexp ;

        let deps = List.filter_map File.of_sexp deps in
        if !debug then
          Format.eprintf "@[<v2>Dependencies:@,%a@]@."
            (Format.pp_print_list Fpath.pp)
            deps ;

        let targets = List.rev_map Target.of_sexp targets |> List.concat in
        if !debug then
          Format.eprintf "@[<v2>Targets:@,%a@]@."
            (Format.pp_print_list Fpath.pp)
            targets ;

        let+ action = Action.of_sexp_list action in
        {deps; targets; action}
    | sexp ->
        if !debug then
          Format.eprintf "@[<v2>Could not parse rule:@,%a@]@." pp_hum sexp ;
        None

  let copy_rule = function
    | {action= Action.Copy {source; target}; _} ->
        Some (target, source)
    | _ ->
        None

  (** [source_map_of rules] builds a map from targets to sources by looking at [copy] actions. *)
  let source_map_of rules =
    rules |> List.to_seq |> Seq.filter_map copy_rule |> Fpath.Map.of_seq

  let pp_source_map =
    Fpath.Map.pp @@ fun ppf (dst, src) ->
    Format.fprintf ppf "%a <- %a" Fpath.pp dst Fpath.pp src
end

(* Caveats: normalization may not work well if there are directory symlinks. *)

let build_root_dir = Fpath.(cwd // project_root |> normalize)

module CompileCommand = struct
  (** a {{: https://clang.llvm.org/docs/JSONCompilationDatabase.html]}compile_commands.json} entry.

    Optional entries are ommitted.
  *)
  type t = {
      directory: Fpath.t  (** working dir of compilation *)
    ; arguments: string list
          (** compilation command as a list of strings. All paths relative to [directory]. *)
    ; file: Fpath.t  (** the main compilation unit *)
  }

  (** [of_rule rules] generates a compile_commands entry from [rules].

    @params rules are all the rules for the current target
   *)
  let of_rule ~source_map rule =
    match rule with
    | Rule.{action= Action.(ChdirRun {dir; run= cmd :: args}); targets; deps} ->
        let+ c_file =
          List.find_opt (Fpath.mem_ext ["c"; "cpp"; "cc"; "cxx"]) deps
        and+ _output = List.find_opt (Fpath.has_ext "o") targets in
        let file =
          Fpath.Map.find_opt c_file source_map |> Option.value ~default:c_file
        in

        if !debug then
          Format.eprintf "file: %a, c_file: %a@." Fpath.pp file Fpath.pp c_file ;

        let directory = Fpath.(build_root_dir // dir) in
        (* -iquote . is needed to make 'clangd' find the generated header,
           all paths are relative to the build dir, so '.' will get replaced with the build dir path.
           Normally a compiler would search in the current dir for quoted includes, but the editor/LSP is not running in the build dir, but the source dir!
        *)
        {file; directory; arguments= cmd :: "-iquote" :: "." :: args}
    | _ ->
        None

  let fpath p = p |> Fpath.to_string |> Ezjsonm.string

  let json_of t =
    Ezjsonm.(
      dict
        [
          ("arguments", list string t.arguments)
        ; ("directory", fpath t.directory)
        ; ("file", fpath t.file)
        ]
    )
end

let parse_rules_from_stdin () =
  stdin |> Sexp.input_rev_sexps |> List.filter_map Rule.of_sexp

let with_output f =
  let outfile_name =
    Filename.concat !output_directory "compile_commands.json"
  in
  if !debug then Format.eprintf "\nSaving to file %s\n" outfile_name ;
  let json = open_out outfile_name in
  Fun.protect ~finally:(fun () -> close_out json) (fun () -> f json)

let () =
  let rules = parse_rules_from_stdin () in

  let source_map = Rule.source_map_of rules in
  if !debug then
    Format.eprintf "@[<v2>Source map:@,%a@]@." Rule.pp_source_map source_map ;

  let entries =
    rules |> List.filter_map @@ CompileCommand.of_rule ~source_map
  in
  let n = List.length entries in
  if !debug then Format.eprintf "Compilation database has %d entries" n ;
  if n = 0 then
    Format.eprintf
      "Warning: No rules found for compiling C files.@,\
      \  Check that your project declares foreign libraries.@." ;

  with_output @@ fun json ->
  entries
  |> Ezjsonm.list CompileCommand.json_of
  |> Ezjsonm.to_channel ~minify:false json

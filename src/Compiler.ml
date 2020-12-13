type middle_end = Closure | Flambda

let middle_end = if Config.flambda then Flambda else Closure

let filename_of_modname modname = modname ^ ".ml"

let cmxname_of_modname modname = modname ^ ".cmx"

let cminame_of_modname modname = modname ^ ".cmi"

let prefixname_of_modname modname = modname

let type_implementation env modname code =
  Typemod.type_implementation
    (filename_of_modname modname)
    (prefixname_of_modname modname)
    modname env code

let transl_implementation =
  match middle_end with
  | Closure -> Translmod.transl_store_implementation
  | Flambda -> Translmod.transl_implementation_flambda

let simplf_lambda lambda =
  Lambda.{ lambda with code = Simplif.simplify_lambda lambda.code }

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'

  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx

  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int

  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend : (module Backend_intf.S) = (module Backend)

let lambda_to_clambda modname =
  let lambda_to_clambda =
    match middle_end with
    | Closure -> Closure_middle_end.lambda_to_clambda
    | Flambda -> Flambda_middle_end.lambda_to_clambda
  in
  lambda_to_clambda ~backend
    ~filename:(filename_of_modname modname)
    ~prefixname:(prefixname_of_modname modname)
    ~ppf_dump:Format.std_formatter

let make_initial_env () =
  (* TODO: -O2 and -O3 parameters*)
  Clflags.native_code := true;
  Compmisc.init_path ();
  Compmisc.initial_env ()

let generate_cmm ~env ~modname code =
  Compilenv.reset modname;
  let cmm =
    code
    |> type_implementation env modname
    |> transl_implementation modname
    |> simplf_lambda |> lambda_to_clambda modname |> Cmmgen.compunit
  in
  Compilenv.save_unit_info (cmxname_of_modname modname);
  (* TODO: maybe cmx in memory? *)
  cmm

let generate_asm ~file cmm =
  let oc = open_out file in
  Emitaux.output_channel := oc;
  Emit.begin_assembly ();
  cmm |> List.iter (Asmgen.compile_phrase ~ppf_dump:Format.std_formatter);
  Emit.end_assembly ();
  close_out oc

let link ~asm_file ~output =
  let command = Printf.sprintf "gcc -shared -o %s %s" output asm_file in
  match Sys.command command with
  | 0 -> ()
  | code -> raise (Failure (Printf.sprintf "%s failed with %d" command code))

let generate_asm_and_link ~output ~modname cmm =
  let asm_file = Filename.temp_file modname ".s" in
  (* Format.eprintf "asm: %s\n%!" asm_file; *)
  generate_asm ~file:asm_file cmm;
  link ~asm_file ~output;
  Sys.remove asm_file

let load_cmi ~modname = Cmi_format.read_cmi (cminame_of_modname modname)

let load_cmx ~modname =
  let cmx, _ = Compilenv.read_unit_info (cmxname_of_modname modname) in
  cmx

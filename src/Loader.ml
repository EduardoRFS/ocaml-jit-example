open Register

let modname = "Loaded_by_loader"

(* TODO: type safe on call_pointer *)
(* this box ensures the parameter count to be 1 when expecting tuples *)
type 'a box = Box of 'a

let call_pointer input pointer =
  let id = Sys.opaque_identity (fun v -> v) in
  let call : pointer ref = Obj.magic id in
  call := pointer;
  let call : 'a -> 'b = Obj.magic call in
  call (Box input)

(* TODO: enforce all parameters are boxed *)
let assert_is_function expr =
  match expr.Ppxlib.pexp_desc with
  | Pexp_fun _ -> ()
  | _ -> raise (Invalid_argument "expression input should be a function")

(* TODO: assert no data on cmm *)

let wrap_fun expr =
  let open Ppxlib in
  assert_is_function expr;
  let loc = expr.pexp_loc in
  let code =
    [%str
      type 'a box = Box of 'a

      let wrapper_func (Box input) = [%e expr] input]
  in
  Ppxlib_ast.Selected_ast.To_ocaml.copy_structure code

let generate_cmm code =
  let env = Compiler.make_initial_env () in
  let code = wrap_fun code in
  Compiler.generate_cmm ~env ~modname code

let find_symbol_in_cmx cmx _name =
  let open Cmx_format in
  (* TODO: how to do this properly *)
  match cmx.ui_export_info with
  | Clambda (Value_tuple [| Value_closure (func_dec, _) |]) ->
      (* TODO: at least check if the name is right *)
      let found_name = func_dec.fun_label in
      found_name
  | _ -> assert false

let load_module ~so_file cmx =
  let library = Dl.dlopen ~filename:so_file ~flags:[ RTLD_LOCAL; RTLD_LAZY ] in
  let frametable =
    let symbol = "caml" ^ cmx.Cmx_format.ui_symbol ^ "__frametable" in
    Dl.dlsym ~handle:library ~symbol
  in

  register_frametable frametable;
  let collect () =
    unregister_frametable frametable;
    Dl.dlclose ~handle:library
  in
  (library, collect)

let load_only_module cmm =
  let so_file = Filename.temp_file modname ".so" in
  Compiler.generate_asm_and_link ~output:so_file ~modname cmm;

  (* load module *)
  let cmx = Compiler.load_cmx ~modname in
  load_module ~so_file cmx

let load cmm =
  let so_file = Filename.temp_file modname ".so" in
  Compiler.generate_asm_and_link ~output:so_file ~modname cmm;

  (* load module *)
  let cmx = Compiler.load_cmx ~modname in
  let library, collect = load_module ~so_file cmx in

  (* load the function *)
  let symbol = find_symbol_in_cmx cmx "wrapper_func" in
  let pointer = Dl.dlsym ~handle:library ~symbol |> nativeint_to_pointer in
  let fn v = call_pointer v pointer in

  (* collect and remove so_file when reference is lost *)
  Gc.finalise_last
    (fun () ->
      collect ();
      Sys.remove so_file)
    fn;
  fun v -> fn v

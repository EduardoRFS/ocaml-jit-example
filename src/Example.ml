open Ppxlib
open Loader

let loc = Location.none

type ('b, 'a) instr =
  | Swap : ('a * ('b * 'rest), 'b * ('a * 'rest)) instr
  | Sub : (int * (int * 'rest), int * 'rest) instr
  | Add : (int * (int * 'rest), int * 'rest) instr
  | Car : (('a * _) * 'rest, 'a * 'rest) instr
  | Seq : ('b, 'trans) instr * ('trans, 'a) instr -> ('b, 'a) instr

module Interpreter = struct
  let rec eval : type b a. (b, a) instr -> b -> a =
   fun instr v ->
    match (instr, v) with
    | Swap, (a, (b, rest)) -> (b, (a, rest))
    | Sub, (a, (b, rest)) -> (a - b, rest)
    | Add, (a, (b, rest)) -> (a + b, rest)
    | Car, ((a, _), rest) -> (a, rest)
    | Seq (cur, next), stack ->
        let stack = eval cur stack in
        eval next stack
end

module Compile = struct
  let rec compile : type b a. (b, a) instr -> expression -> expression =
   fun instr after ->
    match instr with
    | Swap ->
        [%expr
          let a, (b, rest) = stack in
          let stack = (b, (a, rest)) in
          [%e after]]
    | Sub ->
        [%expr
          let a, (b, rest) = stack in
          let stack = (a - b, rest) in
          [%e after]]
    | Add ->
        [%expr
          let a, (b, rest) = stack in
          let stack = (a + b, rest) in
          [%e after]]
    | Car ->
        [%expr
          let (a, _), rest = stack in
          let stack = (a, rest) in
          [%e after]]
    | Seq (cur, next) ->
        let cur = compile cur in
        let next = compile next after in
        cur next

  let compile instr =
    let body = compile instr [%expr stack] in
    [%expr fun stack -> [%e body]]

  let compile code =
    let code = compile code in
    (* store .ml *)
    let oc = open_out (Compiler.filename_of_modname modname) in
    Ppxlib.Pprintast.string_of_expression code |> output_string oc;
    close_out oc;

    (* compile to asm *)
    generate_cmm code
end

let code = Sys.opaque_identity (Seq (Swap, Seq (Car, Seq (Sub, Add))))

let eval_interpreter a b c = Interpreter.eval code (a, ((b, c), (c, ())))

let cmm = Compile.compile code

let eval_compiled =
  let eval_compiled_code : int * ((int * int) * (int * unit)) -> int * unit =
    load cmm
  in
  fun a b c -> eval_compiled_code (a, ((b, c), (c, ())))

let main =
  Random.self_init ();
  let a = Random.int 100 in
  let b = Random.int 100 in
  let c = Random.int 100 in

  let x, () = eval_interpreter a b c in
  Printf.printf "interpreted: %d\n%!" x;

  let x, () = eval_compiled a b c in
  Printf.printf "compiled: %d\n%!" x;

  let open Core in
  let open Core_bench in
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"eval_interpreter" (fun () ->
             eval_interpreter a b c);
         Bench.Test.create ~name:"eval_compiled" (fun () -> eval_compiled a b c);
       ]);
  Stdlib.flush_all ();

  (* this should validate the unload efficacy of Register *)
  let load_library () =
    let so_file = Filename.temp_file modname ".so" in
    Compiler.generate_asm_and_link ~output:so_file ~modname cmm;
    let cmx = Compiler.load_cmx ~modname in
    fun () -> Loader.load_module ~so_file cmx
  in
  let load_module_a = load_library () in
  let load_module_b = load_library () in
  let collect_a = ref (fun () -> ()) in
  let collect_b = ref (fun () -> ()) in
  for i = 0 to 1_000_000 do
    let collect, load_module =
      if i mod 2 = 0 then (collect_a, load_module_a)
      else (collect_b, load_module_b)
    in
    !collect ();
    let _library, new_collect = load_module () in
    collect := new_collect
  done

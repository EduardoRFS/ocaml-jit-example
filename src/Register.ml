type pointer

let nativeint_to_pointer : nativeint -> pointer =
 fun pointer -> Obj.field (Obj.repr pointer) 1 |> Obj.obj

external register_frametable : pointer -> unit = "caml_register_frametable"

external unregister_frametable : pointer -> unit = "caml_unregister_frametable"

let unregister_frametable pointer = unregister_frametable pointer

(* unregister_frametable has a bug,
   if you try to remove the top position of the frametable it will destroy the frametable,
   this should be fixed in the compiler in the future
   so we hold the last pointer and if it should be collected
   and check during register and unregister to see if we can unregister *)

let last_registered_table : (bool * nativeint) ref =
  ref (false, Nativeint.of_int 0)

let unregister_frametable table =
  let _, old_table = !last_registered_table in

  if old_table = table then last_registered_table := (true, old_table)
  else unregister_frametable (nativeint_to_pointer table)

let register_frametable table =
  let should_collect, old_table = !last_registered_table in
  if table = old_table then
    raise
      (Invalid_argument
         "called two times with the same pointer and that is a problem")
  else register_frametable (nativeint_to_pointer table);

  last_registered_table := (false, table);
  if should_collect then unregister_frametable old_table

(* TODO: naked pointers? Not sure *)

module Option = struct
  let ( let+ ) = Option.bind
end

let bimap f g (a1, a2) = (f a1, g a2)

let update_store (computer : Lib.Machine.t)
    (store : Lib.Machine.Cell.t Lib.Machine.Store.t) : Lib.Machine.t =
  { computer with store }

let _ =
  let open Option in
  let+ content =
    In_channel.with_open_text "examples/triangular.krt" (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Lib.Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries -> (
      let compiler, computer =
        Lib.Machine.initialize () |> fun initialComputer ->
        Lib.Compiler.compile
          ( Lib.Preprocessor.group_clauses decls_queries,
            Lib.Compiler.initialize (),
            initialComputer.store )
        |> bimap Fun.id (update_store initialComputer)
      in
      match compiler.entry_point with
      | None -> None
      | Some entry_point ->
          Some
            (Lib.Evaluator.eval compiler.functor_table
               { computer with p_register = entry_point.p_register }))

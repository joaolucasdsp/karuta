open Machine
open Machine.Cell

let set_register (register : Cell.register) (cell : Cell.t)
    ({ store; x_registers; e_register; _ } as computer) : Machine.t =
  match register with
  | Cell.X index_of_register ->
      let open Machine.IntMap in
      let x_registers = add index_of_register cell x_registers in
      { computer with x_registers }
  | Cell.Y index_of_register -> (
      let stack_frame_size = Store.stack_get store (e_register + 2) in
      match stack_frame_size with
      | ArgCount size ->
          if index_of_register < size then
            {
              computer with
              store =
                Store.stack_put cell (e_register + 3 + index_of_register) store;
            }
          else failwith "stack overflow"
      | _ -> failwith "invalid stack size (Not ArgCount)")

let get_register (register : Cell.register)
    { store; x_registers; e_register; _ } : Cell.t =
  match register with
  | Cell.X index_of_register ->
      let open Machine.IntMap in
      find index_of_register x_registers
  | Cell.Y index_of_register -> (
      let stack_frame_size = Store.stack_get store (e_register + 2) in
      match stack_frame_size with
      | ArgCount size ->
          if index_of_register < size then
            Store.stack_get store (e_register + 3 + index_of_register)
          else failwith "stack overflow"
      | _ -> failwith "invalid stack size (Not ArgCount)")

let put_structure (x_register : Cell.register) (functor_label, functor_arity)
    ({ store; h_register; _ } as computer) : Machine.t =
  let func = Functor (functor_label, functor_arity) in
  let structure = Machine.Cell.Structure h_register in
  store
  |> Store.heap_put func h_register
  |> (fun store -> { computer with store; h_register = h_register + 1 })
  |> set_register x_register structure

let set_variable (register : Cell.register)
    ({ store; h_register; _ } as computer) =
  let reference = Reference h_register in
  store
  |> Store.heap_put reference h_register
  |> (fun store -> { computer with store; h_register = h_register + 1 })
  |> set_register register reference

let set_value (register : Cell.register) ({ store; h_register; _ } as computer)
    =
  let value_of_register = get_register register computer in
  store |> Store.heap_put value_of_register h_register |> fun store ->
  { computer with store; h_register = h_register + 1 }

type address = int

let rec deref (a : address) store : address =
  let cell = Store.get store a in
  match cell with
  | Reference value when value <> a -> deref value store
  | _ -> a

let is_reference (cell : Machine.Cell.t) : bool =
  match cell with Reference _ -> true | _ -> false

let trail (a : address)
    ({ hb_register; h_register; b_register; tr_register; store; _ } as computer :
      Machine.t) : Machine.t =
  if a < hb_register || (h_register < a && a < b_register) then
    {
      computer with
      tr_register = tr_register + 1;
      store = Store.trail_put (Cell.Address a) tr_register store;
    }
  else computer

let bind (a1 : address) (a2 : address) ({ store; _ } as computer : Machine.t) :
    Machine.t =
  let t1 = match Store.get store a1 with Functor _ -> Structure a1 | v -> v in
  let t2 = match Store.get store a2 with Functor _ -> Structure a2 | v -> v in
  if is_reference t1 && ((not (is_reference t2)) || a2 < a1) then
    { computer with store = store |> Store.put t2 a1 } |> trail a1
  else { computer with store = store |> Store.put t1 a2 } |> trail a2

let get_structure ((functor_label, functor_arity) : string * int)
    (register : Cell.register) ({ store; h_register; _ } as computer) :
    Machine.t =
  match get_register register computer with
  | Reference address -> (
      let addr = deref address store in
      match Store.get store addr with
      | Reference _ ->
          let structure = Structure (h_register + 1) in
          let func = Functor (functor_label, functor_arity) in
          let computer =
            {
              computer with
              store =
                store
                |> Store.heap_put structure h_register
                |> Store.heap_put func (h_register + 1);
            }
            |> bind addr h_register
          in
          { computer with h_register = h_register + 2; mode = Write }
      | Structure a -> (
          match Store.heap_get store a with
          | Functor (label, arity)
            when label = functor_label && arity = functor_arity ->
              { computer with s_register = a + 1; mode = Read }
          | _ -> { computer with fail = true })
      | _ -> { computer with fail = true })
  | Structure a -> (
      match Store.heap_get store a with
      | Functor (label, arity)
        when label = functor_label && arity = functor_arity ->
          { computer with s_register = a + 1; mode = Read }
      | _ -> { computer with fail = true })
  | _ -> failwith "unreachable get_structure"

let unify_variable (register : Cell.register)
    ({ store; h_register; s_register; mode; _ } as computer) : Machine.t =
  match mode with
  | Read ->
      let value = Store.heap_get store s_register in
      set_register register value { computer with s_register = s_register + 1 }
  | Write ->
      let reference = Reference h_register in
      store
      |> Store.heap_put reference h_register
      |> (fun store ->
           {
             computer with
             store;
             h_register = h_register + 1;
             s_register = s_register + 1;
           })
      |> set_register register reference

let unify (a1 : address) (a2 : address) ({ store; _ } as computer) : Machine.t =
  let preparedComputer =
    store |> Store.pdl_push (Address a1) |> Store.pdl_push (Address a2)
    |> fun store -> { computer with store; fail = false }
  in
  let rec loop ({ store; fail; _ } as computer) : Machine.t =
    if Store.pdl_empty store || fail then computer
    else
      let Address p1, store = Store.pdl_pop store in
      let Address p2, store = Store.pdl_pop store in
      let d1 = deref p1 store in
      let d2 = deref p2 store in
      if d1 != d2 then
        match (Store.get store d1, Store.get store d2) with
        | Reference _, _ | _, Reference _ ->
            loop @@ bind d1 d2 { computer with store }
        | Functor (s1, n1), Functor (s2, n2) ->
            let open Batteries in
            if s1 = s2 && n1 = n2 then
              loop
                {
                  computer with
                  store =
                    List.fold_left
                      (fun store i ->
                        store
                        |> Store.pdl_push (Address (d1 + i))
                        |> Store.pdl_push (Address (d2 + i)))
                      store
                      (List.of_enum (1 -- n1));
                }
            else { computer with fail = true }
        | _, _ -> { computer with fail = true }
      else loop computer
  in
  loop preparedComputer

let unify_value (register : Cell.register)
    ({ store; h_register; s_register; mode; _ } as computer) : Machine.t =
  match mode with
  | Read -> (
      match get_register register computer with
      | Reference addr ->
          { (unify addr s_register computer) with s_register = s_register + 1 }
      | _ -> failwith "unreachable unify_value")
  | Write ->
      let value_of_register = get_register register computer in
      store |> Store.heap_put value_of_register h_register |> fun store ->
      {
        computer with
        store;
        h_register = h_register + 1;
        s_register = s_register + 1;
      }

let instruction_size = 1

let put_variable (register : Cell.register) (a_register : Cell.register)
    ({ store; e_register; h_register; _ } as computer) : Machine.t =
  match register with
  | X _ ->
      let reference = Reference h_register in
      store
      |> Store.heap_put reference h_register
      |> (fun store -> { computer with store; h_register = h_register + 1 })
      |> set_register a_register reference
      |> set_register register reference
  | Y y_register ->
      let addr = e_register + y_register + 3 in
      let reference = Reference addr in
      store |> Store.stack_put reference addr |> fun store ->
      set_register a_register reference { computer with store }

let deref_cell (cell : Cell.t) store : Cell.t =
  match cell with
  | Reference address -> Store.get store (deref address store)
  | Structure _ -> cell
  | _ -> failwith "unreachable deref_cell"

let put_value (register : Cell.register) (a_register : Cell.register) computer :
    Machine.t =
  let value = get_register register computer in
  match register with
  | Y _ -> (
      let derefed_cell = deref_cell value computer.store in
      match derefed_cell with
      | Reference address when address < computer.e_register ->
          set_register a_register (Store.get computer.store address) computer
      | Reference address ->
          let { h_register; store; _ } = computer in
          let reference = Reference h_register in
          store
          |> Store.heap_put reference h_register
          |> (fun store ->
               bind address h_register
                 { computer with store; h_register = h_register + 1 })
          |> set_register a_register reference
      | _ -> set_register a_register value computer)
  | X _ -> set_register a_register value computer

let get_variable (x_register : Cell.register) (a_register : Cell.register)
    computer : Machine.t =
  let value = get_register a_register computer in
  set_register x_register value computer

let get_value = unify

let deallocate ({ e_register; store; _ } as computer) : Machine.t =
  if e_register < Store.stack_start then
    failwith "tried to return from top level"
  else
    let cp_register = Store.stack_get store (e_register + 1) in
    let e_register = Store.stack_get store e_register in
    match (cp_register, e_register) with
    | Cell.Address cp_register, Cell.Address e_register ->
        { computer with cp_register; e_register }
    | _ -> failwith "unreachable deallocate"

let new_stack_pointer { e_register; b_register; store; _ } : int =
  if b_register < e_register then
    if e_register < Store.stack_start then Store.stack_start
    else
      match Store.stack_get store (e_register + 2) with
      | Cell.ArgCount n -> n + e_register + 3
      | _ -> failwith "unreachable new_stack_pointer 0"
  else
    match Store.stack_get store b_register with
    | Cell.ArgCount n -> n + b_register + 7
    | _ -> failwith "unreachable new_stack_pointer 1"

let try_me_else (l : int)
    ({
       e_register;
       b_register;
       cp_register;
       h_register;
       tr_register;
       p_register;
       store;
       arg_count;
       x_registers;
       _;
     } as computer) : Machine.t =
  let new_b = new_stack_pointer computer in
  let save_arguments store =
    let folder acc idx =
      let arg =
        let open Machine.IntMap in
        find idx x_registers
      in
      Store.stack_put arg (new_b + idx + 1) acc
    in
    let open Batteries in
    List.fold_left folder store (List.of_enum (0 --^ arg_count))
  in
  store
  |> Store.stack_put (Machine.Cell.ArgCount arg_count) new_b
  |> save_arguments
  |> Store.stack_put (Machine.Cell.Address e_register) (new_b + arg_count + 1)
  |> Store.stack_put (Machine.Cell.Address cp_register) (new_b + arg_count + 2)
  |> Store.stack_put (Machine.Cell.Address b_register) (new_b + arg_count + 3)
  |> Store.stack_put (Machine.Cell.Address l) (new_b + arg_count + 4)
  |> Store.stack_put (Machine.Cell.Address tr_register) (new_b + arg_count + 5)
  |> Store.stack_put (Machine.Cell.Address h_register) (new_b + arg_count + 6)
  |> fun store ->
  {
    computer with
    b_register = new_b;
    hb_register = h_register;
    p_register = p_register + instruction_size;
    store;
  }

let unwind_trail (a1 : int) (a2 : int) (store : Cell.t Store.t) : Cell.t Store.t
    =
  let folder acc idx =
    let trail_value = Store.trail_get acc idx |> Cell.address_from_cell in
    let cell = Cell.Reference trail_value in
    Store.put cell trail_value acc
  in
  let open Batteries in
  List.fold_left folder store (List.of_enum (a1 --^ a2))

let retry_me_else (l : int)
    ({ b_register; p_register; store; tr_register; x_registers; _ } as computer)
    : Machine.t =
  let n =
    match Store.stack_get store b_register with
    | Cell.ArgCount count -> count
    | _ -> failwith "unreachable retry-me-else 0"
  in
  let restored_registers =
    let folder acc idx =
      let arg =
        match Store.stack_get store (b_register + idx + 1) with
        | Cell.Address _ | Cell.Empty | Cell.ArgCount _ | Cell.Instruction _
        | Cell.Functor _ ->
            failwith "unreachable retry-me-else 1"
        | savable -> savable
      in
      let open Machine.IntMap in
      add idx arg acc
    in
    let open Batteries in
    List.fold_left folder x_registers (List.of_enum (0 --^ n))
  in
  store
  |> Store.stack_put (Machine.Cell.Address l) (b_register + n + 4)
  |> unwind_trail
       (Store.stack_get store (b_register + n + 5) |> Cell.address_from_cell)
       tr_register
  |> fun store ->
  {
    computer with
    store;
    x_registers = restored_registers;
    e_register =
      Store.stack_get store (b_register + n + 1) |> Cell.address_from_cell;
    cp_register =
      Store.stack_get store (b_register + n + 2) |> Cell.address_from_cell;
    tr_register =
      Store.stack_get store (b_register + n + 5) |> Cell.address_from_cell;
    h_register =
      Store.stack_get store (b_register + n + 6) |> Cell.address_from_cell;
    hb_register =
      Store.stack_get store (b_register + n + 6) |> Cell.address_from_cell;
    p_register = p_register + instruction_size;
  }

let trust_me
    ({ b_register; p_register; tr_register; store; x_registers; _ } as computer)
    : Machine.t =
  let n =
    match Store.stack_get store b_register with
    | Cell.ArgCount count -> count
    | _ -> failwith "unreachable trust-me 0"
  in
  let restored_registers =
    let folder acc idx =
      let arg =
        match Store.stack_get store (b_register + idx + 1) with
        | Cell.Address _ | Cell.Empty | Cell.ArgCount _ | Cell.Instruction _
        | Cell.Functor _ ->
            failwith "unreachable trust-me 1"
        | savable -> savable
      in
      let open Machine.IntMap in
      add idx arg acc
    in
    let open Batteries in
    List.fold_left folder x_registers (List.of_enum (0 --^ n))
  in
  store
  |> unwind_trail
       (Store.stack_get store (b_register + n + 5) |> Cell.address_from_cell)
       tr_register
  |> fun store ->
  {
    computer with
    store;
    x_registers = restored_registers;
    e_register =
      Store.stack_get store (b_register + n + 1) |> Cell.address_from_cell;
    cp_register =
      Store.stack_get store (b_register + n + 2) |> Cell.address_from_cell;
    tr_register =
      Store.stack_get store (b_register + n + 5) |> Cell.address_from_cell;
    h_register =
      Store.stack_get store (b_register + n + 6) |> Cell.address_from_cell;
    hb_register =
      Store.stack_get store (b_register + n + 6) |> Cell.address_from_cell;
    b_register =
      Store.stack_get store (b_register + n + 3) |> Cell.address_from_cell;
    p_register = p_register + instruction_size;
  }

let backtrack ({ store; b_register; _ } as computer) : Machine.t =
  let addr =
    match Store.stack_get store b_register with
    | Cell.ArgCount n -> b_register + n + 4
    | _ -> failwith "unreachable backtrack 0"
  in
  match Store.stack_get store addr with
  | Cell.Address n -> { computer with p_register = n; fail = false }
  | _ -> failwith "unreachable backtrack 1"

let allocate (n : int)
    ({ store; e_register; cp_register; p_register; _ } as computer) : Machine.t
    =
  let new_e = new_stack_pointer computer in
  store
  |> Store.stack_put (Cell.Address e_register) new_e
  |> Store.stack_put (Cell.Address cp_register) (new_e + 1)
  |> Store.stack_put (Cell.ArgCount n) (new_e + 2)
  |> fun store ->
  {
    computer with
    store;
    p_register = p_register + instruction_size;
    e_register = new_e;
  }

let call (functor' : Ast.tag * int) (functor_table : Compiler.functor_map)
    ({ p_register; _ } as computer) : Machine.t =
  let open Compiler.FunctorMap in
  {
    computer with
    cp_register = p_register + instruction_size;
    arg_count = snd functor';
    p_register = find functor' functor_table;
  }

let execute (functor' : Ast.tag * int) (functor_table : Compiler.functor_map)
    computer : Machine.t =
  let open Compiler.FunctorMap in
  {
    computer with
    arg_count = snd functor';
    p_register = find functor' functor_table;
  }

let proceed ({ cp_register; _ } as computer) : Machine.t =
  { computer with p_register = cp_register }

let eval_step (functor_table : Compiler.functor_map)
    ({ store; p_register; fail; _ } as computer : Machine.t) : Machine.t * bool
    =
  let open Machine.Cell in
  if fail then (backtrack computer, false)
  else
    match Store.code_get store p_register with
    | Instruction instruction -> (
        match instruction with
        | GetStructure ((name, arity), register) ->
            ( {
                (get_structure (name, arity) register computer) with
                p_register = p_register + 1;
              },
              false )
        | PutStructure ((name, arity), register) ->
            ( {
                (put_structure register (name, arity) computer) with
                p_register = p_register + 1;
              },
              false )
        | PutVariable (x_register, a_register) ->
            ( {
                (put_variable x_register a_register computer) with
                p_register = p_register + 1;
              },
              false )
        | GetVariable (x_register, a_register) ->
            ( {
                (get_variable x_register a_register computer) with
                p_register = p_register + 1;
              },
              false )
        | SetVariable register ->
            ( {
                (set_variable register computer) with
                p_register = p_register + 1;
              },
              false )
        | SetValue register ->
            ( { (set_value register computer) with p_register = p_register + 1 },
              false )
        | UnifyVariable register ->
            ( {
                (unify_variable register computer) with
                p_register = p_register + 1;
              },
              false )
        | PutValue (x_register, a_register) ->
            ( {
                (put_value x_register a_register computer) with
                p_register = p_register + 1;
              },
              false )
        | GetValue (x_register, a_register) -> (
            match
              ( get_register x_register computer,
                get_register a_register computer )
            with
            | ( (Reference x_addr | Structure x_addr),
                (Reference a_addr | Structure a_addr) ) ->
                ( {
                    (get_value x_addr a_addr computer) with
                    p_register = p_register + 1;
                  },
                  false )
            | _ -> failwith "unreachable GetValue")
        | UnifyValue register ->
            ( {
                (unify_value register computer) with
                p_register = p_register + 1;
              },
              false )
        | Allocate n -> (allocate n computer, false)
        | Deallocate ->
            ({ (deallocate computer) with p_register = p_register + 1 }, false)
        | Call predicate -> (call predicate functor_table computer, false)
        | Execute predicate -> (execute predicate functor_table computer, false)
        | Proceed -> (proceed computer, false)
        | Halt -> (computer, true)
        | TryMeElse l -> (try_me_else l computer, false)
        | RetryMeElse l -> (retry_me_else l computer, false)
        | TrustMe -> (trust_me computer, false)
        | Debug ->
            ({ computer with debug = true; p_register = p_register + 1 }, false)
        )
    | _ -> failwith "unreachable eval_step"

let rec eval (functor_table : Compiler.functor_map) (computer : Machine.t) :
    Machine.t =
  if computer.debug then Debugger.run functor_table eval_step eval computer
  else
    let computer, stop = eval_step functor_table computer in
    if stop then computer else eval functor_table computer

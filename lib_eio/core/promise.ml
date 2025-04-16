type 'a state =
  | Resolved of 'a
  | Unresolved of Broadcast.t

type !'a promise = {
  id : Trace.id;
  state : 'a Picos.Computation.t; 
}

type +!'a t
type -!'a u

type 'a or_exn = ('a, exn) result t

let to_public_promise : 'a promise -> 'a t = Obj.magic
let to_public_resolver : 'a promise -> 'a u = Obj.magic
let of_public_promise : 'a t -> 'a promise = Obj.magic
let of_public_resolver : 'a u -> 'a promise = Obj.magic

let create_with_id id =
  let t = {
    id;
    state = Picos.Computation.create ();
  } in
  to_public_promise t, to_public_resolver t

let create ?label () =
  let id = Trace.mint_id () in
  Trace.create_obj ?label id Promise;
  create_with_id id

let create_resolved x =
  let id = Trace.mint_id () in
  Trace.create_obj id Promise;
  to_public_promise { id; state = Picos.Computation.returned x }

let await t =
  let t = of_public_promise t in
  Picos.Computation.await t.state

let await_exn t =
  match await t with
  | Ok x -> x
  | Error ex -> raise ex

let try_resolve p x =
  Picos.Computation.try_return (of_public_resolver p).state x

let resolve p x =
  if not (try_resolve p x) then
    invalid_arg "Can't resolve already-resolved promise"

let resolve_ok    u x = resolve u (Ok x)
let resolve_error u x = resolve u (Error x)

let peek t =
  let t = of_public_promise t in
  match Picos.Computation.peek t.state with
  | Some (Ok v) -> Some v
  | _ -> None

let id t =
  let t = of_public_promise t in
  t.id

let is_resolved t =
  Option.is_some (peek t)

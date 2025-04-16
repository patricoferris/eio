type t = {
  mutable fibers : int;         (* Total, including daemon_fibers and the main function *)
  mutable daemon_fibers : int;
  on_release_lock : Mutex.t;
  mutable on_release : (unit -> unit) Lwt_dllist.t option;      (* [None] when closed. *)
  waiter : unit Single_waiter.t;              (* The main [top]/[sub] function may wait here for fibers to finish. *)
  bundle : Picos_std_structured.Bundle.t;
  id : Trace.id
}

let bundle t = t.bundle

type hook =
  | Null
  | Hook : Mutex.t * (unit -> unit) Lwt_dllist.node -> hook

let null_hook = Null

let cancelled () = assert false

let try_remove_hook = function
  | Null -> false
  | Hook (on_release_lock, n) ->
    Mutex.lock on_release_lock;
    Lwt_dllist.remove n;
    let fn = Lwt_dllist.get n in
    Lwt_dllist.set n cancelled;
    Mutex.unlock on_release_lock;
    fn != cancelled

let remove_hook x = ignore (try_remove_hook x : bool)

let dump f t =
  Fmt.pf f "@[<v2>Switch %d (%d extra fibers):@]"
    (t.id :> int)
    t.fibers

let check _ = ()
let get_error _ = None

let combine_exn ex = function
  | None -> ex
  | Some ex1 -> Exn.combine ex1 ex

(* Note: raises if [t] is finished or called from wrong domain. *)
let fail ?(bt=Exn.empty_backtrace) t ex =
  Picos_std_structured.Bundle.error t.bundle ex bt 

let inc_fibers t =
  t.fibers <- t.fibers + 1

let dec_fibers t =
  t.fibers <- t.fibers - 1

let with_op t fn =
  inc_fibers t;
  Fun.protect fn
    ~finally:(fun () -> dec_fibers t)

let with_daemon t fn =
  inc_fibers t;
  t.daemon_fibers <- t.daemon_fibers + 1;
  Fun.protect fn
    ~finally:(fun () ->
        t.daemon_fibers <- t.daemon_fibers - 1;
        dec_fibers t
      )

let or_raise = function
  | Ok x -> x
  | Error ex -> raise ex

let create bundle =
  {
    fibers = 1;         (* The main function counts as a fiber *)
    daemon_fibers = 0;
    waiter = Single_waiter.create ();
    on_release_lock = Mutex.create ();
    on_release = Some (Lwt_dllist.create ());
    bundle;
    id = Trace.mint_id ()
  }


let run ?name:_ fn = 
  Picos_std_structured.Bundle.join_after @@ fun sw ->
  fn (create sw)

let run_protected ?name:_ fn =
  (* TODO: Almost certainly incorrect *)
  let ctx = Picos.Computation.create () in
  let fiber = Picos.Fiber.create ~forbid:true ctx in
  Picos.Fiber.forbid fiber @@ fun _ ->
  Picos_std_structured.Bundle.join_after @@ fun sw ->
  fn (create sw)

(* Run [fn ()] in [t]'s cancellation context.
   This prevents [t] from finishing until [fn] is done,
   and means that cancelling [t] will cancel [fn]. *)
(* let run_in t fn = *)
(*   with_op t @@ fun () -> *)
(*   let ctx = Effect.perform Cancel.Get_context in *)
(*   let old_cc = ctx.cancel_context in *)
(*   Picos.Computation.attach_canceler *)
(*   Cancel.move_fiber_to t.cancel ctx; *)
(*   match fn () with *)
(*   | ()           -> Cancel.move_fiber_to old_cc ctx; *)
(*   | exception ex -> Cancel.move_fiber_to old_cc ctx; raise ex *)

exception Release_error of string * exn

let () =
  Printexc.register_printer (function
      | Release_error (msg, ex) -> Some (Fmt.str "@[<v2>%s@,while handling %a@]" msg Exn.pp ex)
      | _ -> None
    )

let on_release_full t fn =
  Mutex.lock t.on_release_lock;
  match t.on_release with
  | Some handlers ->
    let node = Lwt_dllist.add_r fn handlers in
    Mutex.unlock t.on_release_lock;
    node
  | None ->
    Mutex.unlock t.on_release_lock;
    match Cancel.protect fn with
    | () -> invalid_arg "Switch finished!"
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Release_error ("Switch finished!", ex)) bt

let on_release t fn =
  ignore (on_release_full t fn : _ Lwt_dllist.node)

let on_release_cancellable t fn =
  Hook (t.on_release_lock, on_release_full t fn)

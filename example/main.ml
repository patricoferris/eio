open Eio__core

let () =
  Picos_mux_random.run @@ fun () ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () -> 
      Printf.printf "Hello Fiber 1\n%!"; 
      Promise.await p; 
      Printf.printf "Bye Fiber 1\n%!"
    )
    (fun () ->
      Printf.printf "Hello Fiber 2\n%!";
      Promise.resolve r ();
      Printf.printf "Bye Fiber 2\n%!"
    )

(* Hello Fiber 1
Hello Fiber 2
Bye Fiber 2
Bye Fiber 1 *)

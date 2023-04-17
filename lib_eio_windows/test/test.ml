open Eio.Std

let () =
  Eio_windows.run @@ fun _ ->
  let check = ref [] in
  Fiber.all [
    (fun () -> Fiber.yield (); check := 3 :: !check);
    (fun () -> Fiber.yield (); check := 2 :: !check);
    (fun () -> Fiber.yield (); check := 1 :: !check)
  ];
  assert (!check = [ 1; 2; 3 ])
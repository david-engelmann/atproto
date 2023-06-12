(*
open Core
open Async
*)

module Sample = struct
  (* variants can be defined within the module *)
  type 'a sample = MySample of { value : 'a } (* defined with as a record *)

  (* create a MySample with string *)
  let mysampletest : string sample = MySample { value = "test" }

  (* create a MySample with int *)
  let mysample4 : int sample = MySample { value = 4 }

  (* create a MySample with float *)
  let mysample4float : float sample = MySample { value = 4.0 }

  (* variables can be defined within the module *)
  let (sample_x : int) = 4
  let (sample_y : int) = 2

  (* functions can be defined within the module *)
  let add1 (x : int) (y : int) = x + y
end

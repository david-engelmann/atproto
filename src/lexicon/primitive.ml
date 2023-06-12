type t =
  | Boolean
  | Number
  | Integer
  | String
  | Ref
  | Union
  | Unknown
  | CidLink
  | Bytes

let lookup_primitive prim =
  match prim with
   | "boolean" -> Boolean
   | "number" -> Number
   | "integer" -> Integer
   | "string" -> String
   | "ref" -> Ref
   | "union" -> Union
   | "unknown" -> Unknown
   | "cid-link" -> CidLink
   | "bytes" -> Bytes

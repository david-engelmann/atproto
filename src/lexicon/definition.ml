type t =
  | Record
  | Query
  | Procedure
  | Subscription
  | Params
  | Token
  | Object
  | Blob
  | Array
  | String

let lookup_definition def =
  match def with
   | "record" -> Record
   | "query" -> Query
   | "procedure" -> Procedure
   | "subscription" -> Subscription
   | "params" -> Params
   | "token" -> Token
   | "object" -> Object
   | "blob" -> Blob
   | "array" -> Array
   | "string" -> String



(** AT Protocol identifier and string-format syntax.
    https://atproto.com/specs/handle
    https://atproto.com/specs/did
    https://atproto.com/specs/nsid
    https://atproto.com/specs/record-key
    https://atproto.com/specs/lexicon#datetime *)
module Syntax = struct
  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let is_ascii_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_ascii_digit = function '0' .. '9' -> true | _ -> false
  let is_ascii_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_ascii_alnum c = is_ascii_alpha c || is_ascii_digit c

  let ascii_lower s =
    String.map
      (function
        | 'A' .. 'Z' as c -> Char.chr (Char.code c + 32) | c -> c)
      s

  let starts_with s prefix =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix

  let ends_with s suffix =
    let n = String.length suffix in
    let len = String.length s in
    len >= n && String.sub s (len - n) n = suffix

  (* ---- Handle ---------------------------------------------------------- *)

  let disallowed_tlds =
    [
      ".alt";
      ".arpa";
      ".example";
      ".internal";
      ".invalid";
      ".local";
      ".localhost";
      ".onion";
    ]

  let handle_max_len = 253

  let is_handle_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' -> true
    | _ -> false

  let is_valid_handle (input : string) : bool =
    let len = String.length input in
    if len = 0 || len > handle_max_len then false
    else
      let rec all_ok i =
        if i >= len then true
        else if is_handle_char input.[i] then all_ok (i + 1)
        else false
      in
      if not (all_ok 0) then false
      else
        let labels = String.split_on_char '.' input in
        let last = List.length labels - 1 in
        if last < 1 then false
        else
          let rec check i = function
            | [] -> true
            | l :: rest ->
                let n = String.length l in
                if n < 1 || n > 63 then false
                else if l.[0] = '-' || l.[n - 1] = '-' then false
                else if i = last && not (is_ascii_letter l.[0]) then false
                else check (i + 1) rest
          in
          check 0 labels

  let ensure_handle s =
    if not (is_valid_handle s) then fail ("invalid handle " ^ s)

  let normalize_handle s = ascii_lower s

  let normalize_and_ensure_handle s =
    let n = normalize_handle s in
    ensure_handle n;
    n

  let is_valid_tld (handle : string) : bool =
    let lower = ascii_lower handle in
    not (List.exists (fun tld -> ends_with lower tld) disallowed_tlds)

  let invalid_handle = "handle.invalid"

  (* ---- DID ------------------------------------------------------------- *)

  let did_max_len = 2048

  let is_did_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | ':' | '%' | '-' ->
        true
    | _ -> false

  let is_lower_letters s =
    String.length s > 0
    &&
    let rec loop i =
      if i >= String.length s then true
      else
        match s.[i] with 'a' .. 'z' -> loop (i + 1) | _ -> false
    in
    loop 0

  let is_valid_did (input : string) : bool =
    let len = String.length input in
    if len < 7 || len > did_max_len then false
    else if not (starts_with input "did:") then false
    else if input.[len - 1] = ':' || input.[len - 1] = '%' then false
    else
      let rec all_ok i =
        if i >= len then true
        else if is_did_char input.[i] then all_ok (i + 1)
        else false
      in
      if not (all_ok 0) then false
      else
        match String.split_on_char ':' input with
        | "did" :: method_ :: rest when rest <> [] ->
            is_lower_letters method_
            &&
            let ident = String.concat ":" rest in
            ident <> ""
            && ident.[String.length ident - 1] <> ':'
            && ident.[String.length ident - 1] <> '%'
        | _ -> false

  let ensure_did s = if not (is_valid_did s) then fail ("invalid DID " ^ s)

  let did_method (input : string) : string option =
    if not (is_valid_did input) then None
    else
      match String.split_on_char ':' input with
      | "did" :: method_ :: _ -> Some method_
      | _ -> None

  let is_blessed_did s =
    match did_method s with
    | Some "plc" | Some "web" -> true
    | _ -> false

  (* ---- NSID ------------------------------------------------------------ *)

  let nsid_max_len = 317

  let is_nsid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' -> true
    | _ -> false

  let is_valid_identifier_name (v : string) : bool =
    String.length v > 0
    && (not (is_ascii_digit v.[0]))
    &&
    let rec loop i =
      if i >= String.length v then true
      else if is_ascii_alnum v.[i] then loop (i + 1)
      else false
    in
    loop 0

  let is_valid_nsid (input : string) : bool =
    let len = String.length input in
    if len = 0 || len > nsid_max_len then false
    else
      let rec all_ok i =
        if i >= len then true
        else if is_nsid_char input.[i] then all_ok (i + 1)
        else false
      in
      if not (all_ok 0) then false
      else
        let segments = String.split_on_char '.' input in
        if List.length segments < 3 then false
        else
          let rec check = function
            | [] -> true
            | l :: rest ->
                let n = String.length l in
                if n < 1 || n > 63 then false
                else if l.[0] = '-' || l.[n - 1] = '-' then false
                else check rest
          in
          if not (check segments) then false
          else if is_ascii_digit (List.hd segments).[0] then false
          else is_valid_identifier_name (List.hd (List.rev segments))

  let ensure_nsid s = if not (is_valid_nsid s) then fail ("invalid NSID " ^ s)

  type nsid = { segments : string list }

  let parse_nsid (input : string) : nsid =
    ensure_nsid input;
    { segments = String.split_on_char '.' input }

  let nsid_name (n : nsid) : string = List.hd (List.rev n.segments)

  (* DNS hostname form, matching @atproto/syntax (com.example.foo -> example.com). *)
  let nsid_authority (n : nsid) : string =
    n.segments |> List.rev |> List.tl |> String.concat "."

  let nsid_authority_nsid (n : nsid) : string =
    n.segments |> List.rev |> List.tl |> List.rev |> String.concat "."

  let nsid_to_string (n : nsid) : string = String.concat "." n.segments

  let create_nsid ~authority ~name : nsid =
    let segs = List.rev (String.split_on_char '.' authority) @ [ name ] in
    parse_nsid (String.concat "." segs)

  let is_valid_nsid_glob (input : string) : bool =
    if input = "*" then true
    else if ends_with input ".*" then
      let prefix = String.sub input 0 (String.length input - 2) in
      (* authority-only glob: at least two segments *)
      let segs = String.split_on_char '.' prefix in
      List.length segs >= 2 && is_valid_nsid (prefix ^ ".x")
    else false

  type nsid_ref = { nsid : string; fragment : string option }

  let parse_nsid_ref (input : string) : nsid_ref =
    match String.index_opt input '#' with
    | None ->
        ensure_nsid input;
        { nsid = input; fragment = None }
    | Some i ->
        let nsid = String.sub input 0 i in
        let frag = String.sub input (i + 1) (String.length input - i - 1) in
        ensure_nsid nsid;
        if frag = "" || frag = "main" then
          fail "NSID ref must not use an empty or #main fragment";
        { nsid; fragment = Some frag }

  (* ---- Record key ------------------------------------------------------ *)

  let record_key_max = 512

  let is_record_key_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '~' | '.' | ':' | '-' ->
        true
    | _ -> false

  let is_valid_record_key (input : string) : bool =
    let len = String.length input in
    if len < 1 || len > record_key_max then false
    else if input = "." || input = ".." then false
    else
      let rec loop i =
        if i >= len then true
        else if is_record_key_char input.[i] then loop (i + 1)
        else false
      in
      loop 0

  let ensure_record_key s =
    if not (is_valid_record_key s) then fail ("invalid record key " ^ s)

  (* ---- at-identifier (handle or DID) ----------------------------------- *)

  let is_valid_at_identifier s = is_valid_did s || is_valid_handle s

  let ensure_at_identifier s =
    if not (is_valid_at_identifier s) then
      fail ("invalid at-identifier " ^ s)

  (* ---- Datetime -------------------------------------------------------- *)

  let datetime_max_len = 64

  let parse_int_slice s off len =
    int_of_string (String.sub s off len)

  let is_leap_year y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

  let days_in_month y m =
    match m with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if is_leap_year y then 29 else 28
    | _ -> 0

  let is_digit_range s off len =
    let rec loop i =
      if i >= len then true
      else if is_ascii_digit s.[off + i] then loop (i + 1)
      else false
    in
    loop 0

  let is_valid_datetime (input : string) : bool =
    let len = String.length input in
    if len < 20 || len > datetime_max_len then false
    else if ends_with input "-00:00" then false
    else if
      not
        (len >= 19
        && is_digit_range input 0 4
        && input.[4] = '-'
        && is_digit_range input 5 2
        && input.[7] = '-'
        && is_digit_range input 8 2
        && input.[10] = 'T'
        && is_digit_range input 11 2
        && input.[13] = ':'
        && is_digit_range input 14 2
        && input.[16] = ':'
        && is_digit_range input 17 2)
    then false
    else
      try
        let year = parse_int_slice input 0 4 in
        let month = parse_int_slice input 5 2 in
        let day = parse_int_slice input 8 2 in
        let hour = parse_int_slice input 11 2 in
        let minute = parse_int_slice input 14 2 in
        let second = parse_int_slice input 17 2 in
        if year < 0 || year > 9999 then false
        else if month < 1 || month > 12 then false
        else if day < 1 || day > days_in_month year month then false
        else if hour > 23 || minute > 59 || second > 60 then false
        else
          let rest = String.sub input 19 (len - 19) in
          let frac, offset =
            if rest = "" then ("", "")
            else if rest.[0] = '.' then
              let rec digits i =
                if i >= String.length rest then i
                else if is_ascii_digit rest.[i] then digits (i + 1)
                else i
              in
              let end_frac = digits 1 in
              if end_frac = 1 then ("", rest)
              else
                ( String.sub rest 0 end_frac,
                  String.sub rest end_frac (String.length rest - end_frac) )
            else ("", rest)
          in
          let frac_ok =
            frac = ""
            || (String.length frac >= 2
               && frac.[0] = '.'
               && is_digit_range frac 1 (String.length frac - 1))
          in
          if not frac_ok then false
          else
            let offset_ok, off_h, off_m, off_sign =
              if offset = "Z" then (true, 0, 0, 1)
              else if
                String.length offset = 6
                && (offset.[0] = '+' || offset.[0] = '-')
                && is_digit_range offset 1 2
                && offset.[3] = ':'
                && is_digit_range offset 4 2
              then
                let h = parse_int_slice offset 1 2 in
                let m = parse_int_slice offset 4 2 in
                let sign = if offset.[0] = '+' then 1 else -1 in
                (h <= 23 && m <= 59, h, m, sign)
              else (false, 0, 0, 1)
            in
            if not offset_ok then false
            else
              (* Reject values that normalize to a negative UTC year, e.g.
                 0000-01-01T00:00:00+01:00. *)
              let utc_min =
                (hour * 60) + minute - (off_sign * ((off_h * 60) + off_m))
              in
              let day_shift =
                if utc_min >= 0 then utc_min / (24 * 60)
                else
                  let adj = utc_min - (24 * 60) + 1 in
                  (adj / (24 * 60)) - 1
              in
              not (year = 0 && day_shift < 0)
      with _ -> false

  let ensure_datetime s =
    if not (is_valid_datetime s) then fail ("invalid datetime " ^ s)

  let now_datetime () : string =
    let tm = Unix.gmtime (Unix.gettimeofday ()) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec

  (* ---- Language (BCP 47 well-formed) ----------------------------------- *)

  let is_alpha_len s lo hi =
    let n = String.length s in
    n >= lo && n <= hi
    &&
    let rec loop i =
      if i >= n then true
      else if is_ascii_alpha s.[i] then loop (i + 1)
      else false
    in
    loop 0

  let is_alnum_len s lo hi =
    let n = String.length s in
    n >= lo && n <= hi
    &&
    let rec loop i =
      if i >= n then true
      else if is_ascii_alnum s.[i] then loop (i + 1)
      else false
    in
    loop 0

  let is_digit_len s n =
    String.length s = n && is_digit_range s 0 n

  let grandfathered_tags =
    [
      "en-gb-oed";
      "i-ami";
      "i-bnn";
      "i-default";
      "i-enochian";
      "i-hak";
      "i-klingon";
      "i-lux";
      "i-mingo";
      "i-navajo";
      "i-pwn";
      "i-tao";
      "i-tay";
      "i-tsu";
      "sgn-be-fr";
      "sgn-be-nl";
      "sgn-ch-de";
      "art-lojban";
      "cel-gaulish";
      "no-bok";
      "no-nyn";
      "zh-guoyu";
      "zh-hakka";
      "zh-min";
      "zh-min-nan";
      "zh-xiang";
    ]

  let is_valid_language (input : string) : bool =
    if input = "" then false
    else
      let lower = ascii_lower input in
      if List.mem lower grandfathered_tags then true
      else
        let parts = String.split_on_char '-' input in
        match parts with
        | [] -> false
        | first :: rest ->
            let private_only = ascii_lower first = "x" in
            let lang_ok =
              private_only
              || is_alpha_len first 2 3
              || is_alpha_len first 4 4
              || is_alpha_len first 5 8
            in
            if not lang_ok then false
            else
              let rec consume expected = function
                | [] -> true
                | p :: ps -> (
                    match expected with
                    | `Extlang_or_later ->
                        if is_alpha_len p 3 3 && String.length first <= 3 then
                          consume `Extlang2 ps
                        else consume `Script_or_later (p :: ps)
                    | `Extlang2 ->
                        if is_alpha_len p 3 3 then consume `Extlang3 ps
                        else consume `Script_or_later (p :: ps)
                    | `Extlang3 ->
                        if is_alpha_len p 3 3 then consume `Script_or_later ps
                        else consume `Script_or_later (p :: ps)
                    | `Script_or_later ->
                        if is_alpha_len p 4 4 then consume `Region_or_later ps
                        else consume `Region_or_later (p :: ps)
                    | `Region_or_later ->
                        if is_alpha_len p 2 2 || is_digit_len p 3 then
                          consume `Variant_or_later ps
                        else consume `Variant_or_later (p :: ps)
                    | `Variant_or_later ->
                        if
                          is_alnum_len p 5 8
                          || (String.length p = 4 && is_ascii_digit p.[0]
                             && is_alnum_len (String.sub p 1 3) 3 3)
                        then consume `Variant_or_later ps
                        else consume `Extension_or_later (p :: ps)
                    | `Extension_or_later ->
                        if String.length p = 1 && ascii_lower p <> "x"
                        then consume (`Extension_rest 0) ps
                        else consume `Private (p :: ps)
                    | `Extension_rest n ->
                        if is_alnum_len p 2 8 then
                          consume (`Extension_rest (n + 1)) ps
                        else if n >= 1 then
                          consume `Extension_or_later (p :: ps)
                        else false
                    | `Private ->
                        if ascii_lower p = "x" then consume (`Private_rest 0) ps
                        else false
                    | `Private_rest n ->
                        if is_alnum_len p 1 8 then
                          consume (`Private_rest (n + 1)) ps
                        else false)
              in
              if private_only then consume (`Private_rest 0) rest
              else if is_alpha_len first 2 3 then
                consume `Extlang_or_later rest
              else consume `Script_or_later rest

  let ensure_language s =
    if not (is_valid_language s) then fail ("invalid language tag " ^ s)

  (* ---- Handle resolution helpers (no network) -------------------------- *)

  let handle_txt_name (handle : string) : string =
    ensure_handle handle;
    "_atproto." ^ normalize_handle handle

  let parse_txt_did (value : string) : string option =
    let v = String.trim value in
    if starts_with v "did=" then
      let did = String.sub v 4 (String.length v - 4) in
      if is_valid_did did then Some did else None
    else None

  let handle_well_known_url (handle : string) : string =
    ensure_handle handle;
    Printf.sprintf "https://%s/.well-known/atproto-did"
      (normalize_handle handle)

  let parse_well_known_did (body : string) : string option =
    let did = String.trim body in
    if is_valid_did did then Some did else None
end

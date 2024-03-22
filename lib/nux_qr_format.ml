type effect =
  | Noisegate of Effects.Noisegate.t
  | Compressor of Effects.Compressor.t
  | EFX of Effects.EFX.t
  | Amp of Effects.Amp.t
  | EQ of Effects.EQ.t
  | Mod of Effects.Mod.t
  | DLY of Effects.DLY.t
  | RVB of Effects.RVB.t
  | IR of Effects.IR.t

type t = effect list

let pp_effect ppf = function
  | Noisegate ng -> Fmt.pf ppf "%a" Effects.Noisegate.pp ng
  | Compressor comp -> Fmt.pf ppf "%a" Effects.Compressor.pp comp
  | EFX efx -> Fmt.pf ppf "%a" Effects.EFX.pp efx
  | Amp amp -> Fmt.pf ppf "%a" Effects.Amp.pp amp
  | EQ eq -> Fmt.pf ppf "%a" Effects.EQ.pp eq
  | Mod mod' -> Fmt.pf ppf "%a" Effects.Mod.pp mod'
  | DLY dly -> Fmt.pf ppf "%a" Effects.DLY.pp dly
  | RVB rvb -> Fmt.pf ppf "%a" Effects.RVB.pp rvb
  | IR ir -> Fmt.pf ppf "%a" Effects.IR.pp ir

(* have not observed any other values *)
let header = "\x0F\x01\x00"
let order_offset = 0x5B

type order = Comp | EFX | Amp | EQ | Gate | Mod | DLY | RVB | IR

let items = 9

let order_of_byte = function
  | 0x1 -> Comp
  | 0x2 -> EFX
  | 0x3 -> Amp
  | 0x4 -> EQ
  | 0x5 -> Gate
  | 0x6 -> Mod
  | 0x7 -> DLY
  | 0x8 -> RVB
  | 0x9 -> IR
  | _otherwise -> failwith "Invalid ordering byte"

let decode v =
  (* verify header match *)
  String.iteri
    (fun index char ->
      match v.[index] = char with
      | true -> ()
      | false -> failwith "Header mismatch")
    header;

  let effect_order =
    List.init items (fun off ->
        let value = int_of_char @@ v.[order_offset + off] in
        order_of_byte value)
  in
  List.map
    (function
      | Gate -> Noisegate (Effects.Noisegate.decode v)
      | Comp -> Compressor (Effects.Compressor.decode v)
      | EFX -> EFX (Effects.EFX.decode v)
      | Amp -> Amp (Effects.Amp.decode v)
      | EQ -> EQ (Effects.EQ.decode v)
      | Mod -> Mod (Effects.Mod.decode v)
      | DLY -> DLY (Effects.DLY.decode v)
      | RVB -> RVB (Effects.RVB.decode v)
      | IR -> IR (Effects.IR.decode v))
    effect_order

let pp ppf v =
  let sep = Fmt.any "\n" in
  Fmt.pf ppf "%a" (Fmt.list ~sep pp_effect) v

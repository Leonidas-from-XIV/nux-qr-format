module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit

  let pf = Format.fprintf
  let any const ppf _don't_care = pf ppf "%s" const

  let rec list ~sep pp_elt ppf v =
    match v with
    | [] -> ()
    | [ x ] -> pf ppf "%a" pp_elt x
    | [ x; y ] ->
        pf ppf "%a" pp_elt x;
        sep ppf ();
        pf ppf "%a" pp_elt y
    | x :: xs ->
        pf ppf "%a" pp_elt x;
        sep ppf ();
        list ~sep pp_elt ppf xs
end

type formatter = Switch | Toggle of int | Percentage

module Param = struct
  type t = {
    name : string;
    (* value of the parameter *)
    value : int;
    formatter : formatter;
  }

  let pp ppf { name; value; formatter } =
    let pp_value =
      match formatter with
      | Percentage -> fun ppf v -> Fmt.pf ppf "%d%%" v
      | _ -> fun ppf _v -> Fmt.pf ppf "TODO"
    in
    Fmt.pf ppf "%s=%a" name pp_value value
end

module Variant = struct
  type t = { name : string; params : Param.t list }

  let pp ppf { name; params } =
    Fmt.pf ppf "%S; params = [%a]" name
      (Fmt.list ~sep:(Fmt.any ", ") Param.pp)
      params
end

module type Pedal = sig
  val switch_addr : int
  val param_offset : int
  val variants : Variant.t list
  val name : string
end

module type Parsed = sig
  type t

  val decode : string -> t
  val pp : t Fmt.t
end

module Reader (P : Pedal) : Parsed = struct
  let switch_to_value v =
    let v = int_of_char v in
    let mask = 0x40 in
    let enabled = v land mask = mask in
    let effect = (v lor mask) - mask in
    (* the effects start at 1, but our lists start at 0 *)
    (enabled, effect - 1)

  let decode bin =
    let enabled, variant_id = bin.[P.switch_addr] |> switch_to_value in
    let variant = List.nth P.variants variant_id in
    let params = variant.params in
    let params =
      List.mapi
        (fun off param ->
          let value = bin.[P.param_offset + off] |> int_of_char in
          { param with Param.value })
        params
    in
    let variant = { variant with params } in
    (enabled, variant)

  type t = bool * Variant.t

  let pp ppf (enabled, variant) =
    Fmt.pf ppf "<%s enabled: %B: variant %a>" P.name enabled Variant.pp variant
end

module NoisegateDef : Pedal = struct
  let name = "Gate"
  let switch_addr = 0x07
  let param_offset = 0x34

  let variants =
    [
      {
        Variant.name = "Noise Gate";
        params =
          [
            { name = "Sens"; value = 0; formatter = Percentage };
            { name = "Decay"; value = 0; formatter = Percentage };
          ];
      };
    ]
end

module Noisegate = Reader (NoisegateDef)

module CompressorDef : Pedal = struct
  let name = "Compressor"
  let switch_addr = 0x03
  let param_offset = 0x11

  let variants =
    [
      {
        Variant.name = "Rose Comp";
        params =
          [
            { name = "Sustain"; value = 0; formatter = Percentage };
            { name = "Level"; value = 0; formatter = Percentage };
          ];
      };
      {
        name = "K Comp";
        params =
          [
            { name = "Sustain"; value = 0; formatter = Percentage };
            { name = "Level"; value = 0; formatter = Percentage };
            { name = "Clipping"; value = 0; formatter = Percentage };
          ];
      };
      {
        name = "Studio Comp";
        params =
          [
            { name = "Thr"; value = 0; formatter = Percentage };
            { name = "Ratio"; value = 0; formatter = Percentage };
            { name = "Gain"; value = 0; formatter = Percentage };
            { name = "Release"; value = 0; formatter = Percentage };
          ];
      };
    ]
end

module Compressor = Reader (CompressorDef)

type effect = Noisegate of Noisegate.t | Compressor of Compressor.t
type t = effect list

let pp_effect ppf = function
  | Noisegate ng -> Fmt.pf ppf "%a" Noisegate.pp ng
  | Compressor comp -> Fmt.pf ppf "%a" Compressor.pp comp

(* have not observed any other values *)
let header = "\x0F\x01\x00"
let order_offset = 0x5B

type order = Comp | EFX | Amp | Eq | Gate | Mod | DLY | RVB | IR

let items = 9

let order_of_byte = function
  | 0x1 -> Comp
  | 0x2 -> EFX
  | 0x3 -> Amp
  | 0x4 -> Eq
  | 0x5 -> Gate
  | 0x6 -> Mod
  | 0x7 -> DLY
  | 0x8 -> RVB
  | 0x9 -> IR
  | _otherwise -> failwith "Invalid ordering byte"

let decode v =
  let effect_order =
    List.init items (fun off ->
        let value = int_of_char @@ v.[order_offset + off] in
        order_of_byte value)
  in
  List.filter_map
    (function
      | Gate -> Some (Noisegate (Noisegate.decode v))
      | Comp -> Some (Compressor (Compressor.decode v))
      | _otherwise ->
          (* TODO remove this eventually, it should crash *)
          None)
    effect_order

let pp ppf v =
  let sep = Fmt.any "\n" in
  let fmt : effect list Fmt.t = Fmt.list ~sep pp_effect in
  Fmt.pf ppf "%a" fmt v

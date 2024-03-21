type formatter = Switch | Toggle of int | Percentage

type param = {
  name : string;
  (* value of the parameter *)
  value : int;
  formatter : formatter;
}

type variant = { name : string; params : param list }

module type Pedal = sig
  val switch_addr : int
  val param_offset : int
  val variants : variant list
end

module type Parsed = sig
  type t

  val decode : string -> t
end

module Reader (P : Pedal) : Parsed = struct
  let switch_to_value v =
    let v = int_of_char v in
    let mask = 0x40 in
    let enabled = v land mask = mask in
    let effect = (v lor mask) - mask in
    (enabled, effect)

  let decode bin =
    let enabled, variant_id = bin.[P.switch_addr] |> switch_to_value in
    let variant = List.nth P.variants variant_id in
    let params = variant.params in
    let params =
      List.mapi
        (fun off param ->
          let value = bin.[P.param_offset + off] |> int_of_char in
          { param with value })
        params
    in
    let variant = { variant with params } in
    (enabled, variant)

  type t = bool * variant
end

module Noisegate : Pedal = struct
  let switch_addr = 0x07
  let param_offset = 0x34

  let variants =
    [
      {
        name = "Noise Gate";
        params =
          [
            { name = "Sens"; value = 0; formatter = Percentage };
            { name = "Decay"; value = 0; formatter = Percentage };
          ];
      };
    ]
end

module NG = Reader (Noisegate)

module Compressor : Pedal = struct
  let switch_addr = 0x03
  let param_offset = 0x11

  let variants =
    [
      {
        name = "Rose Comp";
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

module Comp = Reader (Compressor)

type effect = Noisegate of NG.t | Compressor of Comp.t
type preset = effect list

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
  List.map
    (function
      | Gate -> Noisegate (NG.decode v)
      | Comp -> Compressor (Comp.decode v)
      | _otherwise -> failwith "not implemented yet")
    effect_order

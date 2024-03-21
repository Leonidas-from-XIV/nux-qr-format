type formatter = Switch | Toggle of string list | Percentage

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
    let variant =
      match List.nth_opt P.variants variant_id with
      | Some variant -> variant
      | None ->
          let max = List.length P.variants - 1 in
          Format.asprintf
            "Looking up effect variant %d failed (only up to %d supported)"
            variant_id max
          |> failwith
    in
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

let param name formatter = { Param.name; value = 0; formatter }
let percentage name = param name Percentage

module NoisegateDef : Pedal = struct
  let name = "Gate"
  let switch_addr = 0x07
  let param_offset = 0x33

  let variants =
    [
      {
        Variant.name = "Noise Gate";
        params = [ percentage "Sens"; percentage "Decay" ];
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
        params = [ percentage "Sustain"; percentage "Level" ];
      };
      {
        name = "K Comp";
        params =
          [ percentage "Sustain"; percentage "Level"; percentage "Clipping" ];
      };
      {
        name = "Studio Comp";
        params =
          [
            percentage "Thr";
            percentage "Ratio";
            percentage "Gain";
            percentage "Release";
          ];
      };
    ]
end

module Compressor = Reader (CompressorDef)

module EFXDef : Pedal = struct
  let name = "EFX"
  let switch_addr = 0x04
  let param_offset = 0x16

  let variants =
    let gain = percentage "Gain" in
    let volume = percentage "Volume" in
    let level = percentage "Level" in
    let tone = percentage "Tone" in
    let drive = percentage "Drive" in
    let boost = [ gain; volume; percentage "Bass"; percentage "Treble" ] in
    [
      {
        Variant.name = "Distortion+";
        params = [ percentage "Output"; percentage "Sensivity" ];
      };
      { name = "RC Boost"; params = boost };
      { name = "AC Boost"; params = boost };
      { name = "Dist One"; params = [ level; tone; drive ] };
      { name = "T Screamer"; params = [ drive; tone; level ] };
      { name = "Blues Drv"; params = [ level; tone; gain ] };
      { name = "Morning Drv"; params = [ volume; drive; tone ] };
      {
        name = "Eat Dist";
        params = [ percentage "Distortion"; percentage "Filter"; volume ];
      };
      { name = "Red Dirt"; params = [ drive; tone; level ] };
      { name = "Crunch"; params = [ volume; tone; gain ] };
      { name = "Muff Fuzz"; params = [ volume; tone; percentage "Sustain" ] };
      {
        name = "Katana";
        params = [ { name = "Boost"; value = 0; formatter = Switch }; volume ];
      };
    ]
end

module EFX = Reader (EFXDef)

module AmpDef : Pedal = struct
  let name = "Amp"
  let switch_addr = 0x05
  let param_offset = 0x1D

  let variants =
    let gain = percentage "Gain" in
    let master = percentage "Master" in
    let bass = percentage "Bass" in
    let middle = percentage "Middle" in
    let treble = percentage "Treble" in
    let presence = percentage "Presence" in
    let bright = param "Bright" Switch in
    let mid_freq = percentage "Mid Freq" in
    let cut = percentage "Cut" in
    let gmbmtp = [ gain; master; bass; middle; treble; presence ] in
    let gmbmmt = [ gain; master; bass; mid_freq; middle; treble ] in
    let nux = [ gain; master; bass; middle; treble ] in
    [
      {
        Variant.name = "Jazz Clean";
        params = [ gain; master; bass; middle; treble; bright ];
      };
      { name = "Deluxe Rvb"; params = [ gain; master; bass; middle; treble ] };
      { name = "Bass Mate"; params = gmbmtp };
      { name = "Tweedy"; params = [ gain; master; percentage "Tone" ] };
      { name = "Hiwire"; params = gmbmtp };
      { name = "Cali Crunch"; params = gmbmtp };
      { name = "Class A30"; params = [ gain; master; bass; treble; cut ] };
      { name = "Plexi 100"; params = gmbmtp };
      { name = "Plexi 45"; params = gmbmtp };
      { name = "Brit 800"; params = gmbmtp };
      { name = "1987 X 50"; params = gmbmtp };
      { name = "SLO 100"; params = gmbmtp };
      { name = "Fireman HBE"; params = gmbmtp };
      { name = "Dual Rect"; params = gmbmtp };
      { name = "Die VH4"; params = gmbmtp };
      { name = "Mr. Z38"; params = [ gain; master; bass; treble; cut ] };
      {
        name = "Super Rvb";
        params = [ gain; master; bass; middle; treble; bright ];
      };
      { name = "AGL"; params = gmbmmt };
      { name = "MLD"; params = gmbmmt };
      { name = "Optima Air"; params = nux };
      { name = "Stageman"; params = nux };
    ]
end

module Amp = Reader (AmpDef)

module EQDef : Pedal = struct
  let name = "EQ"
  let switch_addr = 0x06
  let param_offset = 0x26

  let variants =
    [
      {
        Variant.name = "6-Band";
        (* TODO wrong, these are sliders *)
        params =
          [
            percentage "100";
            percentage "220";
            percentage "500";
            percentage "1.2K";
            percentage "2.6K";
            percentage "6.4K";
          ];
      };
      {
        name = "10-Band";
        params =
          [
            percentage "Vol";
            percentage "31.25";
            percentage "62.5";
            percentage "125";
            percentage "250";
            percentage "500";
            percentage "1K";
            percentage "2K";
            percentage "4K";
            percentage "8K";
            percentage "16K";
          ];
      };
    ]
end

module EQ = Reader (EQDef)

module ModDef : Pedal = struct
  let name = "Mod"
  let switch_addr = 0x08
  let param_offset = 0x38

  let variants =
    let depth = percentage "Depth" in
    let rate = percentage "Rate" in
    let intensity = percentage "Intensity" in
    let width = percentage "Width" in
    let speed = percentage "Speed" in
    [
      { Variant.name = "CE-1"; params = [ intensity; depth; rate ] };
      { name = "CE-2"; params = [ depth; rate ] };
      { name = "ST Chorus"; params = [ intensity; width; rate ] };
      { name = "Vibrato"; params = [ rate; depth ] };
      {
        name = "Detune";
        params =
          [ percentage "Shift-L"; percentage "Mix"; percentage "Shift-R" ];
      };
      {
        name = "Flanger";
        params = [ percentage "Level"; rate; width; percentage "F.Back" ];
      };
      { name = "Phase 90"; params = [ speed ] };
      { name = "Phase 100"; params = [ intensity; speed ] };
      {
        name = "S.C.F.";
        params =
          [
            (* TODO investigate *)
            param "Mode" (Toggle [ "Chorus"; "P.M."; "Flanger" ]);
            speed;
            width;
            intensity;
          ];
      };
      {
        name = "U-Vibe";
        params =
          [
            speed;
            percentage "Volume";
            param "Mode" (Toggle [ "Chorus"; "Vibrato" ]);
          ];
      };
      { name = "Tremolo"; params = [ rate; depth ] };
      { name = "Rotary"; params = [ percentage "Balance"; speed ] };
      { name = "SCH-1"; params = [ rate; depth; percentage "Tone" ] };
    ]
end

module Mod = Reader (ModDef)

module DLYDef : Pedal = struct
  let name = "DLY"
  let switch_addr = 0x09
  let param_offset = 0x3F

  let variants =
    let repeat = percentage "Repeat" in
    let d_time = percentage "D.Time" in
    [
      {
        Variant.name = "Analog";
        params =
          [ percentage "Rate"; percentage "Echo"; percentage "Intensity" ];
      };
      {
        name = "Digital";
        params = [ percentage "E.Level"; percentage "F.Back"; d_time ];
      };
      {
        name = "Moduluation";
        params =
          [
            percentage "D-Time"; percentage "D-Level"; percentage "Mod"; repeat;
          ];
      };
      {
        name = "Tape Echo";
        params = [ percentage "Time"; percentage "Level"; repeat ];
      };
      { name = "Pan Delay"; params = [ d_time; repeat; percentage "D.Level" ] };
    ]
end

module DLY = Reader (DLYDef)

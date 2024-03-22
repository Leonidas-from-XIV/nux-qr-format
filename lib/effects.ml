type formatter =
  | Switch
  | Toggle of string list
  | Percentage
  | Db of int * int
  | Hz of int * int
  | Range of int * int

let scale_value low high value =
  let range = abs (low - high) in
  let step_size = float_of_int range /. 100. in
  float_of_int low +. (step_size *. float_of_int value)

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
      | Switch -> (
          fun ppf -> function
            | 0 -> Fmt.pf ppf "%s" "off"
            | _ -> Fmt.pf ppf "%s" "on")
      | Db (low, high) ->
          fun ppf v ->
            let db = scale_value low high v in
            Fmt.pf ppf "%.1f dB" db
      | Range (low, high) ->
          fun ppf v ->
            let db = scale_value low high v in
            Fmt.pf ppf "%.1f" db
      | Hz (low, high) ->
          fun ppf v ->
            let hz = scale_value low high v in
            Fmt.pf ppf "%.0f Hz" hz
      | Toggle options ->
          fun ppf v ->
            let name = List.nth options v in
            Fmt.pf ppf "%s" name
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
    (* enabled when the mask is not set *)
    let enabled = v land mask <> mask in
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
            "Looking up effect variant %d failed (%s only supports up to %d)"
            variant_id P.name max
          |> failwith
    in
    let params =
      List.mapi
        (fun off param ->
          let value = bin.[P.param_offset + off] |> int_of_char in
          { param with Param.value })
        variant.params
    in
    (enabled, { variant with params })

  type t = bool * Variant.t

  let pp ppf (enabled, variant) =
    Fmt.pf ppf "<%s enabled: %B, variant %a>" P.name enabled Variant.pp variant
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
    let vox = [ gain; master; bass; treble; cut ]  in
    let nux = [ gain; master; bass; middle; treble ] in
    [
      {
        Variant.name = "Jazz Clean";
        params = [ gain; master; bass; middle; treble; bright ];
      };
      { name = "Deluxe Rvb"; params = [ gain; master; bass; middle; treble ] };
      { name = "Bass Mate"; params = gmbmtp };
      { name = "Tweedy"; params = [ gain; master; percentage "Tone" ] };
      (* secret amp not exposed in app *)
      { name = "Twin Rvb"; params = gmbmtp };
      { name = "Hiwire"; params = gmbmtp };
      { name = "Cali Crunch"; params = gmbmtp };
      (* another secret amp not exposed in app *)
      { name = "Class A15"; params = vox };
      { name = "Class A30"; params = vox };
      { name = "Plexi 100"; params = gmbmtp };
      { name = "Plexi 45"; params = gmbmtp };
      { name = "Brit 800"; params = gmbmtp };
      { name = "1987 X 50"; params = gmbmtp };
      { name = "SLO 100"; params = gmbmtp };
      { name = "Fireman HBE"; params = gmbmtp };
      { name = "Dual Rect"; params = gmbmtp };
      { name = "Die VH4"; params = gmbmtp };
      (* secret *)
      { name = "Vibro King"; params = gmbmtp};
      (* secret *)
      { name = "Budda"; params = [gain; master; bass; middle; treble; cut]};
      { name = "Mr. Z38"; params = [ gain; master; bass; treble; cut ] };
      {
        name = "Super Rvb";
        params = [ gain; master; bass; middle; treble; bright ];
      };
      (* secret *)
      { name = "Brit Blues"; params = gmbmtp };
      (* secret *)
      { name = "Match D30"; params = [gain; master; bass; treble; cut]};
      (* secret *)
      { name = "Brit 2000"; params = gmbmtp };
      (* final secret *)
      { name = "Uber HiGain"; params = gmbmtp };
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
    let slider name = param name (Range (-15, 15)) in
    [
      {
        Variant.name = "6-Band";
        params =
          [
            slider "100";
            slider "220";
            slider "500";
            slider "1.2K";
            slider "2.6K";
            slider "6.4K";
          ];
      };
      (* there is a value between 6 and 10 but its not selectable *)
      { name = "<Unlisted>"; params = [] };
      {
        name = "10-Band";
        params =
          [
            slider "Vol";
            slider "31.25";
            slider "62.5";
            slider "125";
            slider "250";
            slider "500";
            slider "1K";
            slider "2K";
            slider "4K";
            slider "8K";
            slider "16K";
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
            speed;
            width;
            param "Mode" (Toggle [ "Chorus"; "P.M."; "Flanger" ]);
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

module RVBDef : Pedal = struct
  let name = "RVB"
  let switch_addr = 0x0A
  let param_offset = 0x48

  let variants =
    let level = percentage "Level" in
    let decay = percentage "Decay" in
    let mix = percentage "Mix" in
    [
      { Variant.name = "Room"; params = [ decay; percentage "Tone"; level ] };
      {
        name = "Hall";
        params =
          [ decay; percentage "Pre Delay"; percentage "Liveliness"; level ];
      };
      { name = "Plate"; params = [ decay; level ] };
      { name = "Spring"; params = [ decay; level ] };
      {
        name = "Shimmer";
        params = [ percentage "Mix"; decay; percentage "Shim" ];
      };
      { name = "Damp"; params = [ mix; percentage "Depth" ] };
    ]
end

module RVB = Reader (RVBDef)

module IRDef : Pedal = struct
  let name = "IR"
  let switch_addr = 0x0B
  let param_offset = 0x50

  let variants =
    (* these are all kind of wrong, need to investigate *)
    let level = param "Level" (Db (-12, 12)) in
    let low_cut = param "Low Cut" (Hz (20, 300)) in
    let high_cut = param "Hight Cut" (Hz (5000, 20000)) in
    (* all IRs have the same params *)
    let params = [ level; low_cut; high_cut ] in
    List.map
      (fun name -> { Variant.name; params })
      [
        "JR120";
        "DR112";
        "TR212";
        "HIWIRE412";
        "Cali 112";
        "A112";
        "GB412";
        "M1960AX";
        "M1960AV";
        "M1960TV";
        "SLO412";
        "FIREMAN412";
        "RECT 412";
        "DIE412";
        "MATCH212";
        "UBER412";
        "BS410";
        "A212";
        "M1960AHW";
        "M1936";
        "BUDDA112";
        "Z212";
        "SUPERVERB410";
        "VIBROKING310";
        "AGL_DB810";
        "AMP_SV212";
        "AMP_SV410";
        "AMP_SV810";
        "BASSGUY410";
        "EDED410";
        "MKB410";
        "G-HIBIRD";
        "G-J15";
        "M-D45";
        "CT-Bogna-T75";
        "CT-BritJH-G12M";
      ]
end

module IR = Reader (IRDef)

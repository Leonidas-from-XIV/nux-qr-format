open Base
module Nux = Nux_qr_format

let decode_qr_data qr_value =
  match String.chop_prefix ~prefix:"nux://" qr_value with
  | None -> Error (`Msg "no nux:// prefix")
  | Some payload -> (
      match String.chop_prefix ~prefix:"MightyAmp:" payload with
      | None -> Error (`Msg "Not MightyAmp compatible payload")
      | Some b64 -> Base64.decode b64)

let dump_data nux_data =
  let oc = Stdio.stdout in
  let oc_descr = Unix.descr_of_out_channel oc in
  Stdio.Out_channel.output_string oc nux_data;
  match Unix.isatty oc_descr with
  | true -> Stdio.Out_channel.newline oc
  | false -> ()

let dump qr_value =
  match decode_qr_data qr_value with
  | Error (`Msg msg) -> Stdio.prerr_endline msg
  | Ok nux_data -> dump_data nux_data

let decode qr_value =
  match decode_qr_data qr_value with
  | Error (`Msg msg) -> Stdio.prerr_endline msg
  | Ok nux_data ->
      let d = Nux_qr_format.decode nux_data in
      let s = Stdlib.Format.asprintf "%a" Nux.pp d in
      Stdio.print_endline s

let () =
  let argv = Sys.get_argv () in
  match Array.length argv < 3 with
  | true -> Stdio.prerr_endline "Usage: <command> <qr-data>"
  | false -> (
      let command = argv.(1) in
      let qr_value = argv.(2) in
      match command with
      | "dump" -> dump qr_value
      | "decode" -> decode qr_value
      | _otherwise -> Stdio.prerr_endline "Usage: dump|decode <qr-data>")

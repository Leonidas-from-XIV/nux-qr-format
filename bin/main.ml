open Base

let dump_data nux_data =
  let oc = Stdio.stdout in
  let oc_descr = Unix.descr_of_out_channel oc in
  Stdio.Out_channel.output_string oc nux_data;
  match Unix.isatty oc_descr with
  | true -> Stdio.Out_channel.newline oc
  | false -> ()

let main qr_value =
  match String.chop_prefix ~prefix:"nux://" qr_value with
  | None -> failwith "no nux:// prefix"
  | Some payload -> (
      match String.chop_prefix ~prefix:"MightyAmp:" payload with
      | None -> failwith "Not MightyAmp compatible payload"
      | Some b64 -> (
          match Base64.decode b64 with
          | Error (`Msg msg) -> failwith msg
          | Ok nux_data -> dump_data nux_data))

let () =
  let qr_value = (Sys.get_argv ()).(1) in
  main qr_value

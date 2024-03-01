open Base

let main qr_value =
  match String.chop_prefix ~prefix:"nux://" qr_value with
  | None -> failwith "no nux:// prefix"
  | Some payload -> (
      match String.chop_prefix ~prefix:"MightyAmp:" payload with
      | None -> failwith "Not MightyAmp compatible payload"
      | Some b64 -> (
          match Base64.decode b64 with
          | Error (`Msg msg) -> failwith msg
          | Ok nux_data -> Stdio.print_string nux_data))

let () =
  let qr_value = (Sys.get_argv ()).(1) in
  main qr_value

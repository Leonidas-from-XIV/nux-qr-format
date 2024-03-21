type 'a t = Format.formatter -> 'a -> unit

let pf = Format.fprintf
let any const ppf _don't_care = pf ppf "%s" const

let rec list ~sep pp_elt ppf = function
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

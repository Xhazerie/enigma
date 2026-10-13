type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index c = Char.code c - Char.code 'A'

(** [wrap n] is [n] reduced into [0]..[25], correct for negative [n]. *)
let wrap n = ((n mod 26) + 26) mod 26

let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  wrap (index wiring.[contact] - offset)

(** [letter_of_index i] is the uppercase letter at alphabet index [i]. *)
let letter_of_index i = Char.chr (i + Char.code 'A')

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  wrap (String.index wiring (letter_of_index contact) - offset)

let map_refl wiring input_pos =
  index wiring.[input_pos]

let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (a, b) :: rest ->
    if c = a then b
    else if c = b then a
    else map_plug rest c

let cipher_char config c =
  let pos = index (map_plug config.plugboard c) in
  let after_r =
    List.fold_right
      (fun r acc -> map_r_to_l r.rotor.wiring r.top_letter acc)
      config.rotors pos
  in
  let after_refl = map_refl config.refl after_r in
  let after_l =
    List.fold_left
      (fun acc r -> map_l_to_r r.rotor.wiring r.top_letter acc)
      after_refl config.rotors
  in
  map_plug config.plugboard (letter_of_index after_l)

(** [at_turnover r] is [true] when [r] shows its turnover letter. *)
let at_turnover r = r.top_letter = r.rotor.turnover

(** [advance r] is [r] rotated forward one position. *)
let advance r =
  { r with top_letter = letter_of_index (wrap (index r.top_letter + 1)) }

let step config =
  let rec step_rotors is_leftmost rotors =
    match rotors with
    | [] -> []
    | [ last ] -> [ advance last ]
    | r :: (next :: _ as rest) ->
      let steps = (not is_leftmost && at_turnover r) || at_turnover next in
      (if steps then advance r else r) :: step_rotors false rest
  in
  { config with rotors = step_rotors true config.rotors }

let cipher config s =
  let rec aux config chars =
    match chars with
    | [] -> []
    | c :: rest ->
      let config = step config in
      cipher_char config c :: aux config rest
  in
  s |> String.to_seq |> List.of_seq |> aux config
    |> List.to_seq |> String.of_seq

let hours_worked = 9

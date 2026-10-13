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

let wrap n = ((n mod 26) + 26) mod 26

let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  wrap (index wiring.[contact] - offset)

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

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0

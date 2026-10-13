open OUnit2
open Enigma

let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let refl_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let rI = { wiring = rotor_I; turnover = 'Q' }
let rII = { wiring = rotor_II; turnover = 'E' }
let rIII = { wiring = rotor_III; turnover = 'V' }
let orient r t = { rotor = r; top_letter = t }

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let r_to_l_test name wiring top pos expected =
  name >:: fun _ -> assert_equal expected (map_r_to_l wiring top pos)

let l_to_r_test name wiring top pos expected =
  name >:: fun _ -> assert_equal expected (map_l_to_r wiring top pos)

let refl_test name wiring pos expected =
  name >:: fun _ -> assert_equal expected (map_refl wiring pos)

let plug_test name plugs c expected =
  name >:: fun _ -> assert_equal expected (map_plug plugs c)

let cipher_char_test name config c expected =
  name >:: fun _ -> assert_equal expected (cipher_char config c)

let tops config = List.map (fun r -> r.top_letter) config.rotors

let step_test name config expected =
  name >:: fun _ -> assert_equal expected (tops (step config))

let cipher_test name config s expected =
  name >:: fun _ -> assert_equal expected (cipher config s) ~printer:(fun x -> x)
  
let gp_config =
  { refl = refl_B;
    rotors = [ orient rI 'A'; orient rII 'A'; orient rIII 'A' ];
    plugboard = [] }

let id_config =
  { refl = identity; rotors = []; plugboard = [] }

let step_config tops =
  { refl = refl_B;
    rotors =
      [ orient rIII tops.[0]; orient rII tops.[1]; orient rI tops.[2] ];
    plugboard = [] }

let full_config =
  { refl = refl_B;
    rotors = [ orient rI 'F'; orient rII 'U'; orient rIII 'N' ];
    plugboard = [ ('A', 'Z') ] }

let suite =
  "Enigma test suite" >::: [
    (* index *)
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of G is 6" 'G' 6;

    (* map_r_to_l *)
    r_to_l_test "identity rotor fixes position 0" identity 'A' 0 0;
    r_to_l_test "rotor I at A maps 0 to 4" rotor_I 'A' 0 4;
    r_to_l_test "rotor I at B maps 0 to 9" rotor_I 'B' 0 9;
    r_to_l_test "rotor III at O maps 14 to 17" rotor_III 'O' 14 17;
    r_to_l_test "rotor I at A maps 1 to 10" rotor_I 'A' 1 10;

    (* map_l_to_r *)
    l_to_r_test "identity rotor fixes position 0" identity 'A' 0 0;
    l_to_r_test "rotor I at F maps 10 to 14" rotor_I 'F' 10 14;
    l_to_r_test "rotor I at A maps 0 to 20" rotor_I 'A' 0 20;
    l_to_r_test "identity rotor at A maps 5 to 5" identity 'A' 5 5;
    l_to_r_test "rotor III at A maps 4 to 15" rotor_III 'A' 4 15;

    (* map_refl *)
    refl_test "identity reflector fixes 0" identity 0 0;
    refl_test "identity reflector fixes 25" identity 25 25;
    refl_test "reflector B maps 5 to 18" refl_B 5 18;
    refl_test "reflector B maps 0 to 24" refl_B 0 24;
    refl_test "reflector B maps 24 to 0" refl_B 24 0;

    (* map_plug *)
    plug_test "empty plugboard leaves M unchanged" [] 'M' 'M';
    plug_test "plugboard AZ maps A to Z" [ ('A', 'Z') ] 'A' 'Z';
    plug_test "plugboard AZ maps Z to A" [ ('A', 'Z') ] 'Z' 'A';
    plug_test "plugboard XY maps X to Y"
      [ ('A', 'Z'); ('X', 'Y') ] 'X' 'Y';
    plug_test "unconnected letter unchanged"
      [ ('A', 'Z'); ('X', 'Y') ] 'M' 'M';

    (* cipher_char *)
    cipher_char_test "no-rotor identity maps A to A" id_config 'A' 'A';
    cipher_char_test "no-rotor identity maps B to B" id_config 'B' 'B';
    cipher_char_test "no-rotor identity maps Z to Z" id_config 'Z' 'Z';
    cipher_char_test "historical config maps G to P" gp_config 'G' 'P';
    cipher_char_test "historical config maps A to U" gp_config 'A' 'U';

    (* step *)
    step_test "rightmost steps KDO to KDP" (step_config "KDO") [ 'K'; 'D'; 'P' ];
    step_test "turnover steps KDQ to KER" (step_config "KDQ") [ 'K'; 'E'; 'R' ];
    step_test "double step KER to LFS" (step_config "KER") [ 'L'; 'F'; 'S' ];
    step_test "turnover steps VDQ to VER" (step_config "VDQ") [ 'V'; 'E'; 'R' ];
    step_test "double step VER to WFS" (step_config "VER") [ 'W'; 'F'; 'S' ];

    (* cipher *)
    cipher_test "empty string ciphers to empty" full_config "" "";
    cipher_test "OCAML ciphers to YNGXQ" full_config "OCAML" "YNGXQ";
    cipher_test "YNGXQ deciphers to OCAML" full_config "YNGXQ" "OCAML";
    cipher_test "single character" gp_config "A" "B";
    cipher_test "cipher is an involution"
    full_config (cipher full_config "HELLO") "HELLO";

  ]

let () = run_test_tt_main suite

open OUnit2
open Enigma

let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
(* let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE" *)
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let r_to_l_test name wiring top pos expected =
  name >:: fun _ -> assert_equal expected (map_r_to_l wiring top pos)

let l_to_r_test name wiring top pos expected =
  name >:: fun _ -> assert_equal expected (map_l_to_r wiring top pos)

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

  ]

let () = run_test_tt_main suite

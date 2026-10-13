open OUnit2
open Enigma

let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_i = { wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"; turnover = 'Q' }
let rotor_ii = { wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"; turnover = 'E' }
let rotor_iii = { wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"; turnover = 'V' }
let reflector_b = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let rotor_map_test name map wiring top_letter input expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected (map wiring top_letter input)

let reflector_test name wiring input expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected (map_refl wiring input)

let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Y is 24" 'Y' 24;
    index_test "index of Z is 25" 'Z' 25;
    rotor_map_test "identity rotor maps 0 right-to-left" map_r_to_l
      identity 'A' 0 0;
    rotor_map_test "identity rotor remains identity when rotated"
      map_r_to_l identity 'Z' 17 17;
    rotor_map_test "swapped A and B maps A to B right-to-left"
      map_r_to_l "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 1;
    rotor_map_test "rotor I at A maps position 0 to 4" map_r_to_l
      rotor_i.wiring 'A' 0 4;
    rotor_map_test "rotor I at B accounts for its offset" map_r_to_l
      rotor_i.wiring 'B' 0 9;
    rotor_map_test "rotor III at O maps position 14 to 17" map_r_to_l
      rotor_iii.wiring 'O' 14 17;
    rotor_map_test "identity rotor maps 25 left-to-right" map_l_to_r
      identity 'A' 25 25;
    rotor_map_test "rotated identity remains identity in reverse"
      map_l_to_r identity 'M' 8 8;
    rotor_map_test "swapped A and B maps B to A left-to-right"
      map_l_to_r "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 1 0;
    rotor_map_test "rotated swapped rotor wraps its reverse output"
      map_l_to_r "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 0 25;
    rotor_map_test "rotor I at A reverse maps position 0 to 20"
      map_l_to_r rotor_i.wiring 'A' 0 20;
    rotor_map_test "rotor I at F reverse maps position 10 to 14"
      map_l_to_r rotor_i.wiring 'F' 10 14;
    reflector_test "identity reflector maps position 0 to itself"
      identity 0 0;
    reflector_test "identity reflector maps middle position to itself"
      identity 12 12;
    reflector_test "identity reflector maps position 25 to itself"
      identity 25 25;
    reflector_test "reflector B maps A contact to Y contact" reflector_b
      0 24;
    reflector_test "reflector B maps F contact to S contact" reflector_b
      5 18;
    reflector_test "reflector B maps Y contact back to A contact"
      reflector_b 24 0;
  ]

let () = run_test_tt_main suite

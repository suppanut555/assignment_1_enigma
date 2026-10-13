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

let char_printer c = String.make 1 c

let plugboard_test name plugs input expected =
  name >:: fun _ ->
  assert_equal ~printer:char_printer expected (map_plug plugs input)

let cipher_char_test name config input expected =
  name >:: fun _ ->
  assert_equal ~printer:char_printer expected (cipher_char config input)

let oriented rotor top_letter = { rotor; top_letter }

let static_config =
  {
    refl = reflector_b;
    rotors =
      [ oriented rotor_i 'A'; oriented rotor_ii 'A'; oriented rotor_iii 'A' ];
    plugboard = [];
  }

let tops config =
  config.rotors
  |> List.map (fun oriented -> oriented.top_letter)
  |> List.to_seq |> String.of_seq

let step_test name config expected_tops =
  name >:: fun _ ->
  assert_equal ~printer:(fun value -> value) expected_tops (tops (step config))

let stepping_config top_letters =
  let rec combine rotors letters =
    match (rotors, letters) with
    | [], [] -> []
    | rotor :: rotor_rest, top_letter :: letter_rest ->
        oriented rotor top_letter :: combine rotor_rest letter_rest
    | _ -> invalid_arg "rotor and top-letter counts differ"
  in
  {
    refl = reflector_b;
    rotors = combine [ rotor_iii; rotor_ii; rotor_i ] top_letters;
    plugboard = [];
  }

let cipher_test name config input expected =
  name >:: fun _ ->
  assert_equal ~printer:(fun value -> value) expected (cipher config input)

let full_config =
  {
    refl = reflector_b;
    rotors =
      [ oriented rotor_i 'F'; oriented rotor_ii 'U'; oriented rotor_iii 'N' ];
    plugboard = [ ('A', 'Z') ];
  }

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
    plugboard_test "empty plugboard leaves M unchanged" [] 'M' 'M';
    plugboard_test "plugboard AZ maps A to Z" [ ('A', 'Z') ] 'A' 'Z';
    plugboard_test "plugboard AZ maps Z to A" [ ('A', 'Z') ] 'Z' 'A';
    plugboard_test "plugboard finds X in a later pair"
      [ ('A', 'Z'); ('X', 'Y') ] 'X' 'Y';
    plugboard_test "plugboard maps second endpoint in a later pair"
      [ ('A', 'Z'); ('X', 'Y') ] 'Y' 'X';
    plugboard_test "plugboard leaves an unconnected letter unchanged"
      [ ('A', 'Z'); ('X', 'Y') ] 'Q' 'Q';
    cipher_char_test "identity machine leaves A unchanged"
      { refl = identity; rotors = []; plugboard = [] }
      'A' 'A';
    cipher_char_test "reflector-only machine maps A to Y"
      { refl = reflector_b; rotors = []; plugboard = [] }
      'A' 'Y';
    cipher_char_test "historical static machine maps A to U"
      static_config 'A' 'U';
    cipher_char_test "historical static machine maps G to P"
      static_config 'G' 'P';
    cipher_char_test "historical static machine maps Z to H"
      static_config 'Z' 'H';
    cipher_char_test "plugboard is applied on both sides"
      { static_config with plugboard = [ ('A', 'Z') ] }
      'H' 'A';
    step_test "machine with no rotors remains empty"
      { refl = identity; rotors = []; plugboard = [] }
      "";
    step_test "single rightmost rotor always steps"
      {
        refl = identity;
        rotors = [ oriented rotor_i 'A' ];
        plugboard = [];
      }
      "B";
    step_test "rightmost rotor wraps from Z to A"
      {
        refl = identity;
        rotors = [ oriented rotor_i 'Z' ];
        plugboard = [];
      }
      "A";
    step_test "ordinary step changes only the rightmost rotor"
      (stepping_config [ 'K'; 'D'; 'O' ])
      "KDP";
    step_test "rightmost turnover also steps its left neighbor"
      (stepping_config [ 'K'; 'D'; 'Q' ])
      "KER";
    step_test "middle turnover produces a double-step transition"
      (stepping_config [ 'K'; 'E'; 'R' ])
      "LFS";
    cipher_test "empty plaintext produces empty ciphertext" full_config
      "" "";
    cipher_test "identity machine enciphers multiple letters unchanged"
      { refl = identity; rotors = []; plugboard = [] }
      "AZBY" "AZBY";
    cipher_test "reflector-only machine enciphers each character"
      { refl = reflector_b; rotors = []; plugboard = [] }
      "ABC" "YRU";
    cipher_test "full historical configuration reveals OCAML"
      full_config "YNGXQ" "OCAML";
    cipher_test "same starting configuration decrypts ciphertext"
      full_config "OCAML" "YNGXQ";
    ("full cipher is reciprocal from the same initial state"
    >:: fun _ ->
    let plaintext = "HELLOWORLD" in
    let ciphertext = cipher full_config plaintext in
    assert_equal ~printer:(fun value -> value) plaintext
      (cipher full_config ciphertext));
  ]

let () = run_test_tt_main suite

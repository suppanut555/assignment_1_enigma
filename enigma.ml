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

let alphabet_size = 26

(** [normalize_position position] wraps [position] into the range 0 through
    25. *)
let normalize_position position =
  let remainder = position mod alphabet_size in
  if remainder < 0 then remainder + alphabet_size else remainder

(** [letter position] is the uppercase letter at [position].
    Requires: [position] is in the range 0 through 25. *)
let letter position = Char.chr (Char.code 'A' + position)

let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let rotor_contact = normalize_position (input_pos + offset) in
  let wired_contact = index wiring.[rotor_contact] in
  normalize_position (wired_contact - offset)

(** [find_letter wiring target] is the position of [target] in [wiring].
    Requires: [target] occurs in [wiring]. *)
let find_letter wiring target =
  let rec search position =
    if wiring.[position] = target then position else search (position + 1)
  in
  search 0

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let rotor_contact = normalize_position (input_pos + offset) in
  let wired_contact = find_letter wiring (letter rotor_contact) in
  normalize_position (wired_contact - offset)

let map_refl wiring input_pos = index wiring.[input_pos]

let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (left, right) :: rest ->
      if c = left then right
      else if c = right then left
      else map_plug rest c

(** [map_rotors_r_to_l rotors input_pos] passes [input_pos] through [rotors]
    from the keyboard side toward the reflector. [rotors] is ordered left to
    right. *)
let rec map_rotors_r_to_l rotors input_pos =
  match rotors with
  | [] -> input_pos
  | oriented :: rest ->
      let from_right = map_rotors_r_to_l rest input_pos in
      map_r_to_l oriented.rotor.wiring oriented.top_letter from_right

(** [map_rotors_l_to_r rotors input_pos] passes [input_pos] through [rotors]
    from the reflector side toward the keyboard. [rotors] is ordered left to
    right. *)
let rec map_rotors_l_to_r rotors input_pos =
  match rotors with
  | [] -> input_pos
  | oriented :: rest ->
      let from_left =
        map_l_to_r oriented.rotor.wiring oriented.top_letter input_pos
      in
      map_rotors_l_to_r rest from_left

let cipher_char config c =
  let plugged_input = map_plug config.plugboard c in
  let reflected_position =
    index plugged_input
    |> map_rotors_r_to_l config.rotors
    |> map_refl config.refl
  in
  reflected_position
  |> map_rotors_l_to_r config.rotors
  |> letter
  |> map_plug config.plugboard

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0

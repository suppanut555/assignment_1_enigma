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

(** [next_letter c] is the letter after [c], wrapping ['Z'] to ['A']. *)
let next_letter c = letter (normalize_position (index c + 1))

(** [step_rotors rotors] applies one simultaneous stepping event. A rotor
    steps when it is rightmost, is a non-leftmost rotor at turnover, or has a
    right neighbor at turnover. *)
let step_rotors rotors =
  let rec apply is_leftmost remaining =
    match remaining with
    | [] -> []
    | oriented :: rest ->
        let is_rightmost = rest = [] in
        let at_own_turnover =
          (not is_leftmost)
          && oriented.top_letter = oriented.rotor.turnover
        in
        let right_neighbor_at_turnover =
          match rest with
          | [] -> false
          | right :: _ -> right.top_letter = right.rotor.turnover
        in
        let should_step =
          is_rightmost || at_own_turnover || right_neighbor_at_turnover
        in
        let top_letter =
          if should_step then next_letter oriented.top_letter
          else oriented.top_letter
        in
        { oriented with top_letter } :: apply false rest
  in
  apply true rotors

let step config = { config with rotors = step_rotors config.rotors }

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0

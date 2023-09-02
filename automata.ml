type state = string
type symbol = char

type transition = state * symbol * state

type automaton = {
  states: state list;
  alphabet: symbol list;
  transitions: transition list;
  initial_state: state;
  final_states: state list;
}

let create_automaton states alphabet transitions initial_state final_states =
  { states; alphabet; transitions; initial_state; final_states }

let print_automaton automaton =
  Printf.printf "Alphabet : %s\n" (String.concat ", " (List.map (String.make 1) automaton.alphabet));
  Printf.printf "États : %s\n" (String.concat ", " automaton.states);
  Printf.printf "État initial : %s\n" automaton.initial_state;
  Printf.printf "États finaux : %s\n" (String.concat ", " automaton.final_states);
  Printf.printf "Transitions :\n";
  List.iter (fun (src, sym, dest) ->
    Printf.printf "  %s --%c--> %s\n" src sym dest
  ) automaton.transitions

let () =
let automaton = create_automaton ["q1"; "q2"; "q3"] ['a'; 'b'] [("q1", 'a', "q2"); ("q2", 'b', "q3")] "q1" ["q3"] in
print_automaton automaton

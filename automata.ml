type state = int
type symbol = char

type transition = state * symbol * state

type automaton = {
  states: state list;
  alphabet: symbol list;
  transitions: transition list;
  initial_state: state;
  final_state: state list;
}

let create_automaton states alphabet transitions initial_state final_state =
  { states; alphabet; transitions; initial_state; final_state }

let print_automaton automaton =
  Printf.printf "Alphabet : %s\n" (String.concat ", " (List.map (String.make 1) automaton.alphabet));
  Printf.printf "États : %s\n" (String.concat ", " (List.map string_of_int automaton.states));
  Printf.printf "État initial : %d\n" automaton.initial_state;
  Printf.printf "États finaux : %s\n" (String.concat ", " (List.map string_of_int automaton.final_state));
  Printf.printf "Transitions :\n";
  List.iter (fun (src, sym, dest) ->
    Printf.printf "  %d --%c--> %d\n" src sym dest
  ) automaton.transitions


let () =
let automaton = create_automaton [1; 2; 3] ['a'; 'b'] [(1, 'a', 2); (2, 'b', 3)] 1 [3] in
print_automaton automaton

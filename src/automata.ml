type state = string
type symbol = char

type transition = state * symbol * state

type automaton = {
  name: string;
  states: state list;
  alphabet: symbol list;
  transitions: transition list;
  initial_state: state;
  final_states: state list;
}

let create_automaton name states alphabet transitions initial_state final_states =
  { name; states; alphabet; transitions; initial_state; final_states }

let add_state automaton new_state =
  { automaton with states = new_state :: automaton.states }

let add_transition automaton src sym dest =
  { automaton with transitions = (src, sym, dest) :: automaton.transitions }


let accepte_mot automaton mot =
  let rec est_accepte etat mot_restant =
    match mot_restant with
    | [] ->
      List.mem etat automaton.final_states (* Vérifie si l'état actuel est un état final *)
    | sym::reste ->
      let transitions_sortantes =
        List.filter (fun (src, s, dest) -> src = etat && s = sym) automaton.transitions
      in
      List.exists (fun (_, _, dest) -> est_accepte dest reste) transitions_sortantes
  in
  est_accepte automaton.initial_state (List.to_seq mot |> List.of_seq)
  
  
let print_automaton automaton =
  Printf.printf "Nom de l'automate : %s\n" automaton.name;
  Printf.printf "Alphabet : %s\n" (String.concat ", " (List.map (String.make 1) automaton.alphabet));
  Printf.printf "États : %s\n" (String.concat ", " automaton.states);
  Printf.printf "État initial : %s\n" automaton.initial_state;
  Printf.printf "États finaux : %s\n" (String.concat ", " automaton.final_states);
  Printf.printf "Transitions :\n";
  List.iter (fun (src, sym, dest) ->
    Printf.printf "  %s --%c--> %s\n" src sym dest
  ) automaton.transitions

  let () =
  let automaton = create_automaton "A" ["q1"; "q2"; "q3"] ['a'; 'b'] [("q1", 'a', "q2"); ("q2", 'b', "q3")] "q1" ["q3"] in
  let automaton_with_transition = add_transition automaton "q3" 'a' "q1" in
  let mot1 = ['a'; 'b'; 'b'; 'a'] in
  let mot2 = ['a'; 'b'; 'b'; 'a'; 'b'] in
  let mot3 = ['a'; 'b'] in
  
  print_automaton automaton_with_transition;
  
  Printf.printf "Automate accepte %s : %b\n" (String.of_seq (List.to_seq mot1)) (accepte_mot automaton_with_transition mot1);
  Printf.printf "Automate accepte %s : %b\n" (String.of_seq (List.to_seq mot2)) (accepte_mot automaton_with_transition mot2);
  Printf.printf "Automate accepte %s : %b\n" (String.of_seq (List.to_seq mot3)) (accepte_mot automaton_with_transition mot3);
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
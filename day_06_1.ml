type school = int list

let rec nextGeneration (school: school) (acc: school): school =
  match school with
  | [] -> acc
  | (0::fish) -> nextGeneration fish (6::8::acc)
  | (timer::fish) -> nextGeneration fish ((timer - 1)::acc)

let rec computeAnswer (school: school) (iterations: int): int = 
  match iterations with
  | 0 -> List.length school
  | n -> computeAnswer (nextGeneration school []) (iterations - 1)


let parseInput (file: string): school = List.map int_of_string (String.split_on_char ',' (input_line (open_in file)))

let file = Sys.argv.(1);;

Printf.printf "The asnwer is: %n\n" (computeAnswer (parseInput file) 80)
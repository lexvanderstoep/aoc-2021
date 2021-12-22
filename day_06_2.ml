type school = (int * int * int * int * int * int * int * int * int)

let nextGeneration (school: school): school =
  match school with 
  | (a, b, c, d, e, f, g, h, i) -> (b, c, d, e, f, g, h + a, i, a)

let computeSize (school: school) =
  match school with 
  | (a, b, c, d, e, f, g, h, i) -> a + b + c + d + e + f + g + h + i


let rec computeAnswer (school: school) (iterations: int): int = 
  match iterations with
  | 0 -> computeSize school
  | n -> computeAnswer (nextGeneration school) (iterations - 1)


let countOccurences (elements: int list) (num: int): int =
  List.length (List.filter (fun element -> element = num) elements)

let parseSchoolList (schoolList: int list): school =
  (countOccurences schoolList 0,
   countOccurences schoolList 1,
   countOccurences schoolList 2,
   countOccurences schoolList 3,
   countOccurences schoolList 4,
   countOccurences schoolList 5,
   countOccurences schoolList 6,
   countOccurences schoolList 7,
   countOccurences schoolList 8)

let parseInput (file: string): school = parseSchoolList (List.map int_of_string (String.split_on_char ',' (input_line (open_in file))))

let file = Sys.argv.(1);;

Printf.printf "The asnwer is: %n\n" (computeAnswer (parseInput file) 256)
open System.IO

printfn "Hello, world!"

let readLines (filePath: string): seq<string> = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type display = Set<int>
type permutation = List<int>

let digits: list<display> = [
    Set.ofList [0; 1; 2; 4; 5; 6];
    Set.ofList [2; 5];
    Set.ofList [0; 2; 3; 4; 6];
    Set.ofList [0; 2; 3; 5; 6];
    Set.ofList [1; 2; 3; 5];
    Set.ofList [0; 1; 3; 5; 6];
    Set.ofList [0; 1; 3; 4; 5; 6];
    Set.ofList [0; 2; 5];
    Set.ofList [0; 1; 2; 3; 4; 5; 6];
    Set.ofList [0; 1; 2; 3; 5; 6]
]

let parseChar (char): int = 
    if char = 'a' then
        0
    else if char = 'b' then
        1
    else if char = 'c' then
        2
    else if char = 'd' then
        3
    else if char = 'e' then
        4
    else if char = 'f' then
        5
    else
        6

let parseDisplay (segment: string): display = Set.map parseChar (Set.ofSeq segment)

let parseLine (line: string): list<display> * list<display> =
    let splitLine: string[] = line.Split " | "
    let uniqueDigits: list<string> = Array.toList ((splitLine.[0]).Split " ")
    let puzzleDigits: list<string> = Array.toList ((splitLine.[1]).Split " ")
    List.map parseDisplay uniqueDigits, List.map parseDisplay puzzleDigits

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let generatePermutations(list: List<int>): List<permutation> = permute list

let applyPermutation (wiring: permutation) (digit: display): display = Set.map (fun segment -> wiring.[segment]) digit

let isValidDigit (digitToTest: display): bool = List.exists (fun digit -> digit = digitToTest) digits

let is_valid_permutation (uniqueDigits: list<display>) (wiring: permutation): bool = 
    let descrambledDigits = List.map (applyPermutation wiring) uniqueDigits
    List.forall isValidDigit descrambledDigits

let computeWirePermutations(uniqueDigits: list<display>): permutation = 
    let permutations = generatePermutations [0; 1; 2; 3; 4; 5; 6]
    List.find (is_valid_permutation uniqueDigits) permutations

let computeDigit (displayedDigit: display): int = List.findIndex (fun digit -> digit = displayedDigit) digits

let computeScrambledDigit (scrambledDigit: display) (wiring: permutation): int =
    computeDigit (applyPermutation wiring scrambledDigit)

let computePuzzleNumber(uniqueDigits: list<display>, puzzle_digits: list<display>): int =
    let wiring = computeWirePermutations uniqueDigits 
    1000 * computeScrambledDigit (puzzle_digits.[0]) wiring +
    100  * computeScrambledDigit (puzzle_digits.[1]) wiring +
    10   * computeScrambledDigit (puzzle_digits.[2]) wiring +
    1    * computeScrambledDigit (puzzle_digits.[3]) wiring


let computeAnswer(input: list<list<display> * list<display>>): int =
    List.sum (List.map computePuzzleNumber input)

let lines = Seq.toList (readLines("input/input_8_2.txt"))
let input = List.map parseLine lines
let answer = computeAnswer input
printfn "Anwer: %i" answer
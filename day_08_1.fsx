open System.IO

let readLines (filePath: string): seq<string> = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseDisplay (segment: string): list<char> = Seq.toList segment

let parseLine (line: string): list<list<char>> * list<list<char>> =
    let splitLine: string[] = line.Split " | "
    let uniqueDigits: list<string> = Array.toList ((splitLine.[0]).Split " ")
    let puzzleDigits: list<string> = Array.toList ((splitLine.[1]).Split " ")
    List.map parseDisplay uniqueDigits, List.map parseDisplay puzzleDigits

let digitHasUniqueNumberOfSegments (digit: list<char>): bool =
    let numberOfSegments = List.length digit
    List.contains numberOfSegments [2; 3; 4; 7]

let computeAnswer (input: list<list<list<char>> * list<list<char>>>): int =
    let answerPerDisplay = List.map (fun (uniqueDigits, puzzleDigits) -> List.length (List.filter (fun digit -> digitHasUniqueNumberOfSegments digit) puzzleDigits)) input
    List.sum answerPerDisplay

let lines = Seq.toList (readLines("input/input_8_1.txt"))
let input = List.map parseLine lines
let answer = computeAnswer input
printfn "Anwer: %i" answer
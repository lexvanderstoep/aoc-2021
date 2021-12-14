import java.io.File
import java.util.Stack

fun readInput(fileName: String): List<List<Char>> = File(fileName).readLines().map({line -> line.toList()})

fun computeAutoCompleteScore(line: List<Char>): Long {
    val symbols = Stack<Char>()
    for (symbol in line) {
        when (symbol) {
            '(', '[', '{', '<' -> symbols.push(symbol)
            ')' -> {
                if (symbols.empty() || symbols.peek() != '(') {
                    return 0
                } else {
                    symbols.pop()
                }
            }
            ']' -> {
                if (symbols.empty() || symbols.peek() != '[') {
                    return 0
                } else {
                    symbols.pop()
                }
            }
            '}' -> {
                if (symbols.empty() || symbols.peek() != '{') {
                    return 0
                } else {
                    symbols.pop()
                }
            }
            '>' -> {
                if (symbols.empty() || symbols.peek() != '<') {
                    return 0
                } else {
                    symbols.pop()
                }
            }

        }
    }
    
    var score: Long = 0
    while(!symbols.empty()) {
        val symbol = symbols.pop()
        when(symbol) {
            '(' -> {score = score * 5 + 1}
            '[' -> {score = score * 5 + 2}
            '{' -> {score = score * 5 + 3}
            '<' -> {score = score * 5 + 4}
        }
    }
    return score
}

fun computeAnswer(input: List<List<Char>>): Long {
    val scores = input.map({line -> computeAutoCompleteScore(line)}).filter({score -> score != 0L}).toMutableList()
    for (score in scores) {
        println(score)
    }
    scores.sort()
    return scores[scores.size / 2]
}

fun main(args: Array<String>) {
    val fileName = args[0]
    val input = readInput(fileName)
    val answer = computeAnswer(input)
    println("Answer: " + answer)
}
import java.io.File
import java.util.Stack

fun readInput(fileName: String): List<List<Char>> = File(fileName).readLines().map({line -> line.toList()})

fun computeSyntaxErrorScore(line: List<Char>): Int {
    val symbols = Stack<Char>()
    for (symbol in line) {
        when (symbol) {
            '(', '[', '{', '<' -> symbols.push(symbol)
            ')' -> {
                if (symbols.empty() || symbols.peek() != '(') {
                    return 3
                } else {
                    symbols.pop()
                }
            }
            ']' -> {
                if (symbols.empty() || symbols.peek() != '[') {
                    return 57
                } else {
                    symbols.pop()
                }
            }
            '}' -> {
                if (symbols.empty() || symbols.peek() != '{') {
                    return 1197
                } else {
                    symbols.pop()
                }
            }
            '>' -> {
                if (symbols.empty() || symbols.peek() != '<') {
                    return 25137
                } else {
                    symbols.pop()
                }
            }

        }
    }
    return 0
}

fun computeAnswer(input: List<List<Char>>): Int = input.map({line -> computeSyntaxErrorScore(line)}).sum()

fun main(args: Array<String>) {
    val fileName = args[0]
    println("Hello, World!")
    val input = readInput(fileName)
    val answer = computeAnswer(input)
    println("Answer: " + answer)
}
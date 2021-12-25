import scala.io.Source
import scala.util.Using

sealed abstract class Number extends Product with Serializable

object Number {
  final case class Pair(left: Number, right: Number) extends Number
  final case class Regular(value: Int) extends Number
}

object Main {
    def parseNumber(input: String): (Number, Int) = {
        if (input.charAt(0).isDigit) { // Regular
            return (Number.Regular(input.slice(0, 1).toInt), 1)
        } else if (input.charAt(0) == '[') { // Pair
            val (firstNumber, firstNumberBits) = parseNumber(input.slice(1, input.length()))
            if (input.charAt(1 + firstNumberBits) != ',') {
                throw new RuntimeException("Invalid state")
            }
            val (secondNumber, secondNumberBits) = parseNumber(input.slice(1 + firstNumberBits + 1, input.length()))
            if (input.charAt(1 + firstNumberBits + 1 + secondNumberBits) != ']') {
                throw new RuntimeException("Invalid state")
            }
            return (Number.Pair(firstNumber, secondNumber), 1 + firstNumberBits + 1 + secondNumberBits + 1)
        } else {
            throw new RuntimeException("Invalid state")
        }
    }

    def printNumber(number: Number): String = {
        if (number.isInstanceOf[Number.Pair]) {
            val pair = number.asInstanceOf[Number.Pair]
            return "[" + printNumber(pair.left) + "," + printNumber(pair.right) + "]"
        } else {
            val regular = number.asInstanceOf[Number.Regular]
            return regular.value.toString
        }
    }

    def addToLeft(number: Number, value: Number.Regular): Number = {
        if (number.isInstanceOf[Number.Regular]) {
            val regular = number.asInstanceOf[Number.Regular]
            return Number.Regular(regular.value + value.value)
        }
        val pair = number.asInstanceOf[Number.Pair]
        return Number.Pair(addToLeft(pair.left, value), pair.right)
    }

    def addToRight(number: Number, value: Number.Regular): Number = {
        if (number.isInstanceOf[Number.Regular]) {
            val regular = number.asInstanceOf[Number.Regular]
            return Number.Regular(regular.value + value.value)
        }
        val pair = number.asInstanceOf[Number.Pair]
        return Number.Pair(pair.left, addToRight(pair.right, value))
    }

    def tryExplode(number: Number, depth: Int): (Number, Option[Number.Regular], Option[Number.Regular]) = {
        if (number.isInstanceOf[Number.Regular]) {
            return (number, None, None)
        }
        val pair = number.asInstanceOf[Number.Pair]
        if (depth >= 4 & pair.left.isInstanceOf[Number.Regular] & pair.right.isInstanceOf[Number.Regular]) {
            return (Number.Regular(0), Some(pair.left.asInstanceOf[Number.Regular]), Some(pair.right.asInstanceOf[Number.Regular]))
        } else {
            val (explodedLeft, addLeft, addRight) = tryExplode(pair.left, depth + 1)
            if (explodedLeft != pair.left) {
                if (addRight.nonEmpty) {
                    return (Number.Pair(explodedLeft, addToLeft(pair.right, addRight.get)), addLeft, None)
                }
                return (Number.Pair(explodedLeft, pair.right), addLeft, addRight)
            } else {
                val (explodedRight, addLeft, addRight) = tryExplode(pair.right, depth + 1)
                if (addLeft.nonEmpty) {
                    return (Number.Pair(addToRight(pair.left, addLeft.get), explodedRight), None, addRight)
                }
                return (Number.Pair(explodedLeft, explodedRight), addLeft, addRight)
            }
        }
    }

    def trySplit(number: Number): Number = {
        if (number.isInstanceOf[Number.Regular]) {
            val regular = number.asInstanceOf[Number.Regular]
            if (regular.value >= 10) {
                return Number.Pair(Number.Regular((regular.value.toFloat / 2.0).floor.toInt), Number.Regular((regular.value.toFloat / 2.0).ceil.toInt))
            } else {
                return regular
            }
        } else {
            val pair = number.asInstanceOf[Number.Pair]
            val splitLeft = trySplit(pair.left)
            if (splitLeft == pair.left) {
                return Number.Pair(splitLeft, trySplit(pair.right))
            } else {
                return Number.Pair(splitLeft, pair.right)
            }
        }
    }

    def reduce(number: Number): Number = {
        val (explodedNumber, _, _) = tryExplode(number, 0)
        if (explodedNumber != number) {
            return explodedNumber
        }
        return trySplit(number)
    }

    def fullyReduce(number: Number): Number = {
        val reducedNumber = reduce(number)
        if (reducedNumber == number) {
            return reducedNumber
        } else {
            return fullyReduce(reducedNumber)
        }
    }

    def add(left: Number, right: Number): Number = {
        return fullyReduce(Number.Pair(left, right))
    }

    def magnitude(number: Number): Int = {
        if (number.isInstanceOf[Number.Regular]) {
            return number.asInstanceOf[Number.Regular].value
        } else {
            return 3 * magnitude(number.asInstanceOf[Number.Pair].left) + 2 * magnitude(number.asInstanceOf[Number.Pair].right)
        }
    }

    def computeAnswer(numbers: List[Number]): Int = {
        var result = numbers(0)
        for (i <- 1 to (numbers.length - 1)) {
            result = add(result, numbers(i))
        }

        return magnitude(result)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("input/input_18_1.txt")
        val numbers = source.getLines().map(line => parseNumber(line)._1).toList
        source.close()

        val answer = computeAnswer(numbers)
        println(answer)
    }
}
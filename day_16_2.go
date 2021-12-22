package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

type Packet interface {
	getVersion() int
	getTypeId() int
	getValue() int
}

type Literal struct {
	version int
	typeId  int
	value   int
}

func (literal Literal) getVersion() int {
	return literal.version
}

func (literal Literal) getTypeId() int {
	return literal.typeId
}

func (literal Literal) getValue() int {
	return literal.value
}

type Expression struct {
	version    int
	typeId     int
	subPackets []Packet
}

func (expression Expression) getVersion() int {
	return expression.version
}

func (expression Expression) getTypeId() int {
	return expression.typeId
}

func (expression Expression) getValue() int {
	switch expression.getTypeId() {
	case 0:
		sum := 0
		for _, subPacket := range expression.subPackets {
			sum += subPacket.getValue()
		}
		return sum
	case 1:
		product := 1
		for _, subPacket := range expression.subPackets {
			product *= subPacket.getValue()
		}
		return product
	case 2:
		minimum := math.MaxInt
		for _, subPacket := range expression.subPackets {
			minimum = int(math.Min(float64(minimum), float64(subPacket.getValue())))
		}
		return minimum
	case 3:
		maximum := math.MinInt
		for _, subPacket := range expression.subPackets {
			maximum = int(math.Max(float64(maximum), float64(subPacket.getValue())))
		}
		return maximum
	case 5:
		if expression.subPackets[0].getValue() > expression.subPackets[1].getValue() {
			return 1
		} else {
			return 0
		}
	case 6:
		if expression.subPackets[0].getValue() < expression.subPackets[1].getValue() {
			return 1
		} else {
			return 0
		}
	case 7:
		if expression.subPackets[0].getValue() == expression.subPackets[1].getValue() {
			return 1
		} else {
			return 0
		}
	default:
		panic("Unexpected expression typeId")
	}
}

func readInput(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		return scanner.Text()
	}

	panic("No input line was available")
}

func parseHexadecimal(input string) []uint8 {
	var hexadecimal []uint8
	for _, char := range input {
		value, err := strconv.ParseUint(string(char), 16, 4)
		if err != nil {
			panic("Could not read hexadecimal digit")
		}
		hexadecimal = append(hexadecimal, uint8(value))
	}
	return hexadecimal
}

func hexadecimalDigitToBinaryDigits(hexadecimal uint8) []uint8 {
	var binaryDigits []uint8 = []uint8{0, 0, 0, 0}
	if hexadecimal >= 8 {
		binaryDigits[0] = 1
		hexadecimal -= 8
	}
	if hexadecimal >= 4 {
		binaryDigits[1] = 1
		hexadecimal -= 4
	}
	if hexadecimal >= 2 {
		binaryDigits[2] = 1
		hexadecimal -= 2
	}
	if hexadecimal >= 1 {
		binaryDigits[3] = 1
		hexadecimal -= 1
	}

	if hexadecimal != 0 {
		panic("A logic error occurred while converting hexadecimal to binary")
	}

	return binaryDigits
}

func parseBinary(hexadecimal []uint8) []uint8 {
	var binary []uint8
	for _, hexadecimalDigit := range hexadecimal {
		binaryDigits := hexadecimalDigitToBinaryDigits(hexadecimalDigit)
		binary = append(binary, binaryDigits...)
	}
	return binary
}

func binaryToDecimal(binary []uint8) int {
	decimal := 0
	for i := len(binary) - 1; i >= 0; i-- {
		binaryDigit := binary[i]
		decimal += int(binaryDigit) * int(math.Pow(2, float64(len(binary)-i-1)))
	}
	return decimal
}

func parseLiteral(content []uint8) (int, int) {
	var binary []uint8
	readBits := 0
	for i := 0; i < len(content); i += 5 {
		binary = append(binary, content[i+1:i+5]...)
		readBits += 5
		if content[i] == 0 {
			break
		}
	}
	return binaryToDecimal(binary), readBits
}

func parseExpression(content []uint8) ([]Packet, int) {
	lengthTypeId := content[0]
	readBits := 1
	var subPackets []Packet
	if lengthTypeId == 0 {
		totalLength := binaryToDecimal(content[1:16]) + 16
		readBits += 15
		for readBits < totalLength {
			packet, packetBits := parsePacket(content[readBits:])
			subPackets = append(subPackets, packet)
			readBits += packetBits
		}
		return subPackets, readBits
	} else {
		numberOfSubPackets := binaryToDecimal(content[1:12])
		readBits += 11
		for i := 0; i < numberOfSubPackets; i++ {
			packet, packetBits := parsePacket(content[readBits:])
			subPackets = append(subPackets, packet)
			readBits += packetBits
		}
		return subPackets, readBits
	}
}

func parsePacket(binary []uint8) (Packet, int) {
	version := binaryToDecimal(binary[0:3])
	typeId := binaryToDecimal(binary[3:6])
	if typeId == 4 {
		literalValue, readBits := parseLiteral(binary[6:])
		return Literal{version, typeId, literalValue}, readBits + 6
	} else {
		subPackets, readBits := parseExpression(binary[6:])
		return Expression{version, typeId, subPackets}, readBits + 6
	}
}

func main() {
	input := readInput("input/input_16_2.txt")
	hexadecimal := parseHexadecimal(input)
	binary := parseBinary(hexadecimal)
	packet, _ := parsePacket(binary)
	fmt.Println(packet.getValue())
}

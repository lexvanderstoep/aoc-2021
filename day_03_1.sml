let 
    fun nth(xs, i) =
	    if i < 0 
	        then raise Subscript
	    else
		    case xs of
			        [] => raise Subscript
		    | (x::xs') => if i=0 then x else nth(xs',i-1)
    fun dropLast l =
        case l of []    => []
              |   v::[] => []
              |   v::t  => v::(dropLast t)
    fun readList (infile : string) = let
            val ins = TextIO.openIn infile
            fun loop ins =
            case TextIO.inputLine ins of
                SOME line => (implode (dropLast (explode line))) :: loop ins
              | NONE      => []
        in
            loop ins before TextIO.closeIn ins
        end
    val file = hd (CommandLine.arguments()) 

    fun parseLine (line : string): int list =
        map (fn c => valOf (Int.fromString (implode [c]))) (explode line)

    fun countNthBit (numbers: int list list, index: int): int = 
        List.foldl (op +) 0 (map (fn line => nth(line, index)) numbers)

    fun computeGammaBit (c: int, l: int) =
        if c + c >= l
            then 1
        else 0

    fun toDecimal (bits: int list): int =
        let fun toDecimalHelper (bits: int list, mult) =
                case bits of [] => 0
                           | (b::bs) => mult * b + toDecimalHelper (bs, mult * 2)
        in 
            toDecimalHelper (rev bits, 1)
        end

    fun computeAnswer (lines : string list) =  let
            val numbers = map parseLine lines
            val len = length numbers
            val width = length (hd numbers)
            val bits = List.tabulate (width, fn i => (computeGammaBit ((countNthBit (numbers, i)), len)))
            val flippedBits = map (fn num => (1 - num)) bits
            val gamma = toDecimal bits
            val epsilon = toDecimal flippedBits
        in
            gamma * epsilon
        end
in
    print (Int.toString(computeAnswer (readList file))); print "\n";
    OS.Process.exit OS.Process.success : unit
end;
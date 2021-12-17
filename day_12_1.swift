import Cocoa

func parseInput(filename: String) -> [String: [String]] {
    var graph: [String: [String]] = [:]
    let data: String = try! String(contentsOfFile: filename)
    let lines = data.components(separatedBy: "\n")
    for line in lines {
        let nodes = line.components(separatedBy: "-")
        let firstNode = nodes[0]
        let secondNode = nodes[1]
        if (graph[firstNode] == nil) {
            graph[firstNode] = []
        }
        if (graph[secondNode] == nil) {
            graph[secondNode] = []
        }
        graph[firstNode]!.append(secondNode)
        graph[secondNode]!.append(firstNode)
    }
    return graph
}

func createCapacityGraph(graph: [String: [String]]) -> [String: Int] {
    var capacityGraph: [String: Int] = [:]
    for (node, _) in graph {
        if (node.lowercased() == node) {
            capacityGraph[node] = 1
        } else {
            capacityGraph[node] = Int.max
        }
    }
    return capacityGraph
}

func findAllPaths(graph: [String: [String]], capacityGraph: [String: Int], node: String, endNode: String) -> Int {
    if (node == endNode) {
        return 1
    }

    var numberOfPaths = 0
    var capacityGraphCopy = capacityGraph
    let adjacentNodes: [String] = graph[node]!
    for adjacentNode in adjacentNodes {
        if (capacityGraphCopy[adjacentNode]! > 0) {
            capacityGraphCopy[adjacentNode] = capacityGraphCopy[adjacentNode]! - 1
            numberOfPaths += findAllPaths(graph: graph, capacityGraph: capacityGraphCopy, node: adjacentNode, endNode: endNode)
            capacityGraphCopy[adjacentNode] = capacityGraphCopy[adjacentNode]! + 1
        }
    }

    return numberOfPaths
}

func computeAnswer(graph: [String: [String]]) -> Int {
    var capacityGraph = createCapacityGraph(graph: graph)
    capacityGraph["start"] = 0
    return findAllPaths(graph: graph, capacityGraph: capacityGraph, node: "start", endNode: "end")
}

let graph = parseInput(filename: "input/input_12_1.txt")

print("The answer is \(computeAnswer(graph: graph))")

// Read input
//
// Do a simple depth-first search -- there shouldn't be any loops.
// Keep track of the number of allowed visits left for each node.
// Big caves have infinite visits, small caves have limited visits.
// When making a "decision" to go down a certain edge, subtract 1, 
// and then add one again when done with the decision.
// Have the recursive function simply return the number of paths 
// that it gives.
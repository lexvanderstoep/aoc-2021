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

func countVisits(path: [String]) -> [String: Int] {
    var visits: [String: Int] = [:]
    for node in path {
        if (visits[node] == nil) {
            visits[node] = 0
        }
        visits[node] = visits[node]! + 1
    }
    return visits
}

func isValidPath(path: [String]) -> Bool {
    let visits = countVisits(path: path)
    var doubleVisit = false
    for (node, visitCount) in visits {
        if (node == "start" && visitCount != 1) {
            return false
        }
        if (node == "end" && visitCount > 1) {
            return false
        }
        if (visitCount == 2 && node.lowercased() == node) {
            if (doubleVisit) {
                return false
            } else {
                doubleVisit = true
            }
        }
        if (visitCount > 2 && node.lowercased() == node) {
            return false
        }
    }
    return true
}

func findAllPaths(graph: [String: [String]], node: String, endNode: String, path: [String]) -> [[String]] {
    if (node == endNode) {
        return [path]
    }

    var paths: [[String]] = []
    let adjacentNodes = graph[node]!
    for adjacentNode in adjacentNodes {
        let newPath = path + [adjacentNode]
        if (isValidPath(path: newPath)) {
            paths += findAllPaths(graph: graph, node: adjacentNode, endNode: endNode, path: newPath)
        }
    }

    return paths
}

func computeAnswer(graph: [String: [String]]) -> Int {
    return findAllPaths(graph: graph, node: "start", endNode: "end", path: ["start"]).count
}

let graph = parseInput(filename: "input/input_12_2.txt")

print("The answer is \(computeAnswer(graph: graph))")

open System.IO
open System.Text.RegularExpressions

module Question1 =
    let debug = false
    let path = if debug then @"..\input\day7-sample.txt" else @"..\input\day7.txt"
    let input = File.ReadAllLines path
    let listofBranches, listOfLeaves = input |> Array.partition (fun s -> s.Contains "->")

    let leafRegex = Regex @"(?<name>.*) \((?<weight>\d+)\)"
    let branchRegex = Regex @"(?<name>.*) \((?<weight>\d+)\) -> (?<nodes>.*)$"

    let leaves =
        listOfLeaves
        |> Array.map ( fun leaf ->
            let m = leafRegex.Match leaf
            let name, weight = m.Groups.["name"].Value, m.Groups.["weight"].Value
            name, int weight )
    
    let branches =
        listofBranches
        |> Array.map ( fun branch ->
            let m = branchRegex.Match branch
            let name, weight, nodes = m.Groups.["name"].Value, m.Groups.["weight"].Value, m.Groups.["nodes"].Value
            let ns = nodes.Split(',') |> Array.map ( fun x -> x.Trim() )
            (name, int weight), ns)
    
    // From the branches, decompose the tuple down to get either the second item (nodes)
    // or the first item's, first item (names), and then flatten result if needed
    // into a single array which can be converted to a set
    let branchNodes = branches |> Array.map snd |> Array.collect id |> Set.ofArray
    let branchRoots = branches |> Array.map (fst >> fst) |> Set.ofArray
    let leafNodes = leaves |> Array.map fst |> Set.ofArray

    // the order here matters (set2 - set1). Remove the branch nodes from the branch roots to be left with
    // the unique root
    let distinctBranches = Set.difference branchRoots branchNodes

    // Now remove any of those branches that are leaves
    let distinctNode = Set.difference distinctBranches leafNodes
    let root =
        distinctNode
        |> List.ofSeq
        |> List.head
    
    let answer () = root

Question1.answer () |> printfn "Question 1: %s"

module Question2 =
    // Given that we parse everything in question1, let's re-use that.
    let debug = true
    let leaves = Question1.leaves
    let branches = Question1.branches |> Array.map fst
    let branchNodes = Question1.branches |> Array.map (fun (root, nodes) -> fst root, nodes) |> Map.ofArray

    let map =
        leaves
        |> Array.append branches
        |> Map.ofArray

    let rec sum acc nodes =
        match nodes with
        | [] -> acc
        | node::t ->
            let keyExists = branchNodes |> Map.containsKey node
            match keyExists with
            | true ->
                let cs = branchNodes.[node] |> List.ofArray
                let total = cs |> List.sumBy (fun c -> map.[c])
                let list = t @ cs
                sum (acc + total) list
            | false -> sum acc t
    
    // I have to be honest, this question took much longer for me to figure out than it should have
    let rec findUnbalancedDifference balanceWeight node =
        // get the root's children, and sum up the trees and the child node weights
        let children = branchNodes.[node]
        let sums = children |> Array.map ( fun child -> map.[child] + (sum 0 [child]) )

        if debug then
            printfn "node=%s balanceWeight=%d" node balanceWeight
            printfn "children=%A | sums=%A | " children sums

        // singling out the distinct item in the group and getting the index was tough,
        // until my brain started working again and I remembered that I could filter a sequence
        let distinct =
            sums
            |> Array.mapi (fun i s -> i, s)
            |> Array.groupBy snd
            |> Array.filter (fun (_, group) -> group.Length = 1)

        if debug then printfn "distinct = %A" distinct
        match distinct with
        | [||] -> node, balanceWeight
        | dnode ->
            let idx, value = dnode |> Array.head |> snd |> Array.head
            let balancedIdx = (idx + 1) % sums.Length
            let diff = sums.[balancedIdx] - value
            let nodeValue = map.[ children.[idx] ]
            let bw = nodeValue + diff
            if debug then
                printfn "node=%s value=%d" children.[idx] map.[ children.[idx] ]
                printfn "idx=%d balancedIdx=%d diff=%d bw=%d" idx balancedIdx diff bw

            findUnbalancedDifference bw children.[idx]

    let answer () =
        let node, difference = findUnbalancedDifference 0 Question1.root
        node, difference

Question2.answer () |> printfn "Question 2: %A"

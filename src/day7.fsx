open System.IO
open System.Text.RegularExpressions
open System.Security.AccessControl

module Question1 =
    let debug = false
    let path = if debug then @"..\input\day7-q1-example.txt" else @"..\input\day7-q1.txt"
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

    let leaves = Question1.leaves
    let branches = Question1.branches |> Array.map fst
    let branchNodes = Question1.branches |> Array.map (fun (root, nodes) -> fst root, nodes) |> Map.ofArray

    let map =
        leaves
        |> Array.append branches
        |> Map.ofArray

    let findUnbalancedDifference () =
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

        // get the root's children, and sum up the trees and the child node weights
        let root = Question1.root
        let children = branchNodes.[root] |> List.ofArray
        let sums = children |> List.map ( fun child -> map.[child] + (sum 0 [child]) )

        // find the outlier (unbalanced value) in the result
        let outlierIndex =
            sums
            |> List.countBy id
            |> List.findIndex (fun count -> snd count = 1 )
        
        let unbalanced = sums |> List.item outlierIndex
        let balanced =  List.item ((outlierIndex + 1) % sums.Length) sums

        printfn "unbalanced: %A, balanced = %A" unbalanced balanced
        printfn "children = %A" children // (children |> List.map (fun x -> map.[x]))
        printfn "sums = %A" sums

        let difference = unbalanced - balanced
        let program = children |> List.item outlierIndex
        let weight = map.[program]

        // if our unbalanced program weighs more than our balanced programs, then subtract the difference
        printfn "difference = %d, weight = %d" difference weight
        if difference > 0 then weight - difference else weight + difference

    let answer = findUnbalancedDifference

//Question2.answer () |> printfn "Question 2: %A"
// Question 2 is unfinished. 

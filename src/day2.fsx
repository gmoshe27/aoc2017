open System.IO

module Question1 =
    let input = File.ReadLines @"..\input\day2.txt"

    let orderedList =
        input
        |> Seq.map (fun x -> x.Split('\t'))
        |> Seq.map (fun xs -> xs |> Array.map int |> Array.sort)

    // let orderedList = 
    //     [
    //         [|5; 1; 9; 5|];
    //         [|7; 5; 3|];
    //         [|2; 4; 6; 8|]
    //     ]
    //     |> List.toSeq
    //     |> Seq.map (fun xs -> xs |> Array.map int |> Array.sort)

    let checksum (xs:seq<int []>) =
        xs
        |> Seq.fold (fun acc line -> 
            let min = line.[0]
            let max = line.[line.Length - 1]
            acc + (max - min))
            0

Question1.checksum Question1.orderedList |> printfn "Question 1 - %d"

module Question2 =
    let input = File.ReadLines @"..\input\day2-q2.txt"

    let orderedList =
        input
        |> Seq.map (fun x -> x.Split('\t'))
        |> Seq.map (fun xs -> xs |> Array.map int |> Array.sortBy (fun x -> -x))

    // let orderedList = 
    //     [
    //         [|5; 9; 2; 8|];
    //         [|9; 4; 7; 3|];
    //         [|3; 8; 6; 5|]
    //     ]
    //     |> List.toSeq
    //     |> Seq.map (fun xs -> xs |> Array.map int |> Array.sortBy ( ~- ))

    let modsum (xs:seq<int []>) =
        let rec getWholeNumber nums =
            match nums with
            | [] -> 0
            | h::t ->
                let value = t |> List.tryFind (fun x -> h % x = 0)
                match value with
                | Some v -> h / v
                | None -> getWholeNumber t

        xs |> Seq.sumBy (Array.toList >> getWholeNumber)

Question2.modsum Question2.orderedList |> printfn "Question 2 - %d"
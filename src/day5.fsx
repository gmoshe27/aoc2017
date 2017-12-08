open System.IO

module Question1 =
    let debug = false
    let testInput = [| 0; 3; 0; 1; -3 |]
    let input = File.ReadAllLines @"..\input\day5-q1.txt"

    let rec execute step total (input:int []) =
        let curr = input.[step]
        let next = curr + step
        let t = total + 1
        input.[step] <- curr + 1

        if next >= input.Length then
            t
        else
            execute next t input
    
    if debug then printfn "final counts: %A" input

    let input' = input |> Array.map int
    let answer () = execute 0 0 input'

Question1.answer () |> printfn "Question 1: %d"
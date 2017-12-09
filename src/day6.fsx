open System.IO

module Question1 =
    let debug = false
    let input = File.ReadAllLines @"..\input\day6-q1.txt"
    // let memory = input.[0].Split('\t') |> Array.map int // if debug then [|0; 2; 7; 0|] else input.[0].Split('\t') |> Array.map int
    let memory = if debug then [|0; 2; 7; 0|] else input.[0].Split('\t') |> Array.map int
    let len = memory.Length

    // Note: I initially hosed myself here by just mapping everything to a concatenated string (ex: [1; 2; 3] -> "123")
    // but I found out that there were some sequences that would look like repetitions but they were actually concatenations
    // of different numbers. Adding the ',' in-between the blocks makes all the difference
    let key (mem:int[]) = mem |> Array.map string |> String.concat ","

    // let's try this using a map instead of a dict, just to make it more interesting
    let map = [(key memory, 1)] |> Map.ofList

    let log map =
        if debug then
            printfn "memory = %A" memory
            printfn "map = %A" map

    log map

    let rec distribute index blocks mem =
        if blocks = 0 then
            key mem
        else
            let next = (index + 1) % len
            mem.[next] <- mem.[next] + 1
            if debug then log map
            distribute next (blocks - 1) mem

    let rec reallocate (map:Map<string, int>) mem =
        let max = mem |> Array.max
        let index = mem |> Array.findIndex (fun x -> x = max)

        if debug then printfn "max = %d index = %d" max index

        mem.[index] <- 0
        let key = distribute index max mem
        if debug then log map

        if map.ContainsKey key then
            map.Count
        else
            let m = map.Add (key, 1)
            reallocate m mem

    let answer () = reallocate map memory

Question1.answer () |> printfn "Question 1: %A"
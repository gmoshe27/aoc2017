module Question1 =
    let debug = false

    let split c (s:string)= s.Split(c)

    let input, lengths =
        if debug then
            [|0; 1; 2; 3; 4|], [|3; 4; 1; 5|]
        else
            [|0..255|], ( "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243" |> split [|','|] |> Array.map int )
    
    let hash ( pos, skip, (arr:int[]) ) length =
        if length > arr.Length then
            (pos+1+skip), (skip+1), arr
        else
            let totalSelection = pos + length
            if totalSelection > arr.Length then
                // split the selections
                let rpos, rlength = pos, arr.Length - 1 - pos
                let lpos, llength = 0, length - 1 - rlength - 1
                let selection = arr.[lpos..llength] |> Array.append arr.[rpos..rpos+rlength]

                if debug then
                    printfn "rpos=%d, rlength=%d, lpos=%d, llength=%d" rpos rlength lpos llength
                    printfn "%A" arr

                let reversed = selection |> Array.rev
                for i in [0..rlength] do arr.[pos + i] <- reversed.[i]
                for i in [0..llength] do arr.[0 + i] <- reversed.[i+rlength+1]
            else
                let selection = arr.[pos..pos + length - 1]
                if debug then
                    printfn "selection = %A" selection
                    printfn "%A" arr

                let reversed = selection |> Array.rev
                for i in [0..length - 1] do arr.[pos + i] <- reversed.[i]
            
            let pos' = (pos + length + skip) % arr.Length
            pos', (skip + 1), arr

    // I'm using the fold function as a cheap way of doing the recursion
    let hashed = lengths |> Array.fold hash (0, 0, input)
    if debug then printfn "%A" hashed
    let answer () = input.[0] * input.[1]

Question1.answer () |> printfn "%A"

module Question2 =
    let debug = true
    let postfix = [|17; 31; 73; 47; 23|]
    let input =
        if debug then
            let examples = [""; "AoC 2017"; "1,2,3"; "1,2,4"]
            examples.[2]
        else
            "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
    
    let length =
        let toAsciiBytes (input:string) =
            input |> Seq.map (System.Convert.ToByte >> int ) |> Array.ofSeq
        Array.append (toAsciiBytes input) postfix

    let rounds = 64

    let sparseHash = ()
    let denseHash = ()
    let knotHash = ()


Question2.length |> printfn "%A"
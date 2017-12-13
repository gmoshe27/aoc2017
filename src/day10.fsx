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
    let debug = false
    let postfix = [|17; 31; 73; 47; 23|]
    let data =
        if debug then
            let examples = [""; "AoC 2017"; "1,2,3"; "1,2,4"]
            examples.[0]
        else
            "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
    
    let rounds = 64
    let input = [|0..255|]
    let lengths =
        let toAsciiBytes (input:string) =
            input |> Seq.map (System.Convert.ToByte >> int ) |> Array.ofSeq
        Array.append (toAsciiBytes data) postfix

    let computeSparseHash input =
        if debug then printfn "input = %A" input
        let _, _, sparseHash =
            [1..rounds]
            |> List.fold ( fun (pos, skip, arr) i ->
                // the hash function returns (pos, skip, arr), which can
                // be recursively fed back into the top-level fold for
                // the total number of rounds
                lengths |> Array.fold Question1.hash (pos, skip, arr) )
                (0, 0, input)
        sparseHash

    let computeDenseHash sparseHash =
        if debug then printfn "sparseHash = %A" sparseHash
        sparseHash
        |> Array.chunkBySize 16
        |> Array.map ( fun chunk -> chunk |> Array.fold (^^^) 0 )

    let computeKnotHash denseHash =
        if debug then printfn "denseHash = %A" denseHash else ()
        let toHex (x:int) = x.ToString("x2")
        denseHash |> Array.map toHex |> String.concat ""

    let answer () = input |> computeSparseHash |> computeDenseHash |> computeKnotHash

Question2.answer () |> printfn "Question 2: %A"
module Question1 =
    let rec findMatrixSize n input =
        if n * n >= input then
            n
        else
            // the spiral pattern generates odd sized matrices, increment by 2 to keep it odd
            findMatrixSize (n + 2) input
    
    let findSteps input =
        let n = findMatrixSize 1 input
        let rec findRightEdge n rightEdge input =
            let leftEdge = rightEdge - (n - 1)
            if leftEdge <= input then
                rightEdge
            else
                findRightEdge n leftEdge input
        
        let square = n * n
        let rightEdge = findRightEdge n square input
        let center = (n-1) / 2
        // find out how far away we are from the center
        let moveLeftRight = (rightEdge - input |> abs) - center |> abs
        // Each spiral is a square, so divide by 2 to get to the center
        let moveUpDown = (n - 1) / 2

        moveLeftRight + moveUpDown
    
    let answer () =
        printfn "--- Question 1 ---"
        printfn "Total Steps = %d" <| findSteps 1
        printfn "Total Steps = %d" <| findSteps 12
        printfn "Total Steps = %d" <| findSteps 23
        printfn "Total Steps = %d" <| findSteps 1024
        printfn "Total Steps = %d" <| findSteps 277678

//Question1.answer ()

module Question2 =

    type Move = Left | Right | Up | Down

    // This is cute, the matrix size is equal to the rounded square of the input number
    let findMatrixSize input = input |> float |> sqrt |> ceil |> int

    let addSpiral i j (arr:int [,]) =
        let left = arr.[i, j - 1]
        let right = arr.[i, j + 1]
        let up = arr.[i - 1, j]
        let down = arr.[i + 1, j]
        let upLeft = arr.[i - 1, j - 1]
        let downLeft = arr.[i + 1, j - 1]
        let upRight = arr.[i - 1, j + 1]
        let downRight = arr.[i + 1, j + 1]
        left + right + up + down + upLeft + downLeft + upRight + downRight

    let drawSpiral debug input =

        // initialize the matrix with padding around the borders for simplified adding
        let matrixSize = findMatrixSize input
        let size = matrixSize + 2
        let midpoint = size / 2
        let arr = Array2D.init size size (fun _ _-> 0)

        // start with 1 in the center
        arr.[midpoint, midpoint] <- 1

        // start spiraling
        let rec loop i j steps square move =
            let ki, kj =
                match move with
                | Right -> i, j + 1
                | Up -> i - 1, j
                | Left -> i, j - 1
                | Down -> i + 1, j

            if debug then printfn "addSpiral: %d, %d" ki kj
            arr.[ki, kj] <- arr |> addSpiral ki kj

            if arr.[ki, kj] > input then
                arr.[ki, kj]
            else
                if debug then printfn "%A" arr
                let nextMove =
                    match move with
                    | Right -> if steps + 1 = square then Move.Up else move
                    | Up -> if steps + 1 = (square - 2) then Move.Left else move
                    | Down -> if steps + 1 = (square - 1) then Move.Right else move
                    | Left -> if steps + 1 = (square - 1) then Move.Down else move

                let square' = if steps + 1 = square then square + 2 else square
                let steps' = if move <> nextMove then 0 else steps + 1

                if debug then printfn "next move = %A" nextMove
                loop ki kj steps' square' nextMove
        
        let i, j = midpoint, midpoint
        loop i j 0 1 Move.Right

    let answer debug input = drawSpiral debug input

Question2.answer true 50 |> printfn "Question 2 [%d]: %d" 50
Question2.answer false 277678 |> printfn "Question 2 [%d]: %d" 27678
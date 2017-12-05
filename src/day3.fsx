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

Question1.answer ()

module Question2 = ()
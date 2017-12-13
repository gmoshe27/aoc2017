open System.IO

module Question1 =
    let debug = true
    let data = if debug then @"..\input\day11-sample.txt" else @"..\input\day11.txt"
    let input = File.ReadAllLines data

    // I've never done a grid problem like this before, so this is my first time approaching it.
    // Drawing the grid has its x and y shifted

    //     0,1     0,3
    // 0,0     0,2
    //     1,1     1,3
    // 1,0     1,2
    //     2,1     2,3

    // So, knowing this, maybe we can update our x,y coordinates based on the direction taken

    // we'll handle an array of arrays so that we can do all of the sample inputs in one try
    let split sep (s:string) = s.Split( [|sep|] )
    let moves = input |> Array.map (split ',')

    if debug then printfn "moves = %A" moves

    // cool active pattern to switch on evens/odds
    let (|Even|Odd|) pos = if pos % 2 = 0 then Even else Odd

    let ifEven = function Even -> 1 | Odd -> 0
    let ifOdd = function Even -> 0 | Odd -> 1
    let decrement = (-)
    let increment = (+)
    
    let updatePos (x:int,y:int) move =
        // north and south always increment/decrement x without y
        // ne always increments y, but x is only decremented if y was odd
        // se always increments y, but x is only incremented if y was even
        // nw always decrements y, but x is only decremented if y was odd
        // sw always decrements y, but x is only incremented if y was even
        match move with
        | "n" -> (x-1), y
        | "s" -> (x+1), y
        | "ne" -> (ifOdd y |> decrement x), (y+1)
        | "se" -> (ifEven y |> increment x), (y+1)
        | "nw" -> (ifOdd y |> decrement x), (y-1)
        | "sw" -> (ifEven y |> increment x), (y-1)
        | _ -> failwith "not a valid direction"

    // these are the steps that the child took away from origin (0,0)
    let locations = moves |> Array.map ( fun ms -> ms |> Array.fold updatePos (0,0) )

    // TODO: these are the steps that we have to take back to get to origin (0,0)

    let answer () = locations

Question1.answer () |> printfn "Question 1: %A"
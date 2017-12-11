open System.IO

type Groups =
    {
        Level : int
        Counts : int list
        Garbage : bool
        GarbageCounts : int // for question 2
        Ignore : bool
    }
with
    static member zero =
        {
            Level = 0
            Counts = []
            Garbage = false
            GarbageCounts = 0
            Ignore = false
        }

module Question1 =
    let debug = false
    let data = if debug then @"..\input\day9-sample.txt" else @"..\input\day9-q1.txt"
    let input = File.ReadAllLines data

    // This question is similar to finding if a string has balanced parenthesis, which
    // can be implemented with a stack. The complexity in this question comes with keeping
    // how deep into the nesting you are.

    let sum (groups:Groups) (c:char) =
        if groups.Ignore then
            { groups with Ignore = false }
        else
            let garbageOr g = if not groups.Garbage then g else groups

            match c with
            | '!' -> { groups with Ignore = true }
            | '{' -> garbageOr { groups with Level = groups.Level + 1 }
            | '}' -> garbageOr { groups with Level = groups.Level - 1; Counts = [groups.Level] @ groups.Counts }
            | '<' -> garbageOr { groups with Garbage = true }
            | '>' -> { groups with Garbage = false }
            | _ -> groups // ignore, including ',' and garbage
    
    // since the sample input has more than one stream, we'll loop through the array of inputs
    // otherwise, we only need to fold over the stream (no real need for Array.map)
    let sums = input |> Array.map ( fun stream -> stream |> Seq.fold sum Groups.zero )

    let answer() = sums |> Array.map (fun x -> x.Counts |> List.sum)

Question1.answer () |> printfn "%A"

module Question2 =
    let sum (groups:Groups) (c:char) =
        if groups.Ignore then
            { groups with Ignore = false }
        else
            let garbageOr g =
                if not groups.Garbage then
                    g
                else
                    { groups with GarbageCounts = groups.GarbageCounts + 1 }

            match c with
            | '!' -> { groups with Ignore = true }
            | '{' -> garbageOr { groups with Level = groups.Level + 1 }
            | '}' -> garbageOr { groups with Level = groups.Level - 1; Counts = [groups.Level] @ groups.Counts }
            | '<' -> garbageOr { groups with Garbage = true }
            | '>' -> { groups with Garbage = false }
            | ',' -> garbageOr groups
            | _ -> garbageOr groups // ignore, including ',' and garbage
    
    // since the sample input has more than one stream, we'll loop through the array of inputs
    // otherwise, we only need to fold over the stream (no real need for Array.map)
    let sums = Question1.input |> Array.map ( fun stream -> stream |> Seq.fold sum Groups.zero )

    let answer () = sums |> Array.map (fun x -> x.GarbageCounts)

Question2.answer () |> printfn "%A"
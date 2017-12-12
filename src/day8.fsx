open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Expression =
    {
        Register : string
        Cmd : int -> int -> int
        Value : int
        Left : int
        Condition : int -> int -> bool
        Right : int
    }

module Question1 =
    let debug = false
    let data = if debug then @"..\input\day8-sample.txt" else @"..\input\day8.txt"
    let input = File.ReadAllLines data

    let regex = Regex @"(?<register>.*) (?<cmd>(inc|dec)) (?<cmdvalue>.*) if (?<a>.*) (?<condition>.*) (?<b>.*)$"

    // Because we can't know the register names in advance, we will use a dictionary
    // to hold the state of the dictionary returning 0 when it is not found and its
    // value if it is found.

    let registers = Dictionary<string, int>()

    // I tried my best to avoid the mutable value, but it's the best way to represent memory
    let mutable max = 0

    let getValue register (registers:Dictionary<string,int>) =
        // TryGetValue will return 0 if the key does not exist, otherwise, it will return
        // the value found int the dictionary
        let _, value = registers.TryGetValue register
        value
    
    let setValue register value (registers:Dictionary<string,int>) =
        registers.[register] <- value

    let parse statement =
        let m = regex.Match statement
        let register = m.Groups.["register"].Value
        let cmd = m.Groups.["cmd"].Value
        let value = m.Groups.["cmdvalue"].Value
        let condition = m.Groups.["condition"].Value
        let a = m.Groups.["a"].Value
        let b = m.Groups.["b"].Value

        let command =
            match cmd with
            | "inc" -> (+)
            | "dec" -> (-)
            | _ -> failwith "unknown command"

        let cond =
            match condition with
            | "==" -> (=)
            | "<" -> (<)
            | ">" -> (>)
            | ">=" -> (>=)
            | "<=" -> (<=)
            | "!=" -> (<>)
            | _ -> failwith "unknown condition"

        {
            Register = register
            Cmd = command
            Value = value |> int
            Left = registers |> getValue a
            Condition = cond
            Right = b |> int
        }

    // For Question 2
    let updateMax value = if value > max then value else max

    let evaluate e = if e.Condition e.Left e.Right then Some e else None
    let execute expression =
        match expression with
        | Some e ->
            let rvalue = registers |> getValue e.Register
            let value = e.Cmd rvalue e.Value
            registers |> setValue e.Register value
            max <- updateMax value
        | None -> ()

    // iterate through the input stream, updating the dictionary as we go along.
    // initially I thought that about replacing the dictionary with an F# map,
    // it overly complicates things, and this question behaves more like it is
    // modifying memory
    let executeCommands () =
        input |> Array.iter (parse >> evaluate >> execute)

    // http://theburningmonk.com/2012/08/f-converting-a-c-dictionary-to-a-map/
    let toSeq dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
    let answer () =
        executeCommands ()
        let max = registers |> toSeq |> Seq.maxBy snd
        snd max

Question1.answer () |> printfn "Question 1: %d"

module Question2 =
    let answer() = Question1.max

Question2.answer () |> printfn "Question 2: %d"
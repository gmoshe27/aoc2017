open System.IO

module Question1 =
    let input = File.ReadLines @"..\input\day4.txt"

    let isValidPassword debug (password:string) =
        let phrases = password.Split(' ')
        let phraseCounts = phrases |> Array.countBy id
        let hasRepeats = phraseCounts |> Array.exists (fun kv -> (snd kv) > 1)

        if debug then
            printfn "password: %s" password
            printfn "phraseCounts: %A" phraseCounts
            printfn "hasRepeats = %b" hasRepeats
            System.Console.ReadLine() |> ignore

        not hasRepeats

    let checkValidity debug password = if isValidPassword debug password then 1 else 0
    let totalValidPasswords debug = input |> Seq.sumBy (checkValidity debug)

    let answer debug = totalValidPasswords debug

Question1.answer false |> printfn "Total Valid Passwords Q1 = %d"

module Question2 =
    let input = File.ReadLines @"..\input\day4.txt"

    let isValidPassword debug (password:string) =
        let phrases = password.Split(' ')
        let phraseCounts =
            phrases
            |> Array.map ( Seq.sort >> System.String.Concat )
            |> Array.countBy id
        let hasRepeats = phraseCounts |> Array.exists (fun kv -> (snd kv) > 1)

        if debug then
            printfn "password: %s" password
            printfn "phraseCounts: %A" phraseCounts
            printfn "hasRepeats = %b" hasRepeats
            System.Console.ReadLine() |> ignore

        not hasRepeats

    let checkValidity debug password = if isValidPassword debug password then 1 else 0
    let totalValidPasswords debug = input |> Seq.sumBy (checkValidity debug)

    let answer debug = totalValidPasswords debug

Question2.answer false |> printfn "Total Valid Passwords Q2 = %d"

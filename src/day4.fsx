open System.IO

module Question1 =
    let input = File.ReadLines @"..\input\day4-q1.txt"

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

Question1.answer false |> printfn "Total Valid Passwords = %d"

module Question2 = ()
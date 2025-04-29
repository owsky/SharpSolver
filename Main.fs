module SharpSolver.Main

open FSharp.Text.Lexing
open Absyn
open System
open Prelude
open FSharp.Text
open Impl


let hout hd fmt =
    if not <| String.IsNullOrWhiteSpace hd then
        printf $"[{hd}]{String(' ', max 1 (Config.prefix_max_len - String.length hd))}"
        stdout.Flush()

    printfn fmt

let chout col hd fmt =
    let c = Console.ForegroundColor
    Console.ForegroundColor <- col

    Printf.kprintf
        (fun s ->
            hout hd "%s" s
            Console.ForegroundColor <- c)
        fmt

let out fmt = hout "" fmt
let cout col fmt = chout col "" fmt

let norm fmt = chout ConsoleColor.Yellow "norm" fmt
let redux fmt = chout ConsoleColor.Magenta "redux" fmt
let sol fmt = chout ConsoleColor.Green "sol" fmt
let ident fmt = chout ConsoleColor.Green "identity" fmt
let error fmt = chout ConsoleColor.Red "error" fmt

let deg fmt = chout ConsoleColor.Blue "degree" fmt

let interpreter_loop () =
    while true do
        printf $"\n{Config.prompt_prefix}"
        stdout.Flush()
        let input = Console.ReadLine()
        let lexbuf = LexBuffer<_>.FromString input

        let localized_error msg =
            let tabs = new string (' ', Config.prompt_prefix.Length + lexbuf.StartPos.Column)

            let cuts =
                new string ('^', let n = lexbuf.EndPos.Column - lexbuf.StartPos.Column in if n > 0 then n else 1)

            cout ConsoleColor.Yellow $"{tabs}{cuts}\n"
            error $"error at {lexbuf.StartPos.Column}-{lexbuf.EndPos.Column}: {msg}"

        try
            let line = Parser.line Lexer.tokenize lexbuf

            hout "absyn" $"{line}"
            hout "pretty" $"{line}"

            match line with
            | Cmd "help" -> out $"{Config.help_text}"

            | Cmd("quit" | "exit") ->
                out $"{Config.exit_text}"
                exit 0

            | Cmd s -> error $"unknown command: {s}"

            | Expr e1 ->
                // reduce the polynomial
                let rdc = reduce e1
                redux $"{rdc}"

                // normalize the polynomial
                let nor = normalize rdc
                norm $"{nor}"

                // compute the degree of the polynomial
                let dgr = normalized_polynomial_degree nor
                deg $"{dgr}"


            | Equ(lhs, rhs) ->
                // reduce the two sides of the equation separately
                let (Polynomial rdc1) = reduce lhs
                let rdc2 = reduce rhs
                redux $"{Polynomial rdc1} = {rdc2}"

                // move the right-hand side to the left-hand side and normalize
                let (Polynomial negRhs) = polynomial_negate rdc2
                let poly = normalize (Polynomial(rdc1 @ negRhs))
                norm $"{poly} = 0"

                // compute the degree of the equation
                let dgr = normalized_polynomial_degree poly
                deg $"{dgr}"

                if dgr = 0 then
                    ident $"{solve0 poly}"
                else if dgr = 1 then
                    sol $"x = {solve1 poly}"
                else if dgr = 2 then
                    match solve2 poly with
                    | None -> sol "No solutions in the Real set"
                    | Some(f, None) -> sol $"x = %.15f{f}"
                    | Some(f, Some o) -> sol $"x1 = %.15f{f}, x2 = %.15f{o}"
                else
                    error "Equations of degrees higher than 2 are not supported"

        with
        | LexYacc.ParseErrorContextException ctx ->
            let ctx = ctx :?> Parsing.ParseErrorContext<Parser.token>

            localized_error (
                sprintf
                    "syntax error%s"
                    (match ctx.CurrentToken with
                     | Some t -> $" at token <{t}>"
                     | None -> "")
            )

        | Lexer.LexerError msg -> localized_error msg

[<EntryPoint>]
let main _ =
    let code =
        try
            interpreter_loop ()
            0
        with e ->
            error $"{e}"
            1

    code

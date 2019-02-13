(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Main.fs: console e codice main
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Main

open Microsoft.FSharp.Text.Lexing
open Absyn
open System
open Prelude
open Microsoft.FSharp.Text
open Impl


// funzioni di logging e printing
//

let hout hd fmt =
    if not <| String.IsNullOrWhiteSpace hd then
        printf "[%s]%s" hd (new String (' ', max 1 (Config.prefix_max_len - String.length hd)))
        stdout.Flush ()
    printfn fmt

let chout col hd fmt =
    let c = Console.ForegroundColor
    Console.ForegroundColor <- col
    Printf.kprintf (fun s -> hout hd "%s" s; Console.ForegroundColor <- c) fmt

let out fmt = hout "" fmt
let cout col fmt = chout col "" fmt

let norm fmt = chout ConsoleColor.Yellow "norm" fmt
let redux fmt = chout ConsoleColor.Magenta "redux" fmt
let sol fmt = chout ConsoleColor.Green "sol" fmt
let ident fmt = chout ConsoleColor.Green "identity" fmt    
let error fmt = chout ConsoleColor.Red "error" fmt   

let deg fmt = chout ConsoleColor.Blue "degree" fmt

// interprete dei comandi e delle espressioni
//

let interpreter_loop () =
    while true do
        printf "\n%s" Config.prompt_prefix          // stampa il prompt
        stdout.Flush ()                             // per sicurezza flusha lo stdout per vedere la stampa del prompt senza end-of-line
        let input = Console.ReadLine ()             // leggi l'input scritto dall'utente
        let lexbuf = LexBuffer<_>.FromString input  // crea un lexbuffer sulla stringa di input

        // funzione locale per il pretty-printing degli errori
        let localized_error msg =
            let tabs = new string (' ', Config.prompt_prefix.Length + lexbuf.StartPos.Column)
            let cuts = new string ('^', let n = lexbuf.EndPos.Column - lexbuf.StartPos.Column in if n > 0 then n else 1)
            cout ConsoleColor.Yellow "%s%s\n" tabs cuts
            error "error at %d-%d: %s" lexbuf.StartPos.Column lexbuf.EndPos.Column msg 
        
        // blocco con trapping delle eccezioni
        try
            let line = Parser.line Lexer.tokenize lexbuf    // invoca il parser sul lexbuffer usando il lexer come tokenizzatore
            #if DEBUG
            hout "absyn" "%+A" line
            hout "pretty" "%O" line
            #endif

            // interpreta la linea in base al valore di tipo line prodotto dal parsing
            match line with
            | Cmd "help" ->
                out "%s" Config.help_text

            | Cmd ("quit" | "exit") ->
                out "%s" Config.exit_text
                exit 0

            // TODO: se volete supportare altri comandi, fatelo qui (opzionale)
            
            | Cmd s -> error "unknown command: %s" s    // i comandi non conosciuti cadono in questo caso

            | Expr e1 ->                                                //in caso di espressione printa il polinomio ridotto, normalizzato ed il relativo grado
                let rdc = reduce e1
                let nor = normalize rdc
                let dgr = normalized_polynomial_degree nor

                redux "%O" rdc
                norm "%O" nor
                deg "%O" dgr

            | Equ (e1, e2) ->                                           //in caso di equazione riduce e printa entrambe le espressioni in corrispondenza dell'uguale,
                let rdc1 = reduce e1                                    //ottiene il polinomio normalizzato dopo aver unito le due parti e lo printa posto uguale a 0,
                let rdc2 = reduce e2                                    //printa il grado del polinomio risultante
                let nor = normalize(unisci rdc1  rdc2)                  //infine trova l'identità, in caso di grado = 0, o le radici e mostra il risultato a schermo
                let dgr = normalized_polynomial_degree nor

                redux "%O = %O" rdc1 rdc2
                norm "%O = 0" nor
                deg "%O" dgr

                if dgr = 0 then ident "%O" (solve0 nor)
                else if dgr = 1 then sol "x = %O" (solve1 nor)
                else if dgr = 2 then 
                    match solve2 nor with
                    | None -> sol "x = {}"
                    | Some(f, None) ->
                        sol "x = %.15f" f
                    | Some(f, Some o) ->
                        sol "x1 = %.15f vel x2 = %.15f" f o
                else raise (NotImplementedException (sprintf "not implemented: %O" line))
                   
        // gestione delle eccezioni
        with LexYacc.ParseErrorContextException ctx ->
                let ctx = ctx :?> Parsing.ParseErrorContext<Parser.token>
                localized_error (sprintf "syntax error%s" (match ctx.CurrentToken with Some t -> sprintf " at token <%O>" t | None -> ""))

           | Lexer.LexerError msg -> localized_error msg 

           | :? NotImplementedException as e -> error "%O" e
        
           | e -> localized_error e.Message


// funzione main: il programma comincia da qui
//

[<EntryPoint>]
let main _ = 
    let code =
        try
            interpreter_loop ()                 // chiama l'interprete
            0                                   // ritorna il codice di errore 0 (nessun errore) al sistema operativo se tutto è andato liscio
        with e -> error "fatal error: %O" e; 1  // in caso di eccezione esce con codice di errore 1
    #if DEBUG
    Console.ReadKey () |> ignore                // aspetta la pressione di un tasto prima di chiudere la finestra del terminare 
    #endif
    code



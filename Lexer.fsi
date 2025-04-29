
module SharpSolver.Lexer
open System
open FSharp.Text.Lexing
open SharpSolver.Absyn
open SharpSolver.Parser
open SharpSolver.Prelude
open SharpSolver.Prelude.LexYacc/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token

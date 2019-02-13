
(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Impl.fsi: implementazioni degli studenti
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Impl

open Absyn
open Prelude
open System

let rationalize (x : float) : rational =
    let s = string x
    let a = s.Split [|'.'|]
    if a.Length = 1 then rational (Int32.Parse a.[0], 1)
    else
        let num = a.[0] + a.[1]
        let rec den x =
            if x = 0 then 1
            else 10 * (den (x / 10))
        rational (Int32.Parse num, den (int a.[1]))

let monomial_degree (m : monomial) : int =
    match m with
    | Monomial (coeff, deg) -> deg

let monomial_negate (m : monomial) : monomial =
    match m with
    | Monomial (coeff, deg) -> Monomial (-coeff, deg)
    
let polynomial_degree (p : polynomial) : int =
        match p with
        | Polynomial p -> let rec monList p n =
                               match p with
                               | x::xs -> if n < monomial_degree x then monList xs (monomial_degree x)
                                            else monList xs n
                               | [] -> n
                           in monList p 0
     

let polynomial_negate (p : polynomial) : polynomial =
    match p with
    | Polynomial p -> Polynomial (let rec monList p =
                                        match p with
                                        | x::xs -> (monomial_negate x) :: monList xs
                                        | [] -> []
                                    in monList p)

let normalized_polynomial_degree (np : normalized_polynomial) : int =
    match np with
    | NormalizedPolynomial np -> np.Length - 1

                                                                          ////////////////////////////////////
                                                                         ////// FUNZIONI PER NORMALIZE //////
                                                                        ////////////////////////////////////

let monomial_coeff (m : monomial) : rational =
        match m with
        | Monomial (coeff, deg) -> coeff

let rec sommaSimili l =                                                //somma tutti i monomi simili; necessita di una lista già ordinata
    let rec aux l a = match l with
                        | [] -> []
                        | [x] -> [Monomial (a+(monomial_coeff x), monomial_degree x)]
                        | x::y::xs when (monomial_degree x)<>(monomial_degree y) -> Monomial (a+(monomial_coeff x), monomial_degree x)::(aux (y::xs) 0Q)
                        | x::y::xs -> (aux (y::xs) (a+(monomial_coeff x)))
    in aux l 0Q

let rec ordina l =                                                      //ordina gli elementi della lista per grado
    List.sortBy (fun elem -> monomial_degree elem) l

let pos l =                                                             //inserisce entry nulle per rispettare l'ordine dei gradi nel polinomio normalizzato
    let rec aux lst i =
        match lst with
        | [] -> []
        | x::xs -> if monomial_degree x > i then (Monomial(0Q,i))::(aux (x::xs) (i+1))
                   else x::(aux xs (i+1))
    in aux l 0

let rec rimuoviNull l =                                                 //rimuove tutti i monomi più significativi con coefficiente pari a 0
    match List.rev l with
    | [] -> []
    | [x] -> [x]
    | x::xs when x = 0Q -> rimuoviNull xs
    | x::xs -> x::xs

let rec coeffPoly l =                                                   //ritorna una lista contenente i coefficienti dei monomi
    match l with
    | [] -> []
    | x::xs -> monomial_coeff x :: coeffPoly xs

let normalize (p : polynomial) : normalized_polynomial =                //normalizza il polinomio richiamando le funzioni precedentemente definite:
    match p with                                                        //ordina il polinomio, somma i monomi simili, inserisce entry nulle in caso di necessità ed effettua le dovute conversioni
    | Polynomial p -> NormalizedPolynomial(List.toArray (rimuoviNull(coeffPoly (pos (sommaSimili (ordina p))))))

let derivataSemplice m =                                                //deriva il monomio dato assumendo che contenga solo numeri interi
    if monomial_degree m = 0 then Monomial(0Q,0)
    else Monomial (rational((monomial_degree m) * ((monomial_coeff m).N)), (monomial_degree m)-1)

let derivataFratta m =                                                  //deriva il monomio dato assumendo che contenga frazioni
    let num = (monomial_coeff m).N
    let den = (monomial_coeff m).D
    let deg = monomial_degree m
    Monomial(rational(deg * num * den, pown den 2), deg-1)

let derive (p : polynomial) : polynomial =                              //deriva il polinomio
    match p with
    | Polynomial p -> Polynomial(List.rev(sommaSimili(ordina(let rec aux p =
                                                                  match p with 
                                                                  | [] -> []
                                                                  | x::xs -> if (monomial_coeff x).D = 1 then (derivataSemplice x) :: (aux xs)
                                                                             else (derivataFratta x) :: (aux xs)
                                                              in aux p))))

let reduce (e : expr) : polynomial =                                    //riceve un'espressione e ritorna un polinomio (derivato, in caso di necessità)
    let rec aux ex =
        match ex with
        | Poly ex -> ex
        | Derive ex -> derive(aux ex)
    in aux e

let unisci p1 p2 =                                                      //da due polinomi ricava un polinomio unico negando ogni monomio del secondo
    match p1,p2 with
    | Polynomial p1, Polynomial p2 -> Polynomial(let rec aux p1 p2 =
                                                     match p2 with
                                                     | []       -> []
                                                     | [x]      -> monomial_negate x::p1
                                                     | x::xs -> aux (monomial_negate x::p1) xs
                                                 in aux p1 p2)

let solve0 (np : normalized_polynomial) : bool =
    match np with
    | NormalizedPolynomial np -> np.[0] = 0Q

let solve1 (np : normalized_polynomial) : rational =
    match np with
    | NormalizedPolynomial np -> -np.[0] / np.[1]

let solve2 (np : normalized_polynomial) : (float * float option) option =
    match np with
    | NormalizedPolynomial np -> let a = float np.[2]
                                 let b = float np.[1]
                                 let c = float np.[0]
                                 let delta = b ** 2.0 - 4.0 * a * c
                                 if delta < 0.0 then None
                                 else if delta = 0.0 then 
                                    let x1 = -b / (2.0 * a)
                                    Some(x1, None)
                                 else
                                    let x1 = (-b + sqrt(delta)) / (2.0 * a)
                                    let x2 = (-b - sqrt(delta)) / (2.0 * a)
                                    Some(x1, Some x2)
                               
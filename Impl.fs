module SharpSolver.Impl

open Absyn
open Prelude
open System

/// Converts a floating point number into a rational number, composed as a numerator and denominator pair
let rationalize (x: float) : rational =
    // convert floating point number to string to avoid rounding errors
    let strNums = (string x).Split [| '.' |]
    let whole = strNums[0]
    let decimal = strNums[1]
    let denDigits = decimal.Length

    // express the real number as a fraction having a power of 10 as denominator
    let denominator = pown 10 denDigits
    let numerator = Int32.Parse(whole + decimal)
    rational (numerator, denominator)

/// Extracts a monomial's degree
let monomial_degree (Monomial(_, deg)) : int = deg

/// Negates a monomial's coefficient
let monomial_negate (Monomial(coeff, deg)) : monomial = Monomial(-coeff, deg)

/// Extracts a polynomial's degree, as in the maximum degree of its constituents
let polynomial_degree (Polynomial p) : int =
    p |> List.map monomial_degree |> List.max

/// Negates a polynomial's coefficients, as in the negation of all its constituents' coefficients
let polynomial_negate (Polynomial p) : polynomial =
    Polynomial <| List.map monomial_negate p

/// Extracts a normalized polynomial's degree
let normalized_polynomial_degree (NormalizedPolynomial np) : int = np.Length - 1

/// Extracts a monomial's coefficient
let monomial_coeff (Monomial(coeff, _)) : rational = coeff

/// Normalize a polynomial by adding up monomials of equal degree and sorting non-ascendingly the resulting polynomial
let normalize (Polynomial p) : normalized_polynomial =
    let rec fillMissingTerms (p: monomial list) : monomial list =
        match p with
        | [] -> []
        | [ x ] when monomial_degree x = 0 -> [ x ]
        | [ x ] -> x :: fillMissingTerms [ Monomial(0Q, (monomial_degree x) - 1) ]
        | x :: (y :: _ as rest) ->
            let xDeg, yDeg = monomial_degree x, monomial_degree y

            if xDeg - yDeg > 1 then
                x :: fillMissingTerms (Monomial(0Q, xDeg - 1) :: rest)
            else
                x :: fillMissingTerms rest

    p
    |> List.groupBy monomial_degree
    |> List.map (fun (deg, monos) -> Monomial(List.sumBy monomial_coeff monos, deg))
    |> List.sortByDescending monomial_degree
    |> fillMissingTerms
    |> List.map monomial_coeff
    |> List.toArray
    |> NormalizedPolynomial

/// Computes the derivative of the given polynomial. The only input allowed by the syntax is in the form of simple
/// sums of monomials
let derive (Polynomial p) : polynomial =
    let derivative (Monomial(coeff, deg)) =
        if deg = 0 then
            Monomial(0Q, 0)
        else
            Monomial(coeff * rational (deg, 1), deg - 1)

    p |> List.map derivative |> Polynomial

/// Produces a polynomial from the given expression
let rec reduce (e: expr) : polynomial =
    match e with
    | Poly p -> p
    | Derive p -> derive (reduce p)


/// Solves an equation of degree zero
let solve0 (NormalizedPolynomial np) : bool = np[0] = 0Q

/// Solves an equation of degree one
let solve1 (NormalizedPolynomial np) : rational = -np[1] / np[0]

/// Solves an equation of degree two
let solve2 (NormalizedPolynomial np) : (float * float option) option =
    let a = float np[0]
    let b = float np[1]
    let c = float np[2]
    let delta = b ** 2.0 - 4.0 * a * c

    if delta < 0.0 then
        None
    else if delta = 0.0 then
        Some(-b / (2.0 * a), None)
    else
        let x1 = (-b + sqrt delta) / (2.0 * a)
        let x2 = (-b - sqrt delta) / (2.0 * a)
        Some(x1, Some x2)

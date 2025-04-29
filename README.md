# SharpSolver

CLI program which parses and manipulates simple polynomials, assigned for the Introduction to Programming course at the
University of Venice. A project skeleton was given as starting point for the implementation, which the students were
tasked with filling in the empty function definitions found in Impl.fs. The main function's definition within Main.fs
was also left blank.

The scope of the project was reduced since it was the very first programming project assigned during the first year of
the course. Therefore, only the variable <em>x</em> is supported within the polynomials.

## Supported features

- Polynomial simplification
- Polynomial normalization
- First derivative computation of polynomials
- Equations solving up to the second degree

## Grammar definition

The parser and lexer are generated through [FsLexYacc](https://github.com/fsprojects/FsLexYacc), with grammars
based on the following:

```EBNF
L :=       \\ line 
  | #s     \\ command
  | E      \\ expression
  | E = E  \\ equation
  
E :=       \\ expression
  | P      \\ polynomial
  | D[E]   \\ derivative
  
P :=       \\ polynomial
  | M      \\ monomial
  | P + M  \\ sum of monomials
  
M :=       \\ monomial
  | c      \\ constant
  | cx^n   \\ monomial with coefficient c, and degree n natural
  
c :=
  | n      \\ natural number 
  | n/n    \\ rational number
  | r      \\ real number
```

## Output

- [absyn]: Abstract Syntax Tree of the input
- [pretty]: Prettier version of the Abstract Syntax Tree
- [redux]: Output of the polynomial reduction, differs from [pretty] only when derivatives need to be computed
- [norm]: Normalized input, as in the polynomial resulting from the reduction is printed with the monomials sorted non-ascendingly by degree. It also includes missing terms between the highest degree in the resulting polynomial and zero
- [degree]: Degree of the polynomial after normalization
- [sol]: Solution of the equation, only shown if the input is an equation
- [ident]: Solution of identity, only shown if the input is an equation without variables, e.g., 1 = 2

## Example usage

```
>> x^2 + 1 = x + 2
[absyn]   x^2 + 1 = x + 2
[pretty]  x^2 + 1 = x + 2
[redux]   x^2 + 1 = x + 2
[norm]    x^2 - x - 1 = 0
[degree]  2
[sol]     x1 = 1.618033988749895, x2 = -0.618033988749895

>> D[5 + 2x^3 - 5/6x]
[absyn]   D[5 + 2x^3 - 5/6x]
[pretty]  D[5 + 2x^3 - 5/6x]
[redux]   0 + 6x^2 - 5/6
[norm]    6x^2 + 0 - 5/6
[degree]  2
```

# Simplifying Arithmetic Expressions with Prolog

This is a simple Prolog utility witch allows you to simplify arithmetic expressions. It supports the following operations: addition, subtraction, multiplication, division, exponentiation and parentheses. You can also use variables in your expressions.

## Usage

To simplify an expression, call the `simp/2` predicate with the expression as the first argument and a variable as the second argument. The simplified expression will be unified with the variable.

If the expression is invalid or some error like division by zero occurs, the predicate will fail.

For debugging purposes, you can also use the `simpDebug/2` predicate, which will print the steps of the simplification process.

If the result is too long, prolog wont display the whole thing by default. You can change that by running
`set_prolog_flag(answer_write_options,[max_depth(0)]).` in your interpreter. Or by writing the result explicitly with the `write/1` predicate.

## Examples

$$11+4\cdot1\cdot(2x+y)\cdot((2\frac{4}{3}+x)z+y)\cdot3 + 10= 24xz(x+\frac{8}{3})+12yz(x+\frac{8}{3})+24xy+12y^2+21$$

```prolog
?- simp(11+4*1*(2*x+y)*((2*4/3+x)*z+y)*3 + 10, X).
X = 24*(x*z*(x+8/3))+12*(y*z*(x+8/3))+24*(x*y)+12*y^2+21
```

```prolog
?- simp((a+b+c)^2,X).
X = a^2+b^2+c^2+2*(b*c)+2*(b*a)+2*(c*a).

?- simp((a+b+c)*(b+a+c),X).
X = 2*(a*b)+a^2+c^2+2*(a*c)+b^2+2*(c*b).

?- simp((a+b)*(a-b),X).
X = a^2-b^2.

?- simp((a*x-((x+a)^2 -x^2-a^2-a*x)),X).
X = 0.
```

It can even do some basic fraction simplification:

```prolog
?- simp((3/2 - (6*x+9)/(4*x+6)) * (a+b)^4,X).
X = 0.
```

Factor out stuff and collect like terms:

```prolog
?- simp((2*x+4)*(a+2)*2 + (a+1+y/y)*(x+2),X).
X = 5*((x+2)*(a+2)).

?- simp((a^4+2)^(3/2) / (1/(a^4+2)^(1/2)),X).
X = (a^4+2)^2.

?- simp(((a+2)*b*(2+a))^2,X).
X = (a+2)^4*b^2.
```

## Limitations

It is not perfect and may not simplify some expressions as much as you would like. It also does not support trigonometric functions, logarithms, etc. Those would not be difficult to add in though. It also handles only very basic fraction division of type

$$\frac{kax+kb}{ax+b}=k.$$
Furthermore, it doesn't support irrational numbers, so it will not simplify expressions like $\sqrt{2}+\sqrt{2}$ or $(2a)^{3/2}$.

## Installation

Simply download the `simplify.pl` file and load it into your Prolog interpreter. If you have [SWI-prolog](https://www.swi-prolog.org/), you can start the environment by running `swipl` in your terminal and then load the file with `[simplify].`.

## How does it work?

The simplification is done by recursively applying a set of rules to the expression until it can no longer be simplified. The rules are based on the properties of arithmetic operations and the distributive property.

Expressions are represented as Prolog terms of different complexity levels:

- fractions: `frac(X,Y)`, where $X\in\mathbb{Z}$ and $Y\in\mathbb{N}$
- `typeX`: $x^n$, where $X$ is an atom and $n$ is a fraction
- `typeB`: $aX+b$, where $X$ is `typeX` and $a,b$ are fractions
- `typeBB`: $X+b$, where $X$ is `typeX` and $b$ is a fraction
- `typeC`: $a\cdot B_1^{n_1}\cdot B_2^{n_2}\cdots B_k^{n_k}$, where $B_i$ are of `typeBB` and $a$ is a fraction
- `typeD`: $C_1+C_2+\cdots+C_k + a$, where $C_i$ are of `typeC` and $a$ is a fraction

Note that when you add or multiply two expressions of `typeD`, you get either `typeD` or something less complex, so there is no need for more types.

The simplification process first converts all variables to `typeB` and then applies simplification rules. After it has simplified the expression in this internal representation, it converts it into a more human readable form. You can see the logic of the program by running `simpDebug/2`. For example:

```prolog
?- simpDebug(a+a,X).
1-> a+a  =  a  +  a
1=> B+B=C: frac(1,1)*a^frac(1,1)+frac(0,1)  +  frac(1,1)*a^frac(1,1)+frac(0,1)  =  (a^frac(1,1)+frac(0,1))^frac(1,1)*frac(2,1)
(a^frac(1,1)+frac(0,1))^frac(1,1)*frac(2,1)
============================================================
1-> (a^frac(1,1)+frac(0,1))^frac(1,1)*frac(2,1)  =  (a^frac(1,1)+frac(0,1))^frac(1,1)  *  frac(2,1)
2--> (a^frac(1,1)+frac(0,1))^frac(1,1)  =  a^frac(1,1)+frac(0,1)  ^  frac(1,1)
3---> a^frac(1,1)+frac(0,1)  =  a^frac(1,1)  +  frac(0,1)
4----> a^frac(1,1)  =  a  ^  frac(1,1)
4====> _^_=_: a  ^  1  =  a
3===> _+_=_: a  +  0  =  a
2==> _^_=_: a  ^  1  =  a
1=> _*_=_: a  *  2  =  2*a
X = 2*a. = a.
```

The stuff above the line is simplifying the expression, while the stuff below the line is converting the simplified expression into human readable form. The indentation shows the depth of the recursion.

# Simplifying Arithmetic Expressions with Prolog

This is a Prolog utility witch allows you to simplify mathematical expressions containing

- operators: +, -, \*, /, ^ (power)
- variables: single and multi-letter variables (e.g. `x`, `y`, `pi`)
- unary functions (e.g. `sin`, `cos`, `log`, `sqrt`)

## Installation

1. Make sure you have a Prolog interpreter installed. For example [SWI-prolog](https://www.swi-prolog.org/).
2. Download the `simplify.pl` and `inverse.pl` files.
3. Start the Prolog interpreter and load the `simplify.pl` file. If you have SWI-prolog, you can start it by running `swipl` in your terminal and then load the file with `[simplify].`.

## Usage

To simplify an expression, call the `simp/2` predicate with the expression as the first argument and a variable as the second argument. The simplified expression will be unified with the variable. If the expression is invalid or some error like division by zero occurs, the predicate will fail.

For debugging purposes, you can also use the `simpDebug/2` predicate, which will print the steps of the simplification process.

If the result is too long, prolog wont display the whole thing by default. You can change that by running
`set_prolog_flag(answer_write_options,[max_depth(0)]).` in your interpreter. Or just write the result explicitly with the `write/1` predicate.

## Built in functions and constants

All unary functions will be accepted, but only some have built-in simplification rules.

- `sin, cos, tan` - trigonometric functions
- `asin/arcsin`, `acos/arccos`, `atan/arctan` - inverse trig. functions
- `log` - natural logarithm
- `exp` - exponential function
- `sqrt` - square root

Constants are variables which get special treatment in some cases. The following constants are recognized:

- `pi` - the mathematical constant $\pi$
- `e` - the base of the natural logarithm $e$

The most basic simplification for a function $f$ is $f(f^{-1}(x))=x$. This is why there is a separate file for defining inverse functions. Too instruct the program to treat functions `func1` and `func2` as inverses of each other, simply add the following line to the `inverse.pl` file:

```prolog
inverse(func1,func2).
```

It isn't difficult to add more complex rules, but it requires some understanding of the source code. For more details see the [How does it work?](#how-does-it-work) section.

## Examples

### Polynomial simplification

$$11+4\cdot1\cdot(2x+y)\cdot\Bigl((2\frac{4}{3}+x)z+y\Bigr)\cdot3 + 10= 24xz\Bigl(x+\frac{8}{3}\Bigr)+12yz\Bigl(x+\frac{8}{3}\Bigr)+24xy+12y^2+21$$

```prolog
?- simp(11+4*1*(2*x+y)*((2*4/3+x)*z+y)*3 + 10, X).
X = 24*(x*z*(x+8/3))+12*(y*z*(x+8/3))+24*(x*y)+12*y^2+21
```

$$(2x+4)(a+2)\cdot2+\Bigl(a+1+\frac{y}{y}\Bigr)(2+x)=5(x+2)(a+2)$$

```prolog
?- simp((2*x+4)*(a+2)*2 + (a+1+y/y)*(2+x),X).
X = 5*((x+2)*(a+2)).
```

$$ax-((x+a)^2-x^2-a^2-ax)=0$$

```prolog
?- simp((a*x-((x+a)^2-x^2-a^2-a*x)),X).
X = 0.
```

It can also be used to multiply out parentheses:

```prolog
?- simp((a+b+c)*(b+a+c),X).
X = 2*(a*b)+a^2+c^2+2*(a*c)+b^2+2*(c*b).

?- simp((a+b+c)^3,X).
X = a^3+b^3+3*(b^2*c)+c^3+3*(b*c^2)+3*(c*a^2)+3*(b*a^2)+6*(b*a*c)+3*(c^2*a)+3*(b^2*a)
```

### Fractional simplification

$$\Bigl(\frac{3}{2}-\frac{6x+9}{4x+6}\Bigr)(a+b)^4=0$$

```prolog
?- simp((3/2 - (6*x+9)/(4*x+6)) * (a+b)^4,X).
X = 0.
```

$$\frac{(a^4+2)^{3/2}}{\frac{1}{(a^4+2)^{1/2}}}=(a^4+2)^{2}$$

```prolog
?- simp((a^4+2)^(3/2) / (1/(a^4+2)^(1/2)),X).
X = (a^4+2)^2.
```

### Trigonometric functions

The variable `pi` is recognized as the mathematical constant $\pi$. The program knows the following values:

- $\sin(k\pi)=0$
- $\sin(\pi/2+2k\pi)=1$
- $\sin(-\pi/2+2k\pi)=-1$
- $\arcsin(0)=0$
- cosines and tangents are similar

$$\sin(2\pi)+\cos\Bigl(\frac{1}{2}\cdot\frac{\tan(\arctan(2x+9))}{x+9/2}-1\Bigr)=1$$

```prolog
?- simp(sin(2*pi)+cos((1/2)*tan(atan(2*x+9))/(x+9/2)-1),X).
X = 1.
```

### Logarithms and exponentials

The variable `e` is recognized as the base of the natural logarithm and so `e^x` is equivalent to `exp(x)` in some basic cases. It is however recommended to use `exp` in more complex expressions, as it might simplify further.

- $\log(a\cdot b)=\log(a)+\log(b)$
- $\log(a / b)=\log(a)-\log(b)$
- $\log(a^n)=n\log(a)$
- $\exp(x)^y=\exp(xy)$
- $\exp(x)\exp(y)=\exp(x+y)$
- $\exp(a\log(x)+y)=x^a+\exp(y)$

$$\log\bigl(2(\sqrt{x})^3\bigr)=\frac{3}{2}\log(x)+\log(2)$$

```prolog
?- simp(log(2*sqrt(x)^3),X).
X = 3/2*log(x)+log(2).
```

$$\log\biggl(\frac{a\exp((x+1)^2)}{b\sqrt e}\biggr)=(x+1)^2-\log(b)+\log(a)-\frac{1}{2}$$

```prolog
?- simp(log((a*exp((x+1)^2))/(b*sqrt(e))),X).
X = (x+1)^2-log(b)+log(a)-1/2.
```

$$\log\Bigl((x+1)^2\exp\bigl(2\log(x+1)+1\bigr)\Bigr)=4\log(x+1)+1$$

```prolog
?- simp(log((x+1)^2*exp(2*log(x+1)+1)),X).
X = 4*log(x+1)+1.
```

The next example shows why `exp` is preferred over `e` in more complex cases.
$$\exp\bigr(4\log(x+1)+1\bigl)=e\cdot(x+1)^4$$

```prolog
?- simp(e^(4*log(x+1)+1),X).
X = e^(4*log(x+1)+1).

?- simp(exp(4*log(x+1)+1),X).
X = (x+1)^4*e.
```

$$\exp(a+log(a)+b+a)=a\cdot\exp(2a+b)$$

```prolog
?- simp(exp(a+log(a)+b+a),X).
X = exp(2*a+b)*a.
```

### Square roots

The square root function is recognized as `sqrt`. Note that `^(1/2)` is equivalent to `sqrt` in some basic cases, but should generally be avoided.
$$\sqrt{(a+2)\cdot4\cdot\sqrt{(b^2-1)^4}(x-1)(1+a+1)}=2(b^2-1)\sqrt{x-1}(a+2)$$

```prolog
?- simp(sqrt((a+2)*4*sqrt((b^2-1)^4)*(x-1)*(1+a+1)),X).
X = 2*((b^2-1)*sqrt(x-1)*(a+2)).
```

$$(\sqrt2+\sqrt2)^2=8$$

```prolog
?- simp((sqrt(2)+sqrt(2))^2,X).
X = 8.
```

$$\bigl(x(\sqrt2+\sqrt2)\bigr)^2=8x^2$$

```prolog
?- simp((x*(sqrt(2)+sqrt(2)))^2,X).
X = 8*x^2.
```

$$(x\sqrt2+\sqrt2)^2=2x^2+4x+2$$

```prolog
?- simp((x*sqrt(2)+sqrt(2))^2,X).
X = 2*x^2+4*x+2.
```

## Limitations

The program is not perfect and may not simplify some expressions as much as you would like. This is the case especially for polynomial division. The most complex polynomial division it can do is

$$\frac{kax^n+kb}{ax^n+b}=k.$$

## How does it work?

The simplification is done by recursively applying a set of rules to the expression until it can no longer be simplified. The rules are based on the properties of arithmetic operations and the distributive property.

Expressions are represented as Prolog terms of different complexity:

- fractions: `frac(X,Y)`, where $X\in\mathbb{Z}$ and $Y\in\mathbb{N}$
- `typeX`: $X^n$, where $X$ is a variable or a function like `sin(...)` and $n$ is a `frac`
- `typeBB`: $X+b$, where $X$ is `typeX` and $b$ is a `frac`
- `typeB`: $aX+b$, where $X$ is `typeX` and $a,b$ are `frac`
- `typeCC`: $B_1^{n_1}\cdot B_2^{n_2}\cdots B_k^{n_k}$, where $B_i$ are of `typeBB` and $n_i$ are `frac`
- `typeC`: $a\cdot C$, where $C$ is `typeCC` and $a$ is `frac`
- `typeDD`: $C_1+C_2+\cdots+C_k$, where $C_i$ are of `typeC`
- `typeD`: $a\cdot D$, where $D$ is `typeDD` and $a$ is `frac`

Note that when you add or multiply two expressions of `typeD`, you get either `typeD` or something less complex, so there is no need for more types.

The simplification process first converts all variables to `typeB` and then recursively applies simplification rules using the tree-like structure of the expression. After it has simplified the expression in this internal representation, it converts it into a more human readable form. You can see the logic of the program by running `simpDebug/2`. Let's look at an example to explain what is going on.

The stuff above the line is simplifying the expression, while the stuff below the line is converting the simplified expression into human readable form. Lines starting with `%` are comments and are not part of the output.

```prolog
?- simpDebug(sin(a-a)+sqrt(4*x),X).
1-> sin(a-a)+sqrt(4*x)  =  sin(a-a)  +  sqrt(4*x)
% the root of this expression contains '+', let's first simplify the left operand then the right one and finally add them together
2--> sin(a-a)  =  sin(   a-a   )
% sin is a functions, simplify the argument, depth of recursion is 2 now
3---> a-a  =  a  -  a
% simplify the left operand then the right one and finally subtract them
% 'a' is a variable, so we convert it to typeB first
3===> B-B=F: frac(1,1)*a^frac(1,1)+frac(0,1)  -  frac(1,1)*a^frac(1,1)+frac(0,1)  =  frac(0,1)
% B-B=F means, that we simplified typeB-typeB and got a fraction
% the double arrow '=>' means that we are returning from recursion
2==> sin(F)=F: sin(   frac(0,1)   )  =  frac(0,1)
% we can now evaluate the sinus, sin(0)=0
2--> sqrt(4*x)  =  sqrt(   4*x   )
% start evaluating the square root
3---> 4*x  =  4  *  x
% simplify the argument first
3===> F*B=B: frac(4,1)  *  frac(1,1)*x^frac(1,1)+frac(0,1)  =  frac(4,1)*x^frac(1,1)+frac(0,1)
% after converting '4' to a fraction and 'x' to typeB, we can multiply them and get a 'typeB'
2==> sqrt(B)=B: sqrt(   frac(4,1)*x^frac(1,1)+frac(0,1)   )  =  frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)+frac(0,1)
% simplify the square root, we can see that sqrt(typeB)=typeB
1=> F+B=B: frac(0,1)  +  frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)+frac(0,1)  =  frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)+frac(0,1)
frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)+frac(0,1)
% finally add the two parts together. F from sin(0) and B from the square root give us the final result of typeB
============================================================
% now we convert the result into human readable form
1-> frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)+frac(0,1)  =  frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)  +  frac(0,1)
2--> frac(2,1)*sqrt(x^frac(1,1))^frac(1,1)  =  frac(2,1)  *  sqrt(x^frac(1,1))^frac(1,1)
3---> sqrt(x^frac(1,1))^frac(1,1)  =  sqrt(x^frac(1,1))  ^  frac(1,1)
4----> sqrt(x^frac(1,1))  =  sqrt(   x^frac(1,1)   )
5-----> x^frac(1,1)  =  x  ^  frac(1,1)
5=====> _^_=_: x  ^  1  =  x
4====> sqrt(_)=_: sqrt(   x   )  =  sqrt(x)
3===> _^_=_: sqrt(x)  ^  1  =  sqrt(x)
2==> _*_=_: 2  *  sqrt(x)  =  2*sqrt(x)
1=> _+_=_: 2*sqrt(x)  +  0  =  2*sqrt(x)
X = 2*sqrt(x).
```

### Adding new rules

Let's say that you need this program to simplify expressions containing hyperbolic functions like `sinh` or `cosh`, which are not built-in. Lets see what happens when we try to simplify such an expression:

```prolog
?- simp(2+x+a*sinh(x^2-x*x)-1,X).
X = a*sinh(0)+x+1.
```

The program simplifies the argument of `sinh` to `0`, but it doesn't know that `sinh(0)=0`, so it doesn't simplify `a*sinh(0)` to `0`. To add this rule, we need to modify the `simplify.pl` file. If you look through the file, you will see how this is done for `sin`:

```prolog
s(sin,frac(0,1),frac(0,1)) :- !.
```

`s(+function, +argument, -result)` is a predicate that defines the simplification rule for a function. Since we represent numbers as fractions, we need to convert the number `0` to `frac(0,1)`. Note the cut `!` at the end of the rule, which prevent backtracking.

To add a rule for `sinh`, we need to add the following line to the 'functions' section:

```prolog
s(sinh,frac(0,1),frac(0,1)) :- !.
```

To instruct the program to treat `sinh` and `arcsinh` as inverse functions, add the following line to the `inverse.pl` file:

```prolog
inverse(sinh,arcsinh).
```

Now the program will simplify the expressions as expected:

```prolog
?- simp(2+x+a*sinh(x^2-x*x)-1,X).
X = x+1.

? - simp(arcsinh(sinh(x)),X).
X = x.
```

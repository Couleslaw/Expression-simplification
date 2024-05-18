% load file with predefined inverse functions
:- [inverse].

:- discontiguous s/4.
:- discontiguous s/3.

even(X) :-
    integer(X),    
    0 is X mod 2.  

odd(X) :-
    integer(X),    
    1 is X mod 2.

% =========== functions ===========

function(X) :- X =.. [_Func, _Arg].

% INVERSE FUNCTION LOGIC
s(Func, X, V) :- 
    X = frac(1,1)*ZZ^frac(1,1)+frac(0,1),
    function(ZZ),
    ZZ =.. [InvFunc, Arg],
    (inverse(Func, InvFunc) ; inverse(InvFunc, Func)),
    V = Arg, !.
s(Func, X, V) :- 
    X = (ZZ^frac(1,1)+frac(0,1))^frac(1,1),
    function(ZZ),
    ZZ =.. [InvFunc, Arg],
    (inverse(Func, InvFunc) ; inverse(InvFunc, Func)),
    V = Arg, !.

s(Func, X, V) :- 
    X = frac(1,1)*ZZ^frac(1,1),
    function(ZZ),
    ZZ =.. [InvFunc, Arg],
    (inverse(Func, InvFunc) ; inverse(InvFunc, Func)),
    V = Arg, !.

s(Func, X, V) :- 
    X = (ZZ^frac(1,1)+frac(0,1))^frac(1,1)*frac(1,1),
    function(ZZ),
    ZZ =.. [InvFunc, Arg],
    (inverse(Func, InvFunc) ; inverse(InvFunc, Func)),
    V = Arg, !.

% SIN
s(sin,frac(0,1),frac(0,1)) :- !.
s(sin,frac(_,1)*pi^frac(1,1)+frac(0,1),frac(0,1)) :- !.

% ARCSIN
s(asin,frac(0,1),frac(0,1)) :- !.
s(arcsin,frac(0,1),frac(0,1)) :- !.

% COS
s(cos,frac(0,1),frac(1,1)) :- !.
s(cos,frac(X,1)*pi^frac(1,1)+frac(0,1),frac(1,1)) :- even(X), !.
s(cos,frac(X,1)*pi^frac(1,1)+frac(0,1),frac(-1,1)) :- odd(X), !.

% ARCCOS
s(acos,frac(1,1),frac(0,1)) :- !.
s(arccos,frac(1,1),frac(0,1)) :- !.

% TAN
s(tan,frac(0,1),frac(0,1)) :- !.
s(tan,frac(_,1)*pi^frac(1,1)+frac(0,1),frac(0,1)) :- !.

% ARCTAN
s(atan,frac(0,1),frac(0,1)) :- !.
s(arctan,frac(0,1),frac(0,1)) :- !.

% LOG
s(log,frac(1,1),frac(0,1)) :- !.
s(log,frac(1,1)*e^frac(1,1)+frac(0,1),frac(1,1)) :- !.
s(log,e^frac(1,1)+frac(0,1),frac(1,1)) :- !.
s(log,(e^frac(1,1)+frac(0,1))^frac(1,1),frac(1,1)) :- !.
s(log,frac(1,1)*X^Y+frac(0,1),V) :- 
    Y\=frac(1,1),
    s(log,X,V0),
    make_CC_from_func(V0,VX),
    s(*,VX,Y,V), !.
s(log,B^N,V) :- 
    N\=frac(1,1),
    typeBB(B),
    B=X^P+F,
    s(log,frac(1,1)*X^P+F,V0),
    make_CC_from_func(V0,VX),
    s(*,N,VX,V), !.
s(log,X^N,V) :- 
    N\=frac(1,1),
    s(log,X,V0),
    make_CC_from_func(V0,VX),
    s(*,N,VX,V), !.
s(log,X*Y,V) :-
    s(log,X,VX0),
    s(log,Y,VY0),
    make_CC_from_func(VX0,VX),
    make_CC_from_func(VY0,VY),
    s(+,VX,VY,V), !.

% EXP
s(exp,frac(0,1),frac(1,1)) :- !.
s(exp,X+Y,V) :- 
    s(exp,X,VX),
    s(exp,Y,VY),
    s(*,VX,VY,V), !.
s(^,EXP,F,V) :-
    EXP=X*exp(Y)^frac(1,1)+frac(0,1),
    s(^,X,F,VX),
    s(*,Y,F,VY),
    V=VX*exp(VY)^frac(1,1)+frac(0,1), !.

s(*,EXP1,EXP2,V) :- 
    EXP1=X1*exp(Y1)^frac(1,1)+frac(0,1),
    EXP2=X2*exp(Y2)^frac(1,1)+frac(0,1),
    s(*,X1,X2,VX),
    s(+,Y1,Y2,VY),
    V=VX*exp(VY)^frac(1,1)+frac(0,1), !.

% SQRT
s(sqrt,frac(X,Y),V) :-
    X > 0,
    SX is sqrt(X), is_float_int(SX),
    SY is sqrt(Y), is_float_int(SY),
    round(SX,RSX), round(SY,RSY),
    V=frac(RSX,RSY), !.
s(sqrt,X^frac(A,B),V) :-
    even(A), A1 is A//2,
    V=X^frac(A1,B), !.
s(sqrt,X*Y^frac(A,B)+frac(0,1),V) :-
    s(sqrt,X,SX),
    (even(A) ; (X\=frac(1,1), \+function(SX))),
    s(sqrt,Y^frac(A,B),SQRT),
    (
        SX=.. [sqrt,_]
        ->( 
            SQRT=.. [sqrt,_]
            -> s(*,SX,SQRT,V)
            ;  V=(SQRT+frac(0,1))^frac(1,1)*(SX^frac(1,1)+frac(0,1))^frac(1,1)
        )
        ; (
            SQRT=.. [sqrt,_]
            -> V=SX*SQRT^frac(1,1)+frac(0,1)
            ;  s(*,SX,frac(1,1)*SQRT+frac(0,1),V)
        )
    ), !.

s(sqrt,X*Y,V) :-
    s(sqrt,X,SX),
    s(sqrt,Y,SY),
    s(*,SX,SY,V), !.

s(*,Z*sqrt(X),sqrt(Y),sqrt(V)*Z) :- s(*,X,Y,V), !.
s(*,sqrt(X),sqrt(Y),sqrt(V)) :- s(*,X,Y,V), !.
s(*,SQRT1,SQRT2,V) :-
    SQRT1=(sqrt(X)^frac(1,1)+frac(0,1))^frac(1,1),
    SQRT2=(sqrt(X)^frac(1,1)+frac(0,1))^frac(1,1),
    s(^,SQRT1,frac(2,1),V), !.

s(*,SQRT1,SQRT2,V) :-
    SQRT1=(sqrt(X)^frac(1,1)+frac(0,1))^frac(1,1),
    SQRT2=(sqrt(Y)^frac(1,1)+frac(0,1))^frac(1,1),
    s(*,X,Y,Z),
    s(sqrt,Z,SQZ),
    make_CC_from_func(SQZ,V), !.

s(^,SQRT,frac(A,B),V) :-
    even(A), A1 is A//2,
    SQRT=F*sqrt(X)^N+frac(0,1),
    s(^,F,frac(A,B),VF),
    s(*,N,frac(A1,B),VN),
    s(^,X,VN,VX),
    s(*,VF,VX,V), !.

s(^,SQRT,frac(A,B),V) :-
    even(A), A1 is A//2,
    SQRT=(sqrt(X)^frac(1,1)+frac(0,1))^frac(1,1),
    s(^,X,frac(A1,B),V), !.

s(^,SQRT,frac(A,B),V) :-
    even(A), A1 is A//2,
    SQRT=(sqrt(X)^frac(1,1)+frac(0,1))^frac(1,1) * F,
    s(^,F,frac(A,B),VF),
    % (
    %     frac(X)
    %     -> s(^,X,frac(A1,B),VX)
    %     ;  VX = (X^frac(1,1)+frac(0,1))^frac(A1,B)
    % ),
    s(^,X,frac(A1,B),VX),
    s(*,VX,VF,V), !.
% if nothing else matches
s(Func,X,V) :-V=..[Func,X], !.

% ============ CLASS frac ============
frac(N) :- N=frac(X,Y), frac(X,Y).
frac(X,Y) :- integer(X), integer(Y), Y>0.
% prevedeni zlomku do zakladniho tvaru
nsd(X, Y, X) :- Y = 0, !.
nsd(X, Y, M) :- 
    Z is X mod Y,
    nsd(Y, Z, M). 

% ========== frac operations =========

s(frac, X, Y, frac(A,B)) :-
    frac(X,Y),
    nsd(X,Y,M), 
    A is X // M,
    B is Y // M, !.

s(+,frac(A,B),frac(C,D),X) :-
    E is A*D + B*C,
    F is B*D,
    s(frac, E, F, X), !.

s(-,frac(A,B),frac(C,D),X) :-
    C1 is -C,
    s(+, frac(A,B), frac(C1,D), X), !.

s(*,frac(A,B),frac(C,D),X) :-
    E is A*C,
    F is B*D,
    s(frac, E, F, X), !.

is_float_int(F) :-
    round(F,I), F=:=I.

s(^,frac(A,B),frac(C,D),V) :-
    % (A/B)^(C/D) = A^(C/D) / B^(C/D)
    E is A^(C/D), is_float_int(E), round(E,RE),
    F is B^(C/D), is_float_int(F), round(F,RF),
    s(frac,RE,RF,V), !.
s(^,F,frac(C,D),V) :- 
    % a^(-k) = 1 / a^k
    frac(F), C<0, C1 is -C,
    s(^,F,frac(C1,D),V1),
    s(/,frac(1,1),V1,V), !. 


% ============= CLASS X ==============
basic(X) :- atom(X), !.
basic(X) :- function(X), !.
typeX(X^N) :- frac(N), basic(X), !.

% ============= CLASS B ============== 
typeB(A*X+F) :- frac(A), frac(F), typeX(X).

typeBB(X+F) :- frac(F), typeX(X).

% ============= CLASS C ==============
typeCC(B^N) :- typeBB(B), frac(N), !.
typeCC(Zbytek * B^N) :- typeBB(B), frac(N), typeCC(Zbytek).
typeC(Zbytek * A) :- frac(A), typeCC(Zbytek).

make_C_from_B(B, C) :-
    typeB(B),
    B = A*X^N + F,  % = A * (X^N + F/A)
    s(/,F,A,E),
    (
        E = frac(0,1) 
        -> C = (X^frac(1,1)+E)^N * A
        ;  C = (X^N+E)^frac(1,1) * A
    ).

make_CC_from_func(Func,CC):-
    (
        function(Func)
        -> CC = frac(1,1)*Func^frac(1,1)+frac(0,1)
        ; (
            (Func=Pref*Suff, function(Pref), typeCC(Suff))
            -> s(*,(Pref^frac(1,1)+frac(0,1))^frac(1,1),Suff,CC)
            ; CC = Func
        )
    ).

make_CC_from_X(X^N, CC) :-
    typeX(X^N),
    CC = (X^frac(1,1) + frac(0,1))^N.

% containsCC(+CC1, +CC2) :- every term in CC2 is also in CC1
containsCC(B^N, B^N) :- 
    typeCC(B^N), !.

containsCC(CC * B^N, B^N) :- 
    typeCC(CC * B^N), !.

containsCC(CC * B^N, B1^M) :- 
    typeCC(CC * B^N), typeCC(B1^M),
    containsCC(CC, B1^M), !.

containsCC(CC, Zbytek * B^N) :- 
    typeCC(CC), typeCC(Zbytek * B^N),
    containsCC(CC, Zbytek),
    containsCC(CC, B^N).

similarCC(CC1, CC2) :-
    containsCC(CC1, CC2), 
    containsCC(CC2, CC1). 

% ============= CLASS D ==============
typeDD(C) :- typeC(C), !.
typeDD(Zbytek + C) :- typeC(C), typeDD(Zbytek), !.
typeD(Zbytek + A) :- frac(A), typeDD(Zbytek).

% ============= general rules ============
s(*,_,frac(0,1),frac(0,1)) :- !.
s(*,frac(0,1),_,frac(0,1)) :- !.

s(+,X,frac(0,1),X) :- !.
s(+,frac(0,1),X,X) :- !.

s(^,X,frac(1,1),X) :- !.
s(^,_,frac(0,1),frac(1,1)) :- !.
s(^,X*Y,F,V) :- 
    frac(F), 
    s(^,X,F,XF),
    s(^,Y,F,YF),
    V = XF * YF, !.
s(^,X+Y,frac(N,1),V) :-
    \+typeB(X+Y), \+typeBB(X+Y),
    N > 1, N1 is N-1,
    s(^,X+Y,frac(N1,1),V1),
    s(*,V1,X+Y,V), !.

s(^,X^N,F,Y) :-
    frac(F),
    s(*,N,F,N1),
    Y = X^N1, !.

s(-,A,B,C) :-
    s(*,B,frac(-1,1),MB),
    s(+,A,MB,C), !.

s(/,X,frac(A,B),Y) :-
    frac(A,B),
    % if A is negative --> multiply out by -1
    A1 is -A, B1 is -B,
    (
        A<0 
        -> frac(B1,A1), s(*,X,frac(B1,A1),Y)
        ;  frac(B,A),   s(*,X,frac(B,A),Y)
    ), !.

s(/,X,A/B,V) :-
    % X / (A/B) = X*B / A
    s(*,X,B,V0),
    s(/,V0,A,V), !.

s(/,X,X,frac(1,1)) :- !.

% ============= B operations =============
% >>> with frac <<<
s(+,B,F,B1) :- 
    typeB(B), frac(F),
    B = A*X+C,
    s(+,C,F,C1),
    B1 = A*X + C1, !.
s(+,F,B,B1) :- 
    typeB(B), frac(F),
    s(+,B,F,B1), !.

s(*,B,F,B1) :-
    typeB(B), frac(F),
    B = A*X+C,
    s(*,A,F,A1), s(*,C,F,C1),
    B1 = A1*X + C1, !.
s(*,F,B,B1) :-
    typeB(B), frac(F),
    s(*,B,F,B1), !.

s(^,B,F,B1) :- 
    typeB(B), frac(F),
    B = A*X^N + frac(0,1),
    s(^,A,F,A1),
    s(*,N,F,N1),
    B1 = A1*X^N1 + frac(0,1), !.
s(^,B,F,C) :-
    typeB(B), frac(F),
    make_C_from_B(B,C0),
    s(^,C0,F,C), !.    

% >>> with B <<<

s(/,B1,B2,F) :-
    typeB(B1), typeB(B2),
    B1 = A1*X + frac(0,1), 
    B2 = A2*X + frac(0,1),
    s(/,A1,A2,F), !.
s(/,B1,B2,F) :-
    typeB(B1), typeB(B2),
    B1 = A1*X+C1, 
    B2 = A2*X+C2,
    s(/,A1,A2,F), s(/,C1,C2,F), !.

s(+,B1,B2,D) :-
    typeB(B1), typeB(B2),
    B1 = A1*X1+E1, 
    B2 = A2*X2+E2,
    s(+,E1,E2,E),
    make_CC_from_X(X1, CC1),
    make_CC_from_X(X2, CC2),
    s(+,CC1*A1, CC2*A2, D0),
    s(+,D0, E, D), !.

s(*,B1,B2,B) :-
    B1=A1*X^N+frac(0,1), 
    B2=A2*X^M+frac(0,1),
    s(*,A1,A2,A),
    s(+,N,M,N1),
    (
        N1=0
        -> B = A
        ;  B = A*X^N1+frac(0,1)
    ), !.

s(*,B1,B2,C) :- 
    typeB(B1), typeB(B2),
    make_C_from_B(B1, C1),
    make_C_from_B(B2, C2),
    s(*,C1,C2,C), !.
    
% =========== CC operations ============

s(*,CC,F,CC*F) :-
    typeCC(CC), frac(F), !.
s(*,F,CC,CC*F) :-
    typeCC(CC), frac(F), !.

s(*,B^N, B^M, CC1) :-
    typeCC(B^N), typeCC(B^M),
    s(+,N,M,N1),
    s(^,B,N1,CC1), !.
    
s(*,B^N, B1^M, CC1) :-
    typeCC(B^N), typeCC(B1^M),
    CC1 = B^N * B1^M, !.

s(*,CC * B^N, B^M, CC1) :- 
    typeCC(CC * B^N), typeCC(B^M),
    s(*,B^N, B^M, CC0),
    CC1 = CC * CC0, !.

s(*,CC * B^N, B1^M, CC1) :- 
    typeCC(CC * B^N), typeCC(B1^M),
    s(*,CC, B1^M, Zbytek),
    CC1 = Zbytek * B^N, !.

s(*,CC1, CC2 * B^N, CC) :-
    typeCC(CC1), typeCC(CC2 * B^N),
    s(*,CC1,CC2,Zbytek),
    s(*,Zbytek,B^N,CC), !.

s(/,CC1, CC2, CC) :-
    typeCC(CC1), typeCC(CC2),
    similarCC(CC1, CC2),
    CC = frac(1,1), !.

% =========== C operations =============
% >>> with frac <<<
s(+,C,F,D) :-
    typeC(C), frac(F),
    D = C + F, !.
s(+,F,C,D) :-
    typeC(C), frac(F),
    s(+,C,F,D), !.

s(*,C,F,C1) :-
    typeC(C), frac(F),
    C = Zbytek * A,
    s(*,F,A,A1),
    C1 = Zbytek * A1, !.
s(*,F,C,C1) :-
    typeC(C), frac(F),
    s(*,C,F,C1), !.

% >>> with B <<<
% convert B to C

s(+,C,B,D) :- 
    typeC(C), typeB(B),
    B = A*X+F,
    make_CC_from_X(X,CC),
    s(+,C,CC*A,D0),
    s(+,D0,F,D), !.
s(+,B,C,D) :- 
    typeC(C), typeB(B),
    s(+,C,B,D), !.

s(*,C,B,C1) :- 
    typeC(C), typeB(B),
    make_C_from_B(B, BC),
    s(*,C, BC, C1), !.
s(*,B,C,C1) :- 
    typeC(C), typeB(B),
    s(*,C,B,C1), !.

% >>> with C <<<

s(+,C1,C2,C) :-
    typeC(C1), typeC(C2),
    C1 = CC1 * A1,
    C2 = CC2 * A2,
    similarCC(CC1, CC2),
    % CC1 and CC2 contain like terms, possibly in different orders
    s(+,A1,A2,A),
    s(*,CC1,A,C), !.

s(+,C1,C2,DD) :-
    typeC(C1), typeC(C2),
    DD = C1 + C2, !.

s(*,C1,C2,C) :-
    typeC(C1), typeC(C2),
    C1 = CC1 * A1, 
    C2 = CC2 * A2,
    s(*,A1,A2,A),
    s(*,CC1,CC2,CC),
    s(*,CC,A,C), !.

s(/,C,B,V) :-
    typeC(C), typeB(B),
    make_C_from_B(B,CB),
    s(/,C,CB,V), !.

s(/,C1,C2,V) :-
    typeC(C1), typeC(C2),
    C1 = CC1 * A1, 
    C2 = CC2 * A2,
    s(/,CC1,CC2,V0),
    s(/,A1,A2,A),
    s(*,V0,A,V), !.


% TODO: C/B, C/C --> C,F

% ============= DD operations ==============

% >>> add frac <<< 
s(+,DD,F,DD+F) :-
    typeDD(DD), frac(F), !.
s(+,F,DD,DD+F) :-
    typeDD(DD), frac(F), !.

% >>> add B <<<
s(+,DD,B,D) :- 
    typeDD(DD), typeB(B),
    B = A*X + E,
    make_CC_from_X(X, CC),
    s(+,DD,CC*A,DD1),
    s(+,DD1,E,D), !.
s(+,B,D,D1) :- 
    typeD(D), typeB(B),
    s(+,D,B,D1), !.

% >>> frac/B/C multiply <<<
s(*,DD,X,DD1) :- 
    typeDD(DD),
    (frac(X) ; typeB(X) ; typeC(X)),
    DD = Zbytek + C,
    s(*,Zbytek,X,Zbytek1),
    s(*,C,X,C1),
    s(+,Zbytek1,C1,DD1), !.
s(*,X,DD,DD1) :-
    typeDD(DD),
    (frac(X) ; typeB(X) ; typeC(X)),
    s(*,DD,X,DD1), !.

% >>> add C <<<
s(+,Zbytek+C1,C2,DD) :- 
    typeDD(Zbytek+C1), typeC(C2),
    C1 = CC1 * _A1, 
    C2 = CC2 * _A2,
    similarCC(CC1, CC2),
    s(+,C1,C2,C),
    (
        C=frac(0,1) 
        -> DD = Zbytek
        ; DD = Zbytek + C
    ), !.

s(+,Zbytek+C1,C2,DD) :-
    typeDD(Zbytek+C1), typeC(C2),
    s(+,Zbytek,C2,DD0),
    (  
        DD0=frac(0,1)
        -> DD = C1
        ; DD = DD0 + C1
    ), !.
s(+,C,DD,DD1) :-
    typeC(C), typeDD(DD),
    s(+,DD,C,DD1), !.

% >>> with DD <<<
s(+,DD1,DD2,DD) :-
    typeDD(DD1), typeDD(DD2),
    DD2 = Zbytek + C,   % case DD2=C is solved by DD+C
    s(+,DD1,Zbytek,DD0),
    s(+,DD0,C,DD), !.

s(*,DD1,DD2,DD) :-
    typeDD(DD1), typeDD(DD2),
    DD2 = Zbytek2 + C,   % case DD2=C is solved by DD1+C
    s(*,DD1,Zbytek2,Zbytek),
    s(*,DD1,C,Konec),
    s(+,Zbytek,Konec,DD), !.

% other --> convert to D
s(Op, DD+C, X, D) :-
    typeDD(DD+C),
    s(Op, DD+C+frac(0,1), X, D), !.
s(Op, X, DD+C, D) :-
    typeDD(DD+C),
    s(Op, X, DD+C+frac(0,1), D), !.


% ============= D operations ================
% >>> with frac <<<
s(+,D,F,D1) :- 
    typeD(D), frac(F),
    D = Zbytek + A,
    s(+,A,F,A1),
    D1 = Zbytek + A1, !.
s(+,F,D,D1) :- 
    typeD(D), frac(F),
    s(+,D,F,D1), !.

s(*,D,F,D1) :-
    typeD(D), frac(F),
    D = Zbytek + A,
    s(*,Zbytek,F,Zbytek1),
    s(*,A,F,A1),
    D1 = Zbytek1 + A1, !.
s(*,F,D,D1) :- 
    typeD(D), frac(F),
    s(*,D,F,D1), !.


% >>> with B <<<
s(+,D,B,D1) :- 
    typeD(D), typeB(B),
    D = DD + F,
    B = A*X + E,
    make_CC_from_X(X, CC),
    s(+,DD,CC*A,DD1),
    s(+, F, E, F1),
    s(+,DD1,F1,D1), !.
s(+,B,D,D1) :- 
    typeD(D), typeB(B),
    s(+,D,B,D1), !.

% convert to C
s(*,D,B,D1) :-
    typeD(D), typeB(B),
    make_C_from_B(B, C),
    s(*,D,C,D1), !.
s(*,B,D,D1) :- 
    typeD(D), typeB(B),
    s(*,D,B,D1), !.

% >>> with C <<<
s(+,D,C,D1) :-
    typeD(D), typeC(C),
    D = DD + A,
    s(+,DD,C,DD1),
    D1 = DD1 + A, !.
s(+,C,D,D1) :-
    typeD(D), typeC(C),
    s(+,D,C,D1), !.

s(*,D,C,D1) :-
    typeD(D), typeC(C),
    D = DD + A,
    s(*,DD,C,DD1),
    s(*,A,C,C1),
    D1 = DD1 + C1 + frac(0,1), !.
s(*,C,D,D1) :- 
    typeD(D), typeC(C),
    s(*,D,C,D1), !.

% >>> with D <<<

vyres_konec(Zbytek, frac(0,1), Zbytek) :- typeC(Zbytek), !.
vyres_konec(frac(0,1), A, A) :- !.
vyres_konec(Zbytek, A, Zbytek + A).

s(+,D1,D2,D) :-
    typeD(D1), typeD(D2),
    D1 = Zbytek1 + A1,
    D2 = Zbytek2 + A2, 
    s(+,Zbytek1,Zbytek2,Zbytek),
    s(+,A1,A2,A),
    vyres_konec(Zbytek, A, D), !.

s(*,D1,D2,D) :-
    typeD(D1), typeD(D2),
    D1 = Z1 + A1,
    D2 = Z2 + A2, 
    % D1*D2 = Z1*Z2 + A1*Z2 + Z1*A2 + A1*A2
    s(*,Z1,Z2,DD1),
    s(*,Z2,A1,DD2),
    s(*,Z1,A2,DD3),
    s(*,A1,A2,A),
    s(+,DD1,DD2,DD0),
    s(+,DD0,DD3,Zbytek),
    vyres_konec(Zbytek, A, D), !.



% ============== when nothing works ===============
s(+,X,Y,X+Y).
s(*,X,Y,X*Y).
s(^,X,Y,X^Y).
s(/,X,Y,X/Y) :- 
     Y \= frac(0,1).

% ================ normalize ===============
n(*,1,X,X) :- !.
n(*,X,1,X) :- !.
n(*,-A,X,-R) :- 
    n(*,A,X,R), !.
n(*,X,-A,-R) :- 
    n(*,A,X,R), !.
n(*,X,A,A*X) :- (integer(A) ; frac(A) ; A=_B/_C), !.
n(*,X,Y,X*Y).

n(+,0,X,X) :- !.
n(+,X,0,X) :- !.
n(+,X,-Y,X-Y) :- !.
n(+,X,Y,X+Y).

n(^,_,0,1) :- !.
n(^,1,_,1) :- !.
n(^,X,1,X) :- !.
n(^,X,- 1,1/X) :- !.   % THE SPACE IN '- 1' IS IMPORTANT !!!
n(^,X,-A,1/(X^A)) :- !.
n(^,X,Y,X^Y).


n(/,X,Y,X/Y).
n(frac,A,B,frac(A,B)) :- frac(A,B).

% ================ debug utils ================== 
typeof(VZ, 'F') :- frac(VZ), !.
typeof(VZ, 'X') :- typeX(VZ), !.
typeof(VZ, 'B') :- typeB(VZ), !.
typeof(VZ, 'BB') :- typeBB(VZ), !.
typeof(VZ, 'C') :- typeC(VZ), !.
typeof(VZ, 'CC') :- typeCC(VZ), !.
typeof(VZ, 'D') :- typeD(VZ), !.
typeof(VZ, 'DD') :- typeDD(VZ), !.
typeof(_, '_').

write_char(0,_) :- !.
write_char(A,Char) :- write(Char), A1 is A-1, write_char(A1,Char).
debug_split(A, Op, V, L, P) :- 
    write(A),write_char(A,'-'),write('> '),
    write(V),write('  =  '),write(L),write('  '),write(Op),write('  '),writeln(P).
debug_split(A, Func, V, X) :- 
    write(A),write_char(A,'-'),write('> '),
    write(V),write('  =  '),write(Func),write('(   '),write(X),writeln('   )').
debug_combine(A, Op, V, L, P) :- 
    write(A),write_char(A,'='),write('> '),
    typeof(L,LT),typeof(P,PT),typeof(V,VT),
    write(LT),write(Op),write(PT),write('='),write(VT),write(': '),
    write(L),write('  '),write(Op),write('  '),write(P),write('  =  '),writeln(V).
debug_combine(A, Func, V, X) :-
    write(A),write_char(A,'='),write('> '),
    typeof(X,XT),typeof(V,VT),
    write(Func),write('('),write(XT),write(')'),write('='),write(VT),write(': '),
    write(Func),write('(   '),write(X),write('   )  =  '),writeln(V).

% =============== main logic ====================

simp(V,ZV) :- 
    simp(V,ZV1,1,s,false),
    simp(ZV1, ZV, 1, n, false).

simpDebug(V,ZV) :- 
    simp(V,ZV1,1,s,true),
    writeln(ZV1),
    write_char(60,'='),writeln(''),
    simp(ZV1, ZV, 1, n, true).

simp(-V,ZV,A,s,Debug) :- 
    simp(V,ZZV,A,s,Debug), 
    s(*,frac(-1,1),ZZV,ZV),!.
simp(frac(A,B),-R,_,n,_) :-
    A<0, A1 is -A,
    simp(frac(A1,B),R,_,n,false), !.
simp(frac(X,1),X,_,n,_) :- !.
simp(frac(0,_),0,_,n,_) :- !.
simp(frac(A,B),A/B,_,n,_) :- !.
simp(_^0,1,_,n,_) :- !.
simp(X^1,X,_,n,_) :- !.
simp(V,V,_, n,_) :- (atomic(V) ; integer(V)), !.

simp(frac(X,Y),ZF,_,s,_) :- frac(X,Y), s(frac,X,Y,ZF), !. % zlomek v zakladnim tvaru
simp(V,frac(V,1),_, s,_) :- integer(V), !.
simp(A/B,frac(A,B),_,_,_) :- frac(A,B), !.
simp(V,frac(1,1)*V^frac(1,1)+frac(0,1),_, s,_) :- atom(V), !.
simp(V^N,frac(1,1)*V^frac(N,1)+frac(0,1),_,s,_) :- atom(V),integer(N), !.

simp(V,ZV,A, Alg, Debug) :- 
    V =.. [Func,X],
    (
        Debug = true
        -> debug_split(A,Func,V,X)
        ;  true
    ), 
    A1 is A+1, simp(X,ZX,A1,Alg,Debug),
    (
        Alg=s
        -> call(s,Func,ZX,ZVV), make_CC_from_func(ZVV,ZV)
        ;  ZV=.. [Func,ZX]
    ), 
    (
        Debug = true
        -> debug_combine(A,Func,ZV,ZX)
        ;  true
    ), !.

simp(V,ZV,A, Alg, Debug) :- 
    V =.. [Op,La,Pa],
    (
        Debug = true
        -> debug_split(A,Op,V,La,Pa)
        ;  true
    ),    
    A1 is A+1, simp(La,ZL,A1,Alg,Debug), simp(Pa,ZP,A1,Alg,Debug),
    call(Alg,Op,ZL,ZP,ZV),
    (
        Debug = true
        -> debug_combine(A,Op,ZV,ZL,ZP)
        ;  true
    ).

% set_prolog_flag(answer_write_options,[max_depth(0)]).
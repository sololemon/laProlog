/* Vector Addition */

:- op(1200, xf, vectorPlus).

vectorPlus([],[],[]).
vectorPlus([H1|T1],[H2|T2],[H|T]) :-
				H is H1 + H2,
				vectorPlus(T1,T2,T).
			
/* Matrix Addition */

:- op(1200, xf, matrixPlus).

matrixPlus([],[],[]).
matrixPlus([H1|T1],[H2|T2],[H|T]) :- 
				vectorPlus(H1,H2,H),
				matrixPlus(T1,T2,T).

/* Append Vector */
appendVector([],[],[]).
appendVector([H1|[]],[H2|[]],[X]) :- append(H1, H2, X).
appendVector([H1|T1],[H2|T2],[H3|T3]) :- append(H1,H2,H3),
						appendVector(T1,T2,T3).

/* Transpose */

/* Initialization */
transpose([],[]).
/* Transpose Vector */
transpose([[H1|[]]|[]],[[H1|[]]|[]]).
transpose([[H1|T1]|[]],[[H1]|T2]) :- transpose([T1],T2).
/* Transpose Matrix */
transpose([H1|[H2|[]]],T) :- transpose([H1|[]],X),
				transpose([H2|[]],Y),
				appendVector(X,Y,T).
transpose([H1|[H2|HT]],T) :- transpose([H1|[H2|[]]],X),
				transpose(HT,Y),
				appendVector(X,Y,T).

/* Scalar Multiplication */

:- op(1200, xf, scalarVMal).
:- op(1200, xf, scalarMal).

scalarVMal(_,[],[]).
scalarVMal(S,[H1|T1],[H2|T2]) :- 
				H2 is S*H1,
				scalarVMal(S,T1,T2).
			
scalarMal(_,[],[]).
scalarMal(S,[H1|T1],[H2|T2]) :- 
				scalarVMal(S,H1,H2),
				scalarMal(S,T1,T2).

/* Vector Multiplication */

:- op(1200, xf, vMal).

vMal([],[],0).
vMal([H1|T1],[H2|T2],[X]) :- vMal([H1|T1],[H2|T2],0,X).

vMal([],[],Acc,Acc).
vMal([H1|T1],[H2|T2],Acc,X) :- NewAcc is (Acc + H1*H2),
				vMal(T1,T2,NewAcc,X).

/* Auxiliary: Vector-Matrix Multiplication, the vector is considered as row vector */

:- op(1200, xf, vMMal).

vMMal(_,[],[]).
vMMal([H1|T1],[H2|T2],[H|T]) :- vMal([H1|T1],H2,H),
				vMMal([H1|T1],T2,T).

/* Auxiliary: Matrix-Matrix Multiplication, the first matrix is considered as a combination of row vectors */

:- op(1200, xf, auxMal).

auxMal([H1|[]],M2,X) :- vMMal(H1,M2,X).
auxMal([H1|T1],M2,X) :- vMMal(H1,M2,Xfirstrow),
			auxMal(T1,M2,Xrestrows),
			appendVector(Xfirstrow,Xrestrows,X).

/* Matrix Multiplication */

:- op(1200, xf, mMal).

mMal(A,[H2|T2],X) :- length(A,L),
			length(H2,L),
			transpose(A,TransA), 
			auxMal(TransA,[H2|T2],X).
			

/* Check if a vector has all its element equal to 0 */
nullVector([]).
nullVector([0|T]) :- nullVector(T).

/* Check if a vector has all its element equal to 0 except for the first */
zeroExceptI([],0).
zeroExceptI([_|T],1) :- nullVector(T).
zeroExceptI([0|T],X) :- K is X-1,
			zeroExceptI(T,K).
			
/* Check if a matrix is diagonal */
diag([]).
diag([H|[]]) :- zeroExceptI(H,1).
diag([H|T]) :- zeroExceptI(H,1),
		diag(T,1).

diag([H|[]],X) :- K is X+1,
		   zeroExceptI(H,K).
diag([H|T],X) :- K is X+1,
		   zeroExceptI(H,K),
		   diag(T,K).


























			

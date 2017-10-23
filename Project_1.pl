% CPSC 312 Project 1 by Kayci Wang, Joanna Tu, Nathan Tong
%
% Prolog Program that operates through a Natural Language Interface to
% find appropriate cars satisfying given criteria in queries.
%
% Used nl_interface_dl.pl from David Poole's lecture notes as reference.
%
% ========================================================================
% NATURAL LANGUAGE INTERFACE
% ========================================================================
%
% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0-C4 define the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).


% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([an| T],T,_,C,C).
det(T,T,_,C,C).

% Conjunction (and) requires that all arguments before and after
% conjunction are satisfied.
con([and|T],T,_,C,C).
con(T,T,_,C,C).


% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T3,Ind,C0,C3) :-
        adj(T0,T1,Ind,C0,C1),
        con(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3).
adjectives(T,T,_,C,C).


% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing
mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% question(Question,QR,Indect,Q0,Query) is true if Query-Q0 provides an answer about Indect to Question-QR
question([what,is| T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([what| T0],T3,Ind,C0,C3):-
    noun_phrase(T0,[is|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3).
question([what| T0],T3,Ind,C0,C3):-
    noun_phrase(T0,[has|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,C,[]),
    prove(C).

% prove(L) proves all elements of L against the database
prove([]).
prove([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove(T).

% ========================================================================
% VOCABULARY DICTIONARY
% ========================================================================

% adj(T0,T1,Ind,C0,C1) is true if T0-T1 is an adjective that provides properties C1-C0 to Ind
adj([white | T],T,Ind,[white(Ind)|C],C).
adj([black | T],T,Ind,[black(Ind)|C],C).
adj([blue |T],T,Ind,[blue(Ind)|C],C).
adj([orange | T],T,Ind,[orange(Ind)|C],C).
adj([red | T],T,Ind,[red(Ind)|C],C).
adj([green |T],T,Ind,[green(Ind)|C],C).
adj([silver |T],T,Ind,[silver(Ind)|C],C).
adj([coupe | T],T,Ind,[coupe(Ind)|C],C).
adj([sedan | T],T,Ind,[sedan(Ind)|C],C).
adj([suv | T],T,Ind,[suv(Ind)|C],C).
adj([sport|T],T,Ind,[sport(Ind)|C],C).
adj([hybrid|T],T,Ind,[hybrid(Ind)|C],C).
adj([automatic|T],T,Ind,[automatic(Ind)|C],C).
adj([manual|T],T,Ind,[manual(Ind)|C],C).
adj([two-seater|T],T,Ind,[numSeats(Ind,2)|C],C).
adj([four-seater|T],T,Ind,[numSeats(Ind,4)|C],C).
adj([five-seater|T],T,Ind,[numSeats(Ind,5)|C],C).
adj([seven-seater|T],T,Ind,[numSeats(Ind,7)|C],C).
adj([eight-seater|T],T,Ind,[numSeats(Ind,8)|C],C).
adj([cheap | T],T,Ind,[cheap(Ind)|C],C).
adj([expensive | T],T,Ind,[expensive(Ind)|C],C).
adj([fair | T],T,Ind,[fair(Ind)|C],C).
adj([fast | T],T,Ind,[fast(Ind)|C],C).
adj([extremely,fast|T],T,Ind,[extremely_fast(Ind)|C],C).


% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([car | T],T,Ind,[car(Ind)|C],C).
noun([price |T], T, Ind, [price(Ind)|C],C).
noun([horsepower|T],T,Ind,[horsep(Ind)|C],C).
% The following are for proper nouns.
noun([Ind | T],T,Ind,C,C) :- car(Ind).
noun([Ind | T],T,Ind,C,C) :- number(Ind).
noun([Ind | T],T,Ind,C,C) :- price(Ind).
noun([Ind | T],T,Ind,C,C) :- horsep(Ind).


% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([is | T],T,I1,I2,[is(I1,I2)|C],C).
reln([cheaper,than|T],T,I1,I2,[cheaper_than1(I1,I2)|C],C).
reln([more,affordable,than|T],T,I1,I2,[cheaper_than(I1,I2)|C],C):-car(I2).
reln([more,expensive,than|T],T,I1,I2,[more_expensive_than1(I1,I2)|C],C).
reln([less,affordable,than|T],T,I1,I2,[more_expensive_than(I1,I2)|C],C):-car(I2).
reln([of | T],T,I1,I2,[price_of(I1,I2)|C],C).
reln([of | T],T,I1,I2,[horsep_of(I1,I2)|C],C).
reln([faster,than|T],T,I1,I2,[faster_than(I1,I2)|C],C).
% ========================================================================
% DATABASE
% ========================================================================

% Relationship Predicates
%
% cheap(C) is true if car C has a price less than $20,000
cheap(C) :-car(C),price_of(P,C),P<20000.
% expensive(C) is true if car C has a price more than $60,000
expensive(C) :- car(C),price_of(P,C),P>60000.
% fair(C) is true if car C has a price between $20,000 and $60,000
fair(C):- car(C),price_of(P,C),20000=<P,P=<60000.

% cheaper_than1(C,N) is true if the price of car C is less than the
% given number N
cheaper_than1(C,N):-car(C),price_of(P,C),P<N,number(N).
% cheaper_than(C,D) is true if the price of car C is less than the price
% of car D
cheaper_than(C,D):-car(C),price_of(P,C),car(D),price_of(B,D),P<B.
% more_expensive_than1(C,N) is true if the price of car C is more than
% the given number N
more_expensive_than1(C,N):-car(C),price_of(P,C),P>N,number(N).
% more_expensive_than(C,D) is true if the price of car C is more than
% the price of car D
more_expensive_than(C,D):-car(C),price_of(P,C),car(D),price_of(B,D),P>B.
% fast(C) is true if the horsepower of car C is more than 200HP

fast(C) :- car(C),horsep_of(H,C),H>200.
% extremely_fast(C) is true if the horsepower of car C is more than
% 400HP
extremely_fast(C) :- car(C),horsep_of(H,C),H>400.
% faster_than(C,D) is true if car C has larger horsepower than car D
faster_than(C,D):- car(C),horsep_of(H,C),car(D),horsep_of(F,D),H>F.


% car(C) is true if C is a car
car('Aston Martin DB11 V12').
car('Volkswagen Jetta(M)').
car('Volkswagen Jetta(A)').
car('Volkswagen Tiguan 2018').
car('Lexus NX300').
car('Lexus LX570').
car('Lexus IS 200T RWD').
car('Lexus GX460').
car('Nissan 370Z Coupe').
car('Nissan PathFinder S V6').
car('Nissan GT-R').
car('Mercedes-Benz GLE400 4MATIC').
car('Mercedes-Benz SLC300').
car('Mercedes-Benz Maybach').

% type(C) is true if car C is of specified type
coupe('Aston Martin DB11 V12').
coupe('Nissan 370Z Coupe').
coupe('Mercedes-Benz SLC300').
sedan('Volkswagen Jetta(M)').
sedan('Volkswagen Jetta(A)').
sedan('Lexus IS 200T RWD').
sedan('Mercedes-Benz Maybach').
suv('Volkswagen Tiguan 2018').
suv('Lexus NX300').
suv('Lexus LX570').
suv('Lexus GX460').
suv('Nissan PathFinder S V6').
sport('Nissan GT-R').
hybrid('Mercedes-Benz GLE400 4MATIC').

% transmission(C) is true if car C has specified transmission (M or A)
automatic('Aston Martin DB11 V12').
automatic('Volkswagen Jetta(A)').
automatic('Volkswagen Tiguan 2018').
automatic('Lexus NX300').
automatic('Lexus LX570').
automatic('Lexus IS 200T RWD').
automatic('Lexus GX460').
automatic('Nissan PathFinder S V6').
automatic('Mercedes-Benz GLE400 4MATIC').
automatic('Mercedes-Benz SLC300').
automatic('Mercedes-Benz Maybach').
manual('Volkswagen Jetta(M)').
manual('Nissan 370Z Coupe').
manual('Nissan GT-R').

% color(C) is true if car C is available with specified exterior color
black('Aston Martin DB11 V12').
black('Volkswagen Jetta(M)').
black('Lexus NX300').
black('Lexus GX460').
black('Nissan PathFinder S V6').
black('Mercedes-Benz GLE400 4MATIC').
blue('Volkswagen Jetta(A)').
blue('Volkswagen Jetta(M)').
blue('Volkswagen Tiguan 2018').
blue('Lexus IS 200T RWD').
blue('Nissan 370Z Coupe').
blue('Mercedes-Benz GLE400 4MATIC').
blue('Mercedes-Benz SLC300').
white('Aston Martin DB11 V12').
white('Volkswagen Jetta(A)').
white('Lexus LX570').
white('Lexus GX460').
white('Nissan PathFinder S V6').
white('Mercedes-Benz SLC300').
white('Mercedes-Benz Maybach').
silver('Lexus NX300').
silver('Lexus LX570').
silver('Nissan 370Z Coupe').
silver('Nissan GT-R').
silver('Mercedes-Benz GLE400 4MATIC').
silver('Mercedes-Benz Maybach').
orange('Volkswagen Tiguan 2018').
orange('Nissan GT-R').
red('Volkswagen Tiguan 2018').
red('Lexus NX300').
red('Lexus IS 200T RWD').
red('Nissan 370Z Coupe').
red('Nissan GT-R').
red('Mercedes-Benz SLC300').
green('Volkswagen Tiguan 2018').
green('Mercedes-Benz Maybach').

% numSeats(C,N) is true if car C has N number of seats (or "N-seater")
numSeats('Aston Martin DB11 V12',2).
numSeats('Volkswagen Jetta(A)',5).
numSeats('Volkswagen Jetta(M)',5).
numSeats('Volkswagen Tiguan 2018',5).
numSeats('Lexus NX300',5).
numSeats('Lexus LX570',8).
numSeats('Lexus IS 200T RWD',5).
numSeats('Lexus GX460',7).
numSeats('Nissan 370Z Coupe',2).
numSeats('Nissan PathFinder S V6',7).
numSeats('Nissan GT-R',4).
numSeats('Mercedes-Benz GLE400 4MATIC',5).
numSeats('Mercedes-Benz SLC300',2).
numSeats('Mercedes-Benz Maybach',5).

% price(N) is true if N is an offered price of some car
price(216495).
price(16395).
price(19395).
price(28925).
price(44050).
price(109100).
price(40150).
price(73545).
price(29998).
price(32998).
price(125000).
price(66100).
price(62200).
price(232400).

% price_of(N,C) is true if car C is offered at price N
price_of(216495,'Aston Martin DB11 V12').
price_of(16395,'Volkswagen Jetta(M)').
price_of(19395,'Volkswagen Jetta(A)').
price_of(28925,'Volkswagen Tiguan 2018').
price_of(44050,'Lexus NX300').
price_of(109100,'Lexus LX570').
price_of(40150,'Lexus IS 200T RWD').
price_of(73545,'Lexus GX460').
price_of(29998,'Nissan 370Z Coupe').
price_of(32998,'Nissan PathFinder S V6').
price_of(125000,'Nissan GT-R').
price_of(66100,'Mercedes-Benz GLE400 4MATIC').
price_of(62200,'Mercedes-Benz SLC300').
price_of(232400,'Mercedes-Benz Maybach').

% horsep(N) is true if N is the horsepower of some offered car
horsep(600).
horsep(150).
horsep(184).
horsep(235).
horsep(383).
horsep(241).
horsep(301).
horsep(332).
horsep(284).
horsep(565).
horsep(329).
horsep(523).

% horsep_of(N,C) is true if car C has N horsepower
horsep_of(600,'Aston Martin DB11 V12').
horsep_of(150,'Volkswagen Jetta(M)').
horsep_of(150,'Volkswagen Jetta(A)').
horsep_of(184,'Volkswagen Tiguan 2018').
horsep_of(235,'Lexus NX300').
horsep_of(383,'Lexus LX570').
horsep_of(241,'Lexus IS 200T RWD').
horsep_of(301,'Lexus GX460').
horsep_of(332,'Nissan 370Z Coupe').
horsep_of(284,'Nissan PathFinder S V6').
horsep_of(565,'Nissan GT-R').
horsep_of(241,'Mercedes-Benz SLC300').
horsep_of(329,'Mercedes-Benz GLE400 4MATIC').
horsep_of(523,'Mercedes-Benz Maybach').

% =======================================================================
% TESTS
% =======================================================================

% Queries that are supposed to pass:

%% ask([what,is,a,car],A).
%% A is true if it is a car.
% A = 'Aston Martin DB11 V12' ;
% A = 'Volkswagen Jetta(M)' ;
% A = 'Volkswagen Jetta(A)' ;
% A = 'Volkswagen Tiguan 2018' ;
% A = 'Lexus NX300' ;
% A = 'Lexus LX570' ;
% A = 'Lexus IS 200T RWD' ;
% A = 'Lexus GX460' ;
% A = 'Nissan 370Z Coupe' ;
% A = 'Nissan PathFinder S V6' ;
% A = 'Nissan GT-R' ;
% A = 'Mercedes-Benz GLE400 4MATIC' ;
% A = 'Mercedes-Benz SLC300' ;
% A = 'Mercedes-Benz Maybach' ;

%% ask([what,is,a,red,car],A).
%% A is true if A is a red car.
% A = 'Volkswagen Tiguan 2018' ;
% A = 'Lexus NX300' ;
% A = 'Lexus IS 200T RWD' ;
% A = 'Nissan 370Z Coupe' ;
% A = 'Nissan GT-R' ;
% A = 'Mercedes-Benz SLC300' ;

%% ask([what,is,a,cheap,car],A).
%% A is true if it is a cheap car.
% A = 'Volkswagen Jetta(M)' ;
% A = 'Volkswagen Jetta(A)' ;

%% ask([what,is,the,price,of,'Lexus LX570'],A).
%% A is true if it is the price of 'Lexus LX570'.
% A = 109100 ;

%% ask([what,is,a,two-seater,red,and,fair,car],A).
%% A is true if it is a two-seater, red, and fair car.
% A = 'Nissan 370Z Coupe' ;

%% ask([what,car,is,more,expensive,than,100000],A).
%% A is true if it is a car and price is more than 100000.
% A = 'Aston Martin DB11 V12' ;
% A = 'Lexus LX570' ;
% A = 'Nissan GT-R' ;
% A = 'Mercedes-Benz Maybach' ;

%% ask([what,car,is,more,affordable,than,'Nissan PathFinder S V6'],A).
%% A is true if it is a car and more affordable than 'Nissan PathFinder SV6'
% A = 'Volkswagen Jetta(M)' ;
% A = 'Volkswagen Jetta(A)' ;
% A = 'Volkswagen Tiguan 2018' ;
% A = 'Nissan 370Z Coupe' ;

% Complicated Query:

%% ask([what,two-seater,silver,car,is,more,affordable,than,a,red,two-seater,car],A).
%% A is true if it is a two-seater, silver car and it is more affordable
%% than a red, two-seater,car.
% A = 'Nissan 370Z Coupe' ;


% Queries that should fail:

%% ask([is,a,car],A).
%% does not start with a question word such as "what".

%% ask([what,time,is,it],A).
%% not a question relating to car.

%% ask([what,is,the,fuel,efficiency,of,a,car],A).
%% a property about car that we have not implemented. It could be added
%% in the future.

% ask([what,car,red,blue,price,of,and,a,silver,car],A).
% not a well-formed sentence in our grammar.

% ask([what,is,the,price,of,red],A).
% not asking a question about a noun in this case





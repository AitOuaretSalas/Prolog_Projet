
/*faites*/
employe(mohamed,yahyoui,33,5).
employe(yanis,madi,45,11).
employe(salas,ait,22,13).
employe(yanis,ait,66,7).
employe(anis,ait,25,10).
employe(anais,ouaret,18,3).
employe(mustapha,teraz,55,14).
employe(sadak,besidhom,22,1).
employe(farok,tisso,33,5).
employe(mahmod,fatah,70,40).



















/*rules*/
moyE(M):-findall(T,employe(X,Y,T,Z),R),moy(R,M).
maxE(N,P):-findall(Z,employe(X,Y,T,Z),R),max(R,M),employe(N,P,T,M).


sup_k(X,[X|Xs],1,Xs).
sup_k(X,[Y|Xs],K,[Y|Ys]) :- K > 1, K1 is K - 1, sup_k(X,Xs,K1,Ys).


size([],0).
size([_|T],L) :- size(T,S), L is S+1.

somme([],0).
somme([H|T],L) :-somme(T,S), L is S+H.

moy(L,M):-somme(L,S),size(L,A),M is S/A.

max([X],X).
max([X,Y|L],X):-max([Y|L],M),X>=M.
max([X,Y|L],M):-max([Y|L],M),X<M.

fact(1,1).
fact(N,R):-Nm1 is N-1,fact(Nm1,Nm2),R is Nm2*N.

lastFact([X],Y) :- write("\nlast is : "),write(X),fact(X,Y).
lastFact([_|T],Y) :- lastFact(T,Y).


occurrences([],_,0).
occurrences([X|Y],Q,N):-X=Q,occurrences(Y,Q,W),N is W+1 .
occurrences([_|Y],Q,N):-occurrences(Y,Q,N).





divs([],[],[]).
divs([A],[A],[]).
divs([A,B|T],[A|Ta],[B|Tb]) :- divs(T,Ta,Tb).

merging(A,[],A).
merging([],A,A).
merging([A|Ta],[B|Tb],[A|M]) :- A =< B, merge(Ta, [B|Tb], M).
merging([A|Ta],[B|Tb],[B|M]) :- A > B, merge([A|Ta],Tb,M).

mergingDisc(A,[],A).
mergingDisc([],A,A).
mergingDisc([A|Ta],[B|Tb],[A|M]) :- A =< B , merge([B|Tb],Ta,M).
mergingDisc([A|Ta],[B|Tb],[B|M]) :- A > B, merge(Tb,[A|Ta],M).


discFussion([],[]).
discFussion([A],[A]).
discFussion([A,B|T],S) :- divs([A,B|T],L1,L2), discFussion(L1,S1),discFussion(L2,S2),mergingDisc(S1,S2,S).


ascFussion([],[]).
ascFussion([A],[A]).
ascFussion([A,B|T],S) :- divs([A,B|T],L1,L2), ascFussion(L1,S1),ascFussion(L2,S2),merging(S1,S2,S).



insertion(X,[],[X]).
insertion(X,[Y|L],[X,Y|L]):-X=<Y.
insertion(X,[Y|L],[Y|L1]):-X>Y,insertion(X,L,L1).

ascTriInsirtion([],[]).
ascTriInsirtion([X|L],LT):- ascTriInsirtion(L,L1),insertion(X,L1,LT).

discinsertion(X,[],[X]).
discinsertion(X,[Y|L],[X,Y|L]):-X>Y.
discinsertion(X,[Y|L],[Y|L1]):-X=<Y,discinsertion(X,L,L1).

discTriInsirtion([],[]).
discTriInsirtion([X|L],LT):- discTriInsirtion(L,L1),discinsertion(X,L1,LT).

minimum([X],X).
minimum([X,Y|L],X):-minimum([Y|L],M),X=<M.
minimum([X,Y|L],M):-minimum([Y|L],M),X>M.


retirer(X,[],[]).
retirer(X,[X|L],L).
retirer(X,[Y|L],[Y|M]):- X\==Y, retirer(X,L,M).



ascTriselection([],[]).
ascTriselection([X],[X]).
ascTriselection([X,Y|L],[Z|T]):-minimum([X,Y|L],Z),retirer(Z,[X,Y|L],S),ascTriselection(S,T).

discTriselection([],[]).
discTriselection([X],[X]).
discTriselection([X,Y|L],[Z|T]):-max([X,Y|L],Z),retirer(Z,[X,Y|L],S),discTriselection(S,T).





domains
    list = integer*.

predicates
    listLen(list, integer).
    listLen(list, integer, integer).
    
    listSum(list, integer).
    listSum(list, integer, integer).
    
    sum_odd_pos(list, integer).
    sum_odd_pos(list, integer, integer).
    
    find_bigger(list, integer, list).
   
    remove_once(integer, list, list).
    
    remove_all(integer, list, list).
    
    concatination(list, list, list).
    


clauses
    listLen(List, Length) :- listLen(List, 0, Length).
    listLen([], Acc, Acc) :- !.
    listLen([_|T], Acc, Length) :- !,
        NewAcc = Acc + 1,
    	listLen(T, NewAcc, Length).
    	
    
    listSum(List, Sum) :- listSum(List, 0, Sum), !.
    listSum([], Acc, Acc) :- !.
    listSum([H|T], Acc, Sum) :- 
        NewAcc = Acc + H,
        listSum(T, NewAcc, Sum).
    	

    sum_odd_pos(List, Sum) :- !,
    sum_odd_pos(List, 0, Sum).
    sum_odd_pos([], Sum, Sum) :- !.
    sum_odd_pos([_], Sum, Sum).
    sum_odd_pos([_, X|T], Acc, Sum) :-
        NewAcc = Acc + X,
        sum_odd_pos(T, NewAcc, Sum).
        
    find_bigger([], _, []).
    find_bigger([H|T], Min, [H|ResTail]) :- 
        H > Min,
	find_bigger(T, Min, ResTail).  
    find_bigger([H|T], Min, ResTail) :- 
	H <= Min,
	find_bigger(T, Min, ResTail).
	
	
    remove_once(_, [], []) :- !.
    remove_once(X, [X|Tail], Tail) :- !.
    remove_once(X, [Y|Tail], [Y|Tail1]) :-
         remove_once(X, Tail, Tail1).
         
    remove_all(_, [], []) :- !.
    remove_all(X, [X|Tail], Result) :- !,
        remove_all(X, Tail, Result).
    remove_all(X, [Y|Tail], [Y|Tail1]) :- remove_all(X, Tail, Tail1).
    
    
    concatination([], L, L) :- !.
    concatination([H|T], L, [H|Result]) :- concatination(T, L, Result).

goal
%listLen([1, 2, 3], X).
%listSum([1, 2, 3], X).
%sum_odd_pos([1, 2, 3, 5], X).
%find_bigger([1, 2, 3, 4], 2,  List).
%remove_once(2, [1, 2, 2, 3, 4], Res).
%remove_all(2, [1, 2, 2, 3, 4], Res).
concatination([1, 2, 3],  [4, 5, 6], Res).

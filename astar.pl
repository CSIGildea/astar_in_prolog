:- dynamic(kb/1).

makeKB(File):- open(File,read,Str),
               readK(Str,K), 
               reformat(K,KB), 
               asserta(kb(KB)), 
               close(Str).                  
   
readK(Stream,[]):- at_end_of_stream(Stream),!.
readK(Stream,[X|L]):- read(Stream,X),
                      readK(Stream,L).

reformat([],[]).
reformat([end_of_file],[]) :- !.
reformat([:-(H,B)|L],[[H|BL]|R]) :- !,  
                                    mkList(B,BL),
                                    reformat(L,R).
reformat([A|L],[[A]|R]) :- reformat(L,R).
    
mkList((X,T),[X|R]) :- !, mkList(T,R).
mkList(X,[X]).

initKB(File) :- retractall(kb(_)), makeKB(File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%What to care about%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arc([H|T],Node,Cost,KB) :- member([H|B],KB), append(B,T,Node),
                                    length(B,L), Cost is L+1.
heuristic(Node,H) :- length(Node,H).

goal([]).

astar(Node,Path,Cost) :- kb(KB), astar(Node,Path,Cost,KB).


       %What we are passing to search - Starting Node, Value so far, Start Of Path, Final, Final Cost, KB passed down
astar(Node,Path,Cost,KB) :- search([[Node,0,[Node]]], Path, Cost, KB).


search([[Node,ValSoFar,Path]|T],FinalPath,FinalCost,_) :- goal(Node),FinalPath=Path,FinalCost=ValSoFar.
search([[Node,ValSoFar,Path]|T],FinalPath,FinalCost,KB) :- findall([X,NewCost,[X|Path]],
                                                                 (arc(Node,X,Arcost,KB),NewCost is Arcost+ValSoFar),
                                                                    Children),
                                                          add-to-frontier(Children,T,New), search(New, FinalPath,FinalCost,KB).


add-to-frontier([],More,More).
add-to-frontier([H | T], More, New) :- insert_sort([H | More], FSorted), add-to-frontier(T, FSorted, New).

% Edited version of insertion sort
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).

insert([Node1, Cost1, Path1], [[Node2, Cost2, Path2]|T], [[Node1, Cost1, Path1], [Node2, Cost2, Path2]|T]) :- less-than([Node1, Cost1], [Node2, Cost2]).
insert([Node1, Cost1, Path1], [[Node2, Cost2, Path2]|T], [[Node2, Cost2, Path2]|NT]) :- not(less-than([Node1, Cost1],[Node2, Cost2])), insert([Node1, Cost1, Path1], T, NT).
insert(X,[],[X]).


less-than([Node1,Cost1],[Node2,Cost2]) :-
    heuristic(Node1,Hvalue1), heuristic(Node2,Hvalue2),
    F1 is Cost1+Hvalue1, F2 is Cost2+Hvalue2,
    F1 =< F2.

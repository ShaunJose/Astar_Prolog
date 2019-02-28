:- dynamic(kb/1).

makeKB(File):-
  open(File,read,Str),
  readK(Str,K),
  reformat(K,KB),
  asserta(kb(KB)),
  close(Str).

readK(Stream,[]):- at_end_of_stream(Stream),!.
readK(Stream,[X|L]):-
  read(Stream,X),
  readK(Stream,L).

reformat([],[]).
reformat([end_of_file],[]) :- !.
reformat([:-(H, B)|L], [[H|BL]|R]) :-
  !,
  mkList(B, BL),
  reformat(L, R).
reformat([A|L], [[A]|R]) :- reformat(L, R).

mkList((X, T), [X|R]) :-
  !,
  mkList(T, R).
mkList(X, [X]).

initKB(File) :-
  retractall(kb(_)),
  makeKB(File).

astar(Node, Path, Cost) :-
  kb(KB),
  astar(Node, Path, Cost, KB).

%MAD SHTUFF

%TODO: change the latter part: astar([H|_], _, _, _) :- goal(H).

astar([Path1|OtherPaths], Path, Cost, KB) :-
  findall(Route, arc([Path1], Route, KB), [Path|Cost]).
  %addToFrontier(Routes, OtherPaths, NewRouteList),
  %astar(NewRouteList, ).

goal([]). %TODO: change this accordingly. empty tuple =

arc([H|_], Route, KB) :-
  getNode(H, Node, RestOfPath, HeadCost),
  member([Node|Children], KB),
  append(Children, RestOfPath, NewHeadPath),
  Route = [NewHeadPath, Cost],
  length(Children, L),
  Cost is L + HeadCost.


%gets the first node, other nodes (as a list) and the cost from a list of [[firstNode|OtherNodes], Cost]. Gets node if just an atomic node is passed
getNode(Node, Node, [], 1) :- atom(Node).
getNode([NodeList|CostList], HeadNode, TailNodes, Cost) :-
  extractNode(NodeList, HeadNode, TailNodes),
  extractCost(CostList, Cost).

%gets head node from a list of nodes :)
extractNode([Node|OtherNodes], Node, OtherNodes).

%gets cost from a list containing one element - the cost :D
extractCost([Cost|_], Cost).


lessThan([[Node1|_],Cost1],[[Node2|_],Cost2]) :-
  heuristic(Node1, Hvalue1),
  heuristic(Node2, Hvalue2),
  F1 is Cost1 + Hvalue1, F2 is Cost2 + Hvalue2,
  F1 =< F2.

heuristic(Node, H) :- length(Node, H).

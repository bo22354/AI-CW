% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P), % Gets agents position
    solve_task_bfs(Task, Path), !,
    agent_do_moves(A,Path), length(Path, Cost). 









 
solve_task_bfs(Task, Path) :-
    my_agent(A),
    get_agent_position(A,P),
    (achieved(Task, P) -> true
    ;otherwise   -> solve_task_bfs(Task, [[P]],[],Path)).

solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
                      append(Rest,Newfound,NewQueue),
                      solve_task_bfs(Task, NewQueue,[Pos|Visited],Path))).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj) % checks if next to correct obj or if at correct pos
    ; 
    Task=go(Pos).





























solve_task_aStar(Task, Queue, Visited, Path) :-
    sort(Queue, SortedQueue),
    SortedQueue = [Dist, Length, [Next|Rest]],
    Next = [Pos|RPath],
    % length(Next, TempCost1), Cost1 is TempCost1 - 1,

    (achieved(Task, Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> Task=go(Tar), 
                        (findall([man_dist(Length, Pos, Tar, Dist) ,[NP,Pos|RPath]],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
                        append(Rest,Newfound,NewQueue),
                        solve_task_aStar(Task, NewQueue,[Pos|Visited],Path))
                    ;
                        (findall([0 ,[NP,Pos|RPath]],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
                        append(Rest,Newfound,NewQueue),
                        solve_task_aStar(Task, NewQueue,[Pos|Visited],Path)))   
                    
                    .
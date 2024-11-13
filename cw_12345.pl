% Accomplish a given Task and return the Cost
solve_task(Task, Cost, Path) :-
    (Task = go(_) -> solve_task_aStar(Task, Path), !
    ;
    solve_task_bfs(Task, Path), !
    ),
    length(Path, Cost),
    my_agent(A),
    agent_do_moves(A, Path). 


solve_task_aStar(Task, Path) :-
    my_agent(A),
    get_agent_position(A, P),
    (achieved(Task, P) -> Path = []
    ;
        Task = go(Tar),
        man_dist(0, P, Tar, TotalCost), % Distance to Target
        InitialQueue = [[TotalCost, 0, [P]]], % Total Cost, Cost of current travel, path taken
        solve_task_aStar(Task, InitialQueue, [], Path)
    ).


solve_task_aStar(Task,  [[_, _, [Pos|RPath]]|_], _, FinalPath) :- % Check for complete
    achieved(Task, Pos),
    reverse([Pos|RPath], [_|FinalPath]).


solve_task_aStar(Task, [[_, PathCost, [Pos|RPath]]|Rest], Visited, FinalPath) :-
    Task = go(Tar),
    findall(
        [NewTotalCost, NewPathCost, [NewPos,Pos|RPath]], % What Im collecting
        (
            map_adjacent(Pos,NewPos,empty), 
            \+ memberchk(NewPos,Visited),
            NewPathCost is PathCost + 1,
            man_dist(NewPathCost, NewPos, Tar, NewTotalCost)
        ),
        NewNodes
    ),
    sort(NewNodes, SortedNodes), % Sort the new nodes to then combine 
    ord_union(Rest, SortedNodes, NewQueue), % Combine with other paths to explore
    findall(NewPos, (member([_, _, [NewPos|_]], SortedNodes)), NewPositions), % get all the NewNode Positions
    ord_union(Visited, NewPositions, NewVisited), % Add to a list of where we dont need to look
    solve_task_aStar(Task, NewQueue, [Pos|NewVisited], FinalPath).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_bfs(Task, Path) :-
    my_agent(A),
    get_agent_position(A,P),
    (achieved(Task, P) -> Path = []
    ;solve_task_bfs(Task, [[P]], [], Path)).

solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],[_|Path])
    ;
    findall(
        [NP,Pos|RPath],
        (   
            map_adjacent(Pos,NP,empty),
            \+ member(NP,Visited), 
            \+ member([NP|_],Rest)
        ),
        Newfound),
    append(Rest,Newfound,NewQueue),
    solve_task_bfs(Task, NewQueue, [Pos|Visited], Path)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







% Given the current position, and the real cost to get there it will calcualte the total estiamted cost to get to the target
man_dist(PathCost, Pos, Target, TotalCost) :- 
    map_distance(Pos, Target, RestCost), % Manhatten distance from point to target 
    TotalCost is PathCost + RestCost. 




% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj) % checks if next to correct obj or if at correct pos
    ; 
    Task=go(Pos).
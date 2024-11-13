#Looks at what tak is being performed to choose how to do the A* Search
solve_task_aStar(Task, Path) :-
    my_agent(A),
    get_agent_position(A,P),
    (achieved(Task, P) -> Path = [P]
    ;
        (Task=go(Tar), 
        man_dist(0, P, Tar, TotalCost), % Distance to Target
        InitialQueue = [[TotalCost, 0, [P]]], % Total Cost, Cost of current travel, path taken
        solve_task_aStar(Task, InitialQueue, [], Path))
    ;
        (Task=find(Obj),
        solve_task_bfs(Task, [[P]], [], Path))
    ).
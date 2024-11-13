% solve_task(Task,Cost) :-
%     my_agent(A), get_agent_position(A,P), #gets agents position
%     solve_task_dfs(Task,[P],[P|Path]), !, #runs a depth fiorst search to complete task, this performs a cut so P and Pth can't be changed
%     agent_do_moves(A,Path), length(Path,Cost). 


% Calculate the path required to achieve a Task
solve_task_dfs(Task,[P|Ps],Path) :-
    achieved(Task,P), 
    reverse([P|Ps],Path)    % if task complete then return list reverrsed
    ;
    map_adjacent(P,Q,empty), \+ member(Q,Ps), % goes to a cell adjacent to current posn if not already in list
    solve_task_dfs(Task,[Q,P|Ps],Path).
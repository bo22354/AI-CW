agent_at_exit([], _) :- fail. % No agents left to check

agent_at_exit([Agent|Rest], Target) :- % Look at each agent and check if at the exit
    get_agent_position(Agent, Pos), 
    (Pos = Target -> 
        % If agent is at the target, succeed and cut
        leave_maze(Agent), !, 
        all_exit(Target), % Function for all agents to leave maze
        true
    ;
        % Otherwise, check the remaining agents
        agent_at_exit(Rest, Target)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_exit(Target) :-
    my_agents(Agents),
    maplist(get_agent_position, Agents, Positions),
    Task = go(Target),

    findall([TotalCost, 0, [Pos]], % Create a queue of all the remaining agents
        (
            member(Pos, Positions),
            map_distance(Pos, Target, TotalCost)
        ),
        Queue),
    sort(Queue, SortedQueue), % Sort by the distance to the exit
    all_exit(SortedQueue, Task, Target).
    
all_exit([], _, _) :- true.

all_exit(SortedQueue, Task, Target) :-   
    % Find the shortest path from the exit to an agent and then get them to leave the maze 
    solve_task_aStar_NoE(Task, SortedQueue, [], 0, Path), 
    reverse(Path, RevPath),
    RevPath = [AgentLoc | Rest],
    append(Rest, [Target], FullPath),
    lookup_pos(AgentLoc, Agent),
    Agent = a(X),
    agent_do_moves(X, FullPath),
    Remove = [_,_,[AgentLoc]],
    exclude(=(Remove),SortedQueue,NewSortedQueue),
    leave_maze(X),
    all_exit(NewSortedQueue, Task, Target).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_maze :-
    my_agents(Agents),
    solve_maze([], [], Agents).

solve_maze(_,_, []) :- true. % If no agents left in maze then all have left maze

solve_maze(_, _, Agents) :- % Initial call to solve_maze
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    agent_at_exit(Agents, Target).

solve_maze(CompletedNodes, DecisionNodes, Agents) :- % Recursive call for solve_maze
    agents_moves(Agents, DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes),
    agents_do_moves(Agents,Moves),
    solve_maze(NewCompletedNodes, NewDecisionNodes, Agents).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
agents_moves([], DecisionNodes, CompletedNodes, Moves, DecisionNodes, CompletedNodes):-  % Base case
    Moves = [].

agents_moves([Agent|Rest], DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes) :-
    find_moves(Agent, DecisionNodes, CompletedNodes, Move, TempNewDecisionNodes, TempNewCompletedNodes), % Gets the next move for the current agent at the fron of the list
    agents_moves(Rest, TempNewDecisionNodes, TempNewCompletedNodes, RestMoves, NewDecisionNodes, NewCompletedNodes), %Recursively calls itself to perform same thing on all agents
    Moves = [Move | RestMoves]. % Compiles all the agents move into one list



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_moves([],[]).

find_moves(A, DecisionNodes, CompletedNodes, Move, NewDecisionNodes, NewCompletedNodes) :-
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    get_agent_position(A, Pos),

    findall([Cost, NewPos], % Gets all the possible moves that are NEW
            (
                agent_adjacent(A,NewPos, empty),
                \+ member(NewPos, CompletedNodes),
                \+ member(NewPos, DecisionNodes),
                map_distance(NewPos, Target, Cost)

            ),
            PosMoves),

    length(PosMoves, NumberOfMoves),
    (NumberOfMoves > 1 -> % If we have more then 1 move this means where we are at this point ios a decision node
        % As its a decision node this means that its not done with yet and will need to be returned to for further exploring
        sort(PosMoves, SortedMoves),
        NewDecisionNodes = [Pos|DecisionNodes],

        NewCompletedNodes = CompletedNodes,
        SortedMoves = [First|_],
        First = [_|M],
        M = [Move|_],
        true
    ;
    NumberOfMoves = 1 -> % Only 1 move meaning that once we move off it theres no more exploring for it
        (member(Pos, DecisionNodes) -> % If on a pre existing decision node with only 1 option then remove it from DecsionNodes and add to CompletedNodes 
            exclude(=(Pos), DecisionNodes, NewDecisionNodes)
        ;
            NewDecisionNodes = DecisionNodes
        ),
        NewCompletedNodes = [Pos|CompletedNodes],
        PosMoves = [First|_],
        First = [_|M],
        M = [Move|_],
        true
    ;
    % No moves so needs to go to a decison node if one is available
        (DecisionNodes \= [] ->  % If there are available decision nodes then we want to create a queu of them to find which is closest
            findall([TotalCost, 0, [DNode]],
                (
                    member(DNode, DecisionNodes),
                    \+ member(DNode, CompletedNodes),
                    lookup_pos(DNode, empty),
                    map_distance(DNode, Pos, TotalCost)
                ),
                Queue),
            
            sort(Queue, SortedQueue),
            Task = go(Pos),

            (solve_task_aStar_NoE(Task, SortedQueue, [], A, Path) ->
                % If you can find a path then go to it
                agent_do_moves(A, Path),
                find_moves(A, DecisionNodes, [Pos|CompletedNodes], Move, NewDecisionNodes, NewCompletedNodes)
            ;
                % If you cant find a path, then theres either no current DecisionNodes or theres an agent blocking the path
                % Either way just set the move to the current position
                Move = Pos,
                NewCompletedNodes = [Pos|CompletedNodes],
                NewDecisionNodes = DecisionNodes
            ),
            true 
        ;
        % If no decision node available stay in same place
            Move = Pos,
            NewCompletedNodes = [Pos|CompletedNodes],
            NewDecisionNodes = DecisionNodes   
        )   
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A* Search as before but without checking for energy levels
solve_task_aStar_NoE(Task,  [[_, _, [Pos|RPath]]|_], _, _, FinalPath) :- % Check for complete
    achieved(Task, Pos),
    FinalPath = RPath.


solve_task_aStar_NoE(Task, [[_, PathCost, [Pos|RPath]]|Rest], Visited, Agent, FinalPath) :-
    Task = go(Tar),
    findall(
        [NewTotalCost, NewPathCost, [NewPos,Pos|RPath]], % What Im collecting
        (
            map_adjacent(Pos,NewPos,Obj), 
            (Obj = a(Agent); Obj = empty),
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
    solve_task_aStar_NoE(Task, NewQueue, [Pos|NewVisited], Agent, FinalPath).
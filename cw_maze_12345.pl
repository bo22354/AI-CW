% Solve the maze, aiming to get all the agents to p(N,N)
agent_at_exit([], _) :- fail. % No agents left to check

agent_at_exit([Agent|Rest], Target) :-
    get_agent_position(Agent, Pos),
    (Pos = Target -> 
        % If agent is at the target, succeed and cut
        print_debug("Agent at target", Agent),
        leave_maze(Agent),
        !, true
    ;
        % Otherwise, check the remaining agents
        agent_at_exit(Rest, Target)
    ).


solve_maze :-
    my_agents(Agents),
    solve_maze([], [], Agents).

solve_maze(_,_, []) :- true.

solve_maze(_, _, Agents) :-
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    agent_at_exit(Agents, Target).




solve_maze(CompletedNodes, DecisionNodes, Agents) :-
    Agents = [A|_],
    print_debug("---------------------------------------------------------",""),
    print_debug("Agents", Agents),
    find_moves(A, DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes),
    agents_do_moves(Agents,Moves),
    solve_maze(NewCompletedNodes, NewDecisionNodes, Agents).
    

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([],[]).

find_moves(A, DecisionNodes, CompletedNodes, Move, NewDecisionNodes, NewCompletedNodes) :-
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    get_agent_position(A, Pos),
    print_debug("Agent Position", Pos),
    print_debug("CompletedNodes", CompletedNodes),
    print_debug("DecisionNodes", DecisionNodes),
    print_debug("", ""),



    findall([Cost, NewPos],
            (
                agent_adjacent(A,NewPos, empty),
                \+ member(NewPos, CompletedNodes),
                \+ member(NewPos, DecisionNodes),
                map_distance(NewPos, Target, Cost)

            ),
            PosMoves),
    length(PosMoves, NumberOfMoves),
    (NumberOfMoves > 1 ->

        print_debug("Multiple Moves", ""),
        sort(PosMoves, SortedMoves),
        NewDecisionNodes = [Pos|DecisionNodes],

        NewCompletedNodes = CompletedNodes,
        SortedMoves = [First|_],
        First = [_|Move],
        true
    ;
        NumberOfMoves = 1,
        print_debug("1 Move", ""),
        %%%%%%%%%%%%%% NEED TO ADD CHECKS FOR WHEN COMING BACK TO A DECISION NODE TO REMOVE FROM DECISION NODE LIST
        (member(Pos, DecisionNodes) -> % If on a pre existing decision node with only 1 option then remove it from DecsionNodes and add to CompletedNodes 
            exclude(=(Pos), DecisionNodes, NewDecisionNodes)
        ;
            NewDecisionNodes = DecisionNodes
        ),
        NewCompletedNodes = [Pos|CompletedNodes],
        PosMoves = [First|_],
        First = [_|Move],
        true
    ;
        print_debug("No Moves", ""),
        % Find the shortest path to Pos when passing in the queue of all the current decision nodes
        findall([TotalCost, 0, [DNode]],
            (
                member(DNode, DecisionNodes),
                \+ member(DNode, CompletedNodes),
                map_distance(DNode, Pos, TotalCost)
            ),
            Queue),
        sort(Queue, SortedQueue),
        Task = go(Pos),
        solve_task_aStar_NoE(Task, SortedQueue, [], A, Path),
        agent_do_moves(A, Path),
        find_moves(A, DecisionNodes, [Pos|CompletedNodes], Move, NewDecisionNodes, NewCompletedNodes),
        true
   
    ).
    % find_moves(As,Moves). % For doing rest of agents






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










% Plan (1 Agent) 
% Check if in bottom cornerr
% If you are then exit maze otherwise do following:

% Find all options to start with
% Add current pos to a list called decisions
% If more than 1 option thats not in completed
    % Add current position to list called decisions
    % Move to a one of the new positions

% Else if 1 option thats not in completed
    % Add current pos to list called completed
    % And move to the new position
    % Repeat from beggining
% Else 
    % Add current pos to list called completed
    % Find path to the closest node thats in decision and then repeat from beginning



agent_at_exit([], _) :- fail. % No agents left to check

agent_at_exit([Agent|Rest], Target) :- % Look at each agent and check if at the exit
    get_agent_position(Agent, Pos), 
    (Pos = Target -> 
        % If agent is at the target, succeed and cut
        print_debug("Agent at target", Agent),
        leave_maze(Agent),
        % Need to make a function then to go through each agent and find shortest path to exit and then go to it
        !, 
        all_exit(Target),
        true
    ;
        % Otherwise, check the remaining agents
        agent_at_exit(Rest, Target)
    ).
all_exit(Target) :-
    my_agents(Agents),
    maplist(get_agent_position, Agents, Positions),
    Task = go(Target),

    findall([TotalCost, 0, [Pos]],
        (
            member(Pos, Positions),
            map_distance(Pos, Target, TotalCost)
        ),
        Queue),
    sort(Queue, SortedQueue),
    all_exit(SortedQueue, Task, Target).
    
all_exit([], _, _) :- true.

all_exit(SortedQueue, Task, Target) :-   
    solve_task_aStar_NoE(Task, SortedQueue, [], 0, Path),
    print_debug("Path", Path),
    reverse(Path, RevPath),
    print_debug("Here", ""),
    RevPath = [AgentLoc | Rest],
    print_debug("Here", ""),
    append(Rest, [Target], FullPath),
    print_debug("AgentLoc", AgentLoc),
    print_debug("FullPath", FullPath),

    lookup_pos(AgentLoc, Agent),
    print_debug("Agent", Agent),
    Agent = a(X),
    agent_do_moves(X, FullPath),
    Remove = [_,_,[AgentLoc]],
    exclude(=(Remove),SortedQueue,NewSortedQueue),
    leave_maze(X),
    print_debug("New Queue", NewSortedQueue),
    all_exit(NewSortedQueue, Task, Target).

    % Need to find out which agents path it is and then make them do the moves and then recursively call until no asgents left


solve_maze :-
    my_agents(Agents),
    solve_maze([], [], Agents).

solve_maze(_,_, []) :- true. % If no agents left in maze then all have left maze

solve_maze(_, _, Agents) :- % Initial call to solve_maze
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    agent_at_exit(Agents, Target).

solve_maze(CompletedNodes, DecisionNodes, Agents) :- % Recursive call for solve_maze
    % Agents = [A|_],
    print_debug("---------------------------------------------------------",""),
    agents_moves(Agents, DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes),
    % find_moves(A, DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes),
    print_debug("Agents Moves", Moves),

    agents_do_moves(Agents,Moves),
    solve_maze(NewCompletedNodes, NewDecisionNodes, Agents).
    
agents_moves([], DecisionNodes, CompletedNodes, Moves, DecisionNodes, CompletedNodes):- 
    Moves = [].

agents_moves([Agent|Rest], DecisionNodes, CompletedNodes, Moves, NewDecisionNodes, NewCompletedNodes) :-
    find_moves(Agent, DecisionNodes, CompletedNodes, Move, TempNewDecisionNodes, TempNewCompletedNodes),
    agents_moves(Rest, TempNewDecisionNodes, TempNewCompletedNodes, RestMoves, NewDecisionNodes, NewCompletedNodes),
    Moves = [Move | RestMoves],
    print_debug("CompletedNodes", CompletedNodes),
    print_debug("DecisionNodes", DecisionNodes).




%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([],[]).

find_moves(A, DecisionNodes, CompletedNodes, Move, NewDecisionNodes, NewCompletedNodes) :-
    ailp_grid_size(GridSize),
    Target = p(GridSize, GridSize),
    get_agent_position(A, Pos),
    print_debug("Agent", A),
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
    print_debug("PosMoves", PosMoves),
    length(PosMoves, NumberOfMoves),
    (NumberOfMoves > 1 ->

        print_debug("Multiple Moves", ""),
        sort(PosMoves, SortedMoves),
        NewDecisionNodes = [Pos|DecisionNodes],

        NewCompletedNodes = CompletedNodes,
        SortedMoves = [First|_],
        First = [_|M],
        M = [Move|_],
        true
    ;
    NumberOfMoves = 1 ->
        print_debug("1 Move", ""),
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
    % Find the shortest path to Pos when passing in the queue of all the current decision nodes
        print_debug("No Moves", ""),
        (DecisionNodes \= [] -> 
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
                print_debug("PATH", Path),

                agent_do_moves(A, Path),
                find_moves(A, DecisionNodes, [Pos|CompletedNodes], Move, NewDecisionNodes, NewCompletedNodes)
            ;
                print_debug("No Path", ""),
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
            ((Obj = a(Agent), print_debug("Obj", Obj)); Obj = empty),
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
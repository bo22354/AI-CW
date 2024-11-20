print_debug(Name, Value) :- format("~w: ~w~n", [Name, Value]).


% Accomplish a given Task and return the Cost
solve_task(Task, Cost, Path) :-
    my_agent(A),
    get_agent_energy(A, Energy),
    (Task = go(_) -> 
        (solve_task_aStar(Task, Energy, Path1) -> 
            Path = Path1,
            true
        ;
            recharge_stations(_, StationPaths),
            % print_debug('Stations Paths', StationPaths),
            Task = go(Tar),
            maplist(reverse, StationPaths, RevStationPaths),
            
            findall( % Create a new queue with the 
                [TotalCost, 0, Path, 100],
                (
                    member(Path, StationPaths),
                    Path = [Pos| _],
                    man_dist(0, Pos, Tar, TotalCost)
                ),
                Queue
            ),
            print_debug('Queue', Queue),
            


            solve_task_aStar(Task, Queue, [], Path2), % will return the path to target from (and including) the recharge station
            
            print_debug('Found Target Path', Path2),
            print_debug('Rev', RevStationPaths),
            
            
            

            find_sub_path(RevStationPaths, Path2, StationPath), % Find which Station Path is the path being used for Full Path
            print_debug('StationPath', StationPath),
            append(StationPath, Rest, Path2), % Remove Station Path from Full Path
            print_debug('Rest', Rest),
            agent_do_moves(A, StationPath), % Run moves for getting to the station

            % Recharge
            get_agent_position(A, CurrPos), 
            once(
                map_adjacent(CurrPos,_,c(X))
            ),
            print_debug('Temp', "Temp"),
            agent_topup_energy(A, c(X)),
            Path = Rest,
            true
        )
    ;
        solve_task_bfs(Task, Path), !
    ),
    length(Path, Cost),
    agent_do_moves(A, Path), !. 

find_sub_path(StationPaths, FullPath, StationPath) :-
    member(Temp, StationPaths), % Gets each station path individually
    Temp = [_|StationPath],     % Removes the first element
    append(_, Rest, FullPath),  % Splits FullPath into a prefix and Rest
    append(StationPath, _, Rest), % Checks if StationPath is a prefix of Rest
    !.                          % Cut to stop after finding the first match




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


solve_task_aStar(Task, Energy, Path) :-
    my_agent(A),
    get_agent_position(A, P),
    (achieved(Task, P) -> Path = []
    ;
        Task = go(Tar),
        man_dist(0, P, Tar, TotalCost), % Distance to Target
        InitialQueue = [[TotalCost, 0, [P], Energy]], % Total Cost, Cost of current travel, path taken
        solve_task_aStar(Task, InitialQueue, [], Path)
    ).


solve_task_aStar(Task,  [[_, _, [Pos|RPath], Energy]|_], _, FinalPath) :- % Check for complete
    achieved(Task, Pos),
    Energy >= 0,
    reverse([Pos|RPath], [_|FinalPath]).


solve_task_aStar(Task, [[_, PathCost, [Pos|RPath], Energy]|Rest], Visited, FinalPath) :-
    Task = go(Tar),
    NewEnergy is Energy -1,
    findall(
        [NewTotalCost, NewPathCost, [NewPos,Pos|RPath], NewEnergy], % What Im collecting
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
    findall(NewPos, (member([_, _, [NewPos|_], _], SortedNodes)), NewPositions), % get all the NewNode Positions
    ord_union(Visited, NewPositions, NewVisited), % Add to a list of where we dont need to look
    solve_task_aStar(Task, NewQueue, [Pos|NewVisited], FinalPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    append(Rest,Newfound,NewQueue), % Add all new exploration options to the queue
    solve_task_bfs(Task, NewQueue, [Pos|Visited], Path)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recharge_stations(Stations, StationPaths) :-
    my_agent(A),
    get_agent_position(A,P),
    get_agent_energy(A, Energy),
    recharge_stations([[[P], Energy]], [], [], [], Stations, StationPaths).

recharge_stations([], _, Stations, StationPaths, Stations, StationPaths) :-
    print_debug('Base Case', 'Queue Empty'),
    print_debug('Stations Found', Stations),
    print_debug('Stations Paths', StationPaths).

recharge_stations([[[Pos|_], Energy]|Rest], Visited, Stations, StationPaths, FinalStations, FinalStationPaths) :-
    Energy =< 0,!, % Cut to avoid backtracking into the next clause
    recharge_stations(Rest, [Pos|Visited], Stations, StationPaths, FinalStations, FinalStationPaths).


recharge_stations([[[Pos|Path], Energy]|Rest], Visited, Stations, StationPaths, FinalStations, FinalStationPaths) :-
    NewEnergy is Energy -1,
    findall(
        [[NewPos, Pos|Path], NewEnergy],
        (
            map_adjacent(Pos, NewPos, empty),  
            \+ member(NewPos, Visited),   % NewPos hasnt been visited
            \+ member([[NewPos|_], _] , Rest)  % NewPos isnt queued to be visited
        ),
        EmptyNodes
    ),
    findall(
        [NewPos, Pos|Path],
        (
            map_adjacent(Pos, NewPos, c(_)),  
            \+ member(NewPos, Stations)   % NewPos isnt a station already found
        ),
        StationNodes
    ),
    maplist(nth0(0), StationNodes, StationPositions), % get all the Station Positions
    append(Stations, StationPositions, NewStations),

    (StationNodes = [] ->
        NewStationPaths = StationPaths
    ;
        append([[Pos|Path]], StationPaths, NewStationPaths) % If we find a adjacent Node that is a station add the path to it to the list
    ),
    

    append(Rest, EmptyNodes, NewQueue), % Add all new exploration options to the queue

    recharge_stations(NewQueue, [Pos|Visited], NewStations, NewStationPaths, FinalStations, FinalStationPaths).


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
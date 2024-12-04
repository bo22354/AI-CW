% True if link L appears on As wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eliminate([],unknown,_, _, _,_) :- !.
eliminate(_,unknown,_,[],_,_) :- !.

eliminate(As,A,Stations, OracleLocations, MaxEnergy,AskEnergy) :- 
    As=[A], !
    ;
    my_agent(Agent),
    get_agent_energy(Agent, Energy), 

    get_agent_position(Agent, Pos),
    (Energy = MaxEnergy -> % If its at max energy (Either started at max or just gone to station), we want it to go to a new oracle
        % Yes
        find_closest(OracleLocations, Pos, Energy, Agent, Path), % Gets the closest unvisited oracle
        go_to(Path, Agent, OracleLocations, As, NewOracleLocations, ViableAs), % Goes to found oracle
        eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy), % Recursivve call with one less oracle in list
        true

        % Need to then do the bit after aswell 
    ;
        % Check if can go to next closest Oracle and get back to station
        find_closest(Stations, Pos, Energy, Agent, StationPath), % Gets the closest station 
        
        (StationPath = failed -> % Cant get to station so may aswell try go to another oracle
            find_closest(OracleLocations, Pos, Energy, Agent, OraclePath), % Gets the closest unvisited oracle
            (OraclePath = failed -> % No viable path 
                eliminate(As, unknown, Stations, [], MaxEnergy, AskEnergy),
                true
            ;
                length(OraclePath, OPathLength),
                OCost is OPathLength -2,
                (Energy >= (OCost + AskEnergy) -> % Checks it is possible to get to the oracle and ask a question
                    go_to(OraclePath, Agent, OracleLocations, As, NewOracleLocations, ViableAs),
                    eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy),
                    true
                ;
                    eliminate(As, unknown, Stations, [], MaxEnergy, AskEnergy),
                    true
                ),
                true
            ),
            true
            
        ;
            find_closest(OracleLocations, Pos, Energy, Agent, OraclePath), % Gets the closest unvisited oracle
            (OraclePath = failed -> % get to station but not a path to get any other oracle first
                go_to(StationPath, Agent, Stations, As, _, _),
                eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy),
                true
            ;
                OraclePath = [_, L2|_], % Location is where the oracle actually is

                find_closest(Stations, L2, Energy, Agent, StationPath2), % Gets the closest station 
                (StationPath2 = failed -> % Cant get to station from next node so just go station now
                    go_to(StationPath, Agent, Stations, As, _, _),
                    eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy),
                    true  
                ;
                    length(OraclePath, OPathLength),
                    length(StationPath2, SPathLength),
                    SCost is SPathLength - 2,
                    OCost is OPathLength -2,
                    (Energy >= (OCost + SCost + AskEnergy) ->
                        % Yes, go to oracle
                        go_to(OraclePath, Agent, OracleLocations, As, NewOracleLocations, ViableAs),
                        eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy),
                        true
                    ;
                        % No, go to station
                        go_to(StationPath, Agent, Stations, As, _, _),
                        eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy),
                        true
                    ),
                    true                
                ),
                true
                
           
            ),
            true

        ),
        true  
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_closest(ObjectLocations, Pos, Energy, Agent, Path) :-
    NewEnergy is Energy +2, % Calculates the path but includes 2 extra spaces
    findall(
            [TotalCost, 0, [ObjectLocation], NewEnergy],
            (
            member(ObjectLocation, ObjectLocations),
            man_dist(0, ObjectLocation, Pos, TotalCost) 
            ),
            Queue
        ),
        sort(Queue, NewQueue),

        T = go(Pos),
        (solve_task_aStar(T ,NewQueue, [], Agent, Path) ->
            true
        ;
            Path = failed,
            true
        ).
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go_to(ObjectPath, Agent, ObjectLocations, As, NewObjectLocations, ViableAs) :-
        ObjectPath = [Location|Rest], % Location is where the oracle actually is
        Rest = [Adjacent|_],
        map_adjacent(Adjacent, Location, Obj), % Gets which oracle were next too

        reverse(Rest, [_|RevPath]), % Removing first element as thats where the agent is

        agent_do_moves(Agent, RevPath), % Move to the oracles
        (Obj = o(_) ->
            agent_ask_oracle(Agent,Obj,link,L), % Ask question
            include(actor_has_link(L),As,ViableAs), % Remove any actors that no longer fit
            select(Location, ObjectLocations, NewObjectLocations), % Remove the oracle we just visited from teh list so it cant be visited again
            true
        ;
            agent_topup_energy(Agent, Obj), % Top up energy
            true
        ).
                 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_oracles(Oracles) :-
    my_agent(A),
    get_agent_position(A,P),
    find_oracles([[P]], [], [], Oracles).

find_oracles([], _, Oracles, Oracles) :- true.

find_oracles([[Pos|Path]|Rest], Visited, Oracles, FinalOracles) :-
    findall(
        [NewPos, Pos|Path],
        (
            map_adjacent(Pos, NewPos, empty),  
            \+ member(NewPos, Visited),   % NewPos hasnt been visited
            \+ member([NewPos|_], Rest)  % NewPos isnt queued to be visited
        ),
        EmptyNodes
    ),
    findall(
        [NewPos],
        (
            map_adjacent(Pos, NewPos, o(_)),  
            \+ member(NewPos, Oracles)   % NewPos isnt a station already found
        ),
        OracleNodes
    ),

    maplist(nth0(0), OracleNodes, OraclePositions), % Extract oracle positions
    ord_union(Oracles, OraclePositions, NewOracles),  

    append(Rest, EmptyNodes, NewQueue), % Add all new exploration options to the queue

    find_oracles(NewQueue, [Pos|Visited], NewOracles, FinalOracles).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deduce the identity of the secret actor A
find_identity(A) :- 
    my_agent(Agent),
    get_agent_position(Agent, P),

    recharge_stations([[[P], 10000]], [], [], [], Stations, _), % Find locations of all recharge Stations
    find_oracles(OracleLocations),

    findall(A,actor(A),As), % Collect together all the actors

    ailp_grid_size(GridSize),
    MaxEnergy is ceiling((GridSize*GridSize) / 4),
    AskEnergy is ceiling(MaxEnergy/10),

    eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy).
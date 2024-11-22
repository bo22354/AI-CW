% True if link L appears on As wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eliminate([],_,_, _, _,_) :- false.


% Attempt to solve by visiting each oracle in ID order
eliminate(As,A,Stations, OracleLocations, MaxEnergy,AskEnergy) :- 
    As=[A], !
    ;
    print_debug("----------------------------------------", ""),
    my_agent(Agent),
    get_agent_energy(Agent, Energy), 
    print_debug("Energy", Energy),
    get_agent_position(Agent, Pos),
    (Energy = MaxEnergy -> % If its at max energy (Either started at max or just gone to station), we want it to go to a new oracle
        % Yes
        print_debug("MaxEnergy", ""),
        find_closest(OracleLocations, Pos, Energy, Agent, Path), % Gets the closest unvisited oracle
        go_to(Path, Agent, OracleLocations, As, NewOracleLocations, ViableAs),
        eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy),
        true

        % Need to then do the bit after aswell 
    ;
        % Check if can go to next closest Oracle and get back to station
        print_debug("Not MaxEnergy", ""),
        find_closest(Stations, Pos, Energy, Agent, StationPath), % Gets the closest station 
        
        (StationPath = [] -> % Cant get to station so may aswell try go to another oracle
            print_debug("Can't get to Station", ""),
            find_closest(OracleLocations, Pos, Energy, Agent, OraclePath), % Gets the closest unvisited oracle
            go_to(OraclePath, Agent, OracleLocations, As, NewOracleLocations, ViableAs),
            eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy),
            true
        ;
            print_debug("Can get to Station", ""),
            find_closest(OracleLocations, Pos, Energy, Agent, OraclePath), % Gets the closest unvisited oracle
            OraclePath = [_, L2|_], % Location is where the oracle actually is

            find_closest(Stations, L2, Energy, Agent, StationPath2), % Gets the closest station 
            
            length(OraclePath, OPathLength),
            length(StationPath2, SPathLength),
            SCost is SPathLength - 2,
            OCost is OPathLength -2,
            print_debug("O Cost", OCost),
            print_debug("S Cost", SCost),
            (Energy >= (OCost + SCost + AskEnergy) ->
                % Yes, go to oracle

                print_debug("Going oracle and Possibly Station Next Time", ""),
                go_to(OraclePath, Agent, OracleLocations, As, NewOracleLocations, ViableAs),
                eliminate(ViableAs, A, Stations, NewOracleLocations, MaxEnergy, AskEnergy),
                true
            ;
                % No, go to station
                print_debug("Going to Station", ""),
                go_to(StationPath, Agent, Stations, As, _, _),
                eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy),
                true
            ),
            true
        ),
        true  
    ).





find_closest(ObjectLocations, Pos, Energy, Agent, Path) :-
    findall(
            [TotalCost, 0, [ObjectLocation], Energy],
            (
            member(ObjectLocation, ObjectLocations),
            man_dist(0, ObjectLocation, Pos, TotalCost) 
            ),
            Queue
        ),
        sort(Queue, NewQueue),

        T = go(Pos),
        solve_task_aStar(T ,NewQueue, [], Agent, Path),
        print_debug("Task", T),
        print_debug("Path to get there", Path).


go_to(ObjectPath, Agent, ObjectLocations, As, NewObjectLocations, ViableAs) :-
        ObjectPath = [Location|Rest], % Location is where the oracle actually is
        Rest = [Adjacent|_],
        map_adjacent(Adjacent, Location, Obj), % Gets which oracle were next too

        reverse(Rest, [_|RevPath]), % Removing first element as thats where the agent is
        % print_debug("RevPath", RevPath),

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
    print_debug("MAX Energy", MaxEnergy),
    print_debug("Ask Energy", AskEnergy),
    print_debug("-----------------------------------", ""),



    eliminate(As, A, Stations, OracleLocations, MaxEnergy, AskEnergy).



% Plan
% Find locations of all recharge Stations
% Go to closest oracle and ask question (-1/10 Energy)
% Check if enough energy to go to station
% If you dont then just go to closest oracle and keep going until energy runs out
% If you do then find next closest oracle and check if we still have enough energy to get back to a charge station
    % If you wouldnt be able to get back to charge station then go straight to charge station
    % If you would then repeat the same prrocess (check closest oracle and if that would make it to charge staiton too)
    % When you eventually go to charge station then go to closest unvisited oracle and repeat process again

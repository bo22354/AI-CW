% True if link L appears on As wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).

% Attempt to solve by visiting each oracle in ID order
eliminate(As,A,K, Stations, Agent) :- 
    As=[A], !
    ;
    get_agent_energy(Agent, Energy), % Not Needed just useful right now
    print_debug("Energy", Energy),

    solve_task(find(o(K)), _ ,Agent, Path), !,
    print_debug("Returned", ""),

    agent_ask_oracle(Agent,o(K),link,L), 
    include(actor_has_link(L),As,ViableAs), 
    K1 is K+1, 
    eliminate(ViableAs,A,K1).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    my_agent(Agent),
    get_agent_position(Agent, P),

    recharge_stations([[[P], 10000]], [], [], [], Stations, _), % Find locations of all recharge Stations

    findall(A,actor(A),As), % Collect together all the actors

    eliminate(As,A,_, Stations, Agent).




% Plan
% Find locations of all recharge Stations
% Go to closest oracle and ask question (-1/10 Energy)
% Check if enough energy to go to station
% If you dont then just go to closest oracle and keep going until energy runs out
% If you do then find next closest oracle and check if we still have enough energy to get back to a charge station
    % If you wouldnt be able to get back to charge station then go straight to charge station
    % If you would then repeat the same prrocess (check closest oracle and if that would make it to charge staiton too)
    % When you eventually go to charge station then go to closest unvisited oracle and repeat process again

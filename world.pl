fluent(location(robbie, cafeteria)).
fluent(door(cafeteria-northwesthallway, unlocked)).
fluent(door(cafeteria-northeasthallway, unlocked)).
fluent(door(cafeteria-centerhallway, unlocked)).

fluent(door(northeasthallway-weapons, unlocked)).

fluent(door(northwesthallway-medbay,unlocked)).
fluent(door(northwesthallway-upperengine, unlocked)).

fluent(door(centerhallway-admin, unlocked)).
fluent(door(centerhallway-storage, unlocked)).

fluent(door(weapons-easthallway, unlocked)).

fluent(door(easthallway-o2, unlocked)).
fluent(door(easthallway-navigation, unlocked)).
fluent(door(easthallway-shields, unlocked)).

fluent(door(shields-southeasthallway, unlocked)).
fluent(door(southeasthallway-communications, unlocked)).
fluent(door(southeasthallway-storage, unlocked)).

fluent(door(storage-southwesthallway,unlocked)).
fluent(door(southwesthallway-electrical,unlocked)).
fluent(door(southwesthallway-lowerengine, unlocked)).

fluent(door(lowerengine-westhallway, unlocked)).
fluent(door(westhallway-reactor, unlocked)).
fluent(door(westhallway-security, unlocked)).
fluent(door(westhallway-upperengine, unlocked)).

fluent(holding(nothing)).

fluent(chart(course, false)).
fluent(fix(wiring, false)).
fluent(clean(filter, false)).
fluent(start(reactor, false)).
fluent(reboot(wifi, false)).
%facts, unlike fluents, don't change
fact(home(car-key, hallway)).
fact(home(garage-key, hallway)).
fact(home(vacuum-cleaner, cafeteria)).

% s0, the initial situation, is the (ordered) set
% of fluents
s0(Situation) :-
    setof(S, fluent(S), Situation).

% Take a list of Actions and execute them
execute_process(S1, [], S1). % Nothing to do
execute_process(S1, [Action|Process], S2) :-
    poss(Action, S1), % Ensure valid Process
    result(S1, Action, Sd),
    execute_process(Sd, Process, S2).

% Does a fluent hold (is true) in the Situation?
% This is the query mechanism for Situations
% Use-case 1: check a known fluent
holds(Fluent, Situation) :-
    ground(Fluent), ord_memberchk(Fluent, Situation), !.
% Use-case 2: search for a fluent
holds(Fluent, Situation) :-
    member(Fluent, Situation).

% Utility to replace a fluent in the Situation
replace_fluent(S1, OldEl, NewEl, S2) :-
    ord_del_element(S1, OldEl, Sd),
    ord_add_element(Sd, NewEl, S2).

% Lots of actions to declare here...
% Still less code than writing out the
% graph we're representing
%
% Robbie's Action Repertoire :
%  - goTo(Origin, Destination)
%  - pickup(Item)
%  - drop(Item)
%  - put_away(Item) % the tidy version of drop
%  - unlock(Room1-Room2)
%  - lock(Room1-Room2)
%  - clean_car

poss(goto(L), S) :-
    % If robbie is in X and the door is unlocked
    holds(location(robbie, X), S),
    (   holds(door(X-L, unlocked), S)
    ;   holds(door(L-X, unlocked), S)
    ).
poss(pickup(X), S) :-
    % If robbie is in the same place as X and not
    % holding anything
    dif(X, robbie), % Can't pickup itself!
    holds(location(X, L), S),
    holds(location(robbie, L), S),
    holds(holding(nothing), S).
poss(put_away(X), S) :-
    % If robbie is holding X, it belongs in L
    % and robbie is in L (location(X, L) is implicit)
    holds(holding(X), S),
    fact(home(X, L)),
    holds(location(robbie, L), S).
poss(drop(X), S) :-
    % Can drop something if holding it
    % Can't drop nothing!
    dif(X, nothing),
    holds(holding(X), S).
poss(unlock(R1-R2), S) :-
    % Can unlock door between R1 and R2
    % Door is locked
    holds(door(R1-R2, locked), S),
    % Holding the key to the room
    holds(holding(R2-key), S),
    % Located in one of the rooms
    (   holds(location(robbie, R1), S)
    ;   holds(location(robbie, R2), S)
    ).
poss(lock(R1-R2), S) :-
    % Can lock door R1-R2
    % Only if it's locked, robbie has the key
    % and is in one of the rooms
    holds(door(R1-R2, unlocked), S),
    holds(holding(R2-key), S),
    (   holds(location(robbie, R1), S)
    ;   holds(location(robbie, R2), S)
    ).
poss(clean_car, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, navigation), S),
    holds(holding(vacuum-cleaner), S).

poss(chart_course, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, navigation), S).

poss(fix_wiring, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, electrical), S).

poss(clean_filter, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, o2), S).

poss(start_reactor, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, reactor), S).

poss(reboot_wifi, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, communications), S).


result(S1, goto(L), S2) :-
    % Robbie moves
    holds(location(robbie, X), S1),
    replace_fluent(S1, location(robbie, X),
                   location(robbie, L), Sa),
    % If Robbie is carrying something, it moves too
    dif(Item, nothing),
    (
        holds(holding(Item), S1),
        replace_fluent(Sa, location(Item, X),
                       location(Item, L), S2)
    ;   \+ holds(holding(Item), S1),
        S2 = Sa
    ).
result(S1, pickup(X), S2) :-
    % Robbie is holding X
    replace_fluent(S1, holding(nothing),
                   holding(X), S2).
result(S1, drop(X), S2) :-
    % Robbie is no-longer holding X,
    % its location is not changed
    replace_fluent(S1, holding(X),
                   holding(nothing), S2).
result(S1, put_away(X), S2) :-
    % Robbie is no-longer holding X,
    % its location is not changed
    replace_fluent(S1, holding(X),
                   holding(nothing), S2).
result(S1, unlock(R1-R2), S2) :-
    % Door R1-R2 is unlocked
    replace_fluent(S1, door(R1-R2, locked),
                       door(R1-R2, unlocked), S2).
result(S1, lock(R1-R2), S2) :-
    % Door R1-R2 is locked
    replace_fluent(S1, door(R1-R2, unlocked),
                   door(R1-R2, locked), S2).
result(S1, clean_car, S2) :-
    % The car is clean
    replace_fluent(S1, clean(car, false),
                   clean(car, true), S2).

result(S1, chart_course, S2) :-
    % The car is clean
    replace_fluent(S1, chart(course, false),
                   chart(course, true), S2).

result(S1, fix_wiring, S2) :-
    % The car is clean
    replace_fluent(S1, fix(wiring, false),
                   fix(wiring, true), S2).

result(S1, clean_filter, S2) :-
    % The car is clean
    replace_fluent(S1, clean(filter, false),
                   clean(filter, true), S2).

result(S1, start_reactor, S2) :-
    % The car is clean
    replace_fluent(S1, start(reactor, false),
                   start(reactor, true), S2).

result(S1, reboot_wifi, S2) :-
    % The car is clean
    replace_fluent(S1, reboot(wifi, false),
                   reboot(wifi, true), S2).




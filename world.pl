% Define world module
:- module(world, [fluent/1, s0/1, poss/2, result/3]).

%fluent are things that change value between states 

%initial location of robot
fluent(location(bluedude, cafeteria)).

%represent map as vertices
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


%state of the current world where goals are not completed
fluent(holding(nothing)).

fluent(chart(course, false)).
fluent(fix(wiring, false)).
fluent(clean(filter, false)).
fluent(start(reactor, false)).
fluent(reboot(wifi, false)).


% s0, the initial situation, is the (ordered) set
% of fluents
% setof(+Template, +Goal, -Set)
s0(Situation) :-
    setof(S, fluent(S), Situation).

% Take a list of Actions and execute them
execute_process(S1, [], S1). % base case process is empty
% execute an action in process one by one
execute_process(S1, [Action|Process], S2) :-
    poss(Action, S1), % Ensure valid Process
    result(S1, Action, Sd),
    execute_process(Sd, Process, S2).

% Does a fluent hold (is true) in the Situation?
% This is the query mechanism for Situations
% ord_memberchk(+Element, +OrdSet)
% True if Element is a member of OrdSet, compared using ==. 
% Note that enumerating elements of an ordered set can be done using member/2.
% Use-case 1: check a known fluent
% ground(@Term)
% True if Term holds no free variables
holds(Fluent, Situation) :-
    ground(Fluent), ord_memberchk(Fluent, Situation), !.
% Use-case 2: search for a fluent
holds(Fluent, Situation) :-
    member(Fluent, Situation).

% Utility to replace a fluent in the Situation
% ord_del_element: Delete an element from an ordered set. 
% This is the same as ord_subtract(Set, [Element], NewSet).
replace_fluent(S1, OldEl, NewEl, S2) :-
    ord_del_element(S1, OldEl, Sd),
    ord_add_element(Sd, NewEl, S2).


% Robot Possible Action List defined as rules :
%  - goTo(Origin, Destination)
%  - chart_course
%  - fix_wiring
%  - clean_filter
%  - start_reactor
%  - reboot_wifi
%  - unlock(Room1-Room2)
%  - lock(Room1-Room2)


poss(goto(L), S) :-
    % If bluedude is in X and the door is unlocked
    holds(location(bluedude, X), S),
    (   holds(door(X-L, unlocked), S)
    ;   holds(door(L-X, unlocked), S)
    ).

poss(unlock(R1-R2), S) :-
    % Can unlock door between R1 and R2
    % Door is locked
    holds(door(R1-R2, locked), S),
    % Holding the key to the room
    holds(holding(R2-key), S),
    % Located in one of the rooms
    (   holds(location(bluedude, R1), S)
    ;   holds(location(bluedude, R2), S)
    ).
poss(lock(R1-R2), S) :-
    % Can lock door R1-R2
    % Only if it's locked, bluedude has the key
    % and is in one of the rooms
    holds(door(R1-R2, unlocked), S),
    holds(holding(R2-key), S),
    (   holds(location(bluedude, R1), S)
    ;   holds(location(bluedude, R2), S)
    ).


poss(chart_course, S) :-
    % must be in correct location in order to complete goal
    holds(location(bluedude, navigation), S).

poss(fix_wiring, S) :-
    % must be in correct location in order to complete goal
    holds(location(bluedude, electrical), S).

poss(clean_filter, S) :-
    % must be in correct location in order to complete goal
    holds(location(bluedude, o2), S).

poss(start_reactor, S) :-
    % must be in correct location in order to complete goal
    holds(location(bluedude, reactor), S).

poss(reboot_wifi, S) :-
    % must be in correct location in order to complete goal
    holds(location(bluedude, communications), S).


% Results of performing action listed below

result(S1, goto(L), S2) :-
    % bluedude moves
    holds(location(bluedude, X), S1),
    replace_fluent(S1, location(bluedude, X),
                   location(bluedude, L), Sa),
    % If bluedude is carrying something, it moves too
    dif(Item, nothing),
    (
        holds(holding(Item), S1),
        replace_fluent(Sa, location(Item, X),
                       location(Item, L), S2)
    ;   \+ holds(holding(Item), S1),
        S2 = Sa
    ).

result(S1, unlock(R1-R2), S2) :-
    % Door R1-R2 is unlocked
    replace_fluent(S1, door(R1-R2, locked),
                       door(R1-R2, unlocked), S2).
result(S1, lock(R1-R2), S2) :-
    % Door R1-R2 is locked
    replace_fluent(S1, door(R1-R2, unlocked),
                   door(R1-R2, locked), S2).

result(S1, chart_course, S2) :-
    % state is changed to true
    replace_fluent(S1, chart(course, false),
                   chart(course, true), S2).

result(S1, fix_wiring, S2) :-
    % state is changed to true
    replace_fluent(S1, fix(wiring, false),
                   fix(wiring, true), S2).

result(S1, clean_filter, S2) :-
    % state is changed to true
    replace_fluent(S1, clean(filter, false),
                   clean(filter, true), S2).

result(S1, start_reactor, S2) :-
    % state is changed to true
    replace_fluent(S1, start(reactor, false),
                   start(reactor, true), S2).

result(S1, reboot_wifi, S2) :-
    % state is changed to true
    replace_fluent(S1, reboot(wifi, false),
                   reboot(wifi, true), S2).




% Define goal module
:- module(goal, [goal/1, goal_situation/1, reached_goal/2]).

% Make goal procedure dynamic
:- dynamic goal/1.

%goal(location(robbie,shields)).
%goal(clean(filter, true)).
%goal(door(garage-car, locked)).
%goal(door(kitchen-garage, locked)).
% Everything must be tidied away
%goal(location(X, L)) :- fact(home(X, L)).
%%goal(clean(car, true)).
goal(holding(nothing)).

% The Goal Situation is the (ordered) set of fluents that
% describe a goal
goal_situation(S) :-
    setof(G, goal(G), S).

% Test to see if Situation satifies the Goal
% Note that the Situation can contain fluents
% not described in Goal
reached_goal(GoalSituation, Situation) :-
    ord_subtract(GoalSituation, Situation, []). % [] -> no goals not in Situation

% Checks if something is a proper list,
% with a bonus use of generating lists
% of increasing length
list([]).
list([_|T]) :-
    list(T).

iterative_deepening_search(Process) :-
    s0(S0),
    goal_situation(GoalSituation),
    % generate a list (increasing length via failure)
    list(Process),
    % generate a solution
    execute_process(S0, Process, Result),
    % test solution
    reached_goal(GoalSituation, Result).

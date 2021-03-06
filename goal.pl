% Define goal module
:- module(goal, [goal/1, goal_situation/1, reached_goal/2]).

% Make goal procedure dynamic
:- dynamic goal/1.

%possible goals
%goal(clean(filter,true)). 
%goal(chart(course,true)). 
%goal(fix(wiring,true)). 
%goal(start(reactor,true)). 
%goal(reboot(wifi,true)).
%goal(location(bluedude,weapons)).
goal(holding(nothing)).

% The Goal Situation is the (ordered) set of fluents that
% describe a goal
goal_situation(S) :-
    setof(G, goal(G), S).

% Test to see if Situation satifies the Goal
% Note that the Situation can contain fluents
% not described in Goal
% ord_subtract(+InOSet, +NotInOSet, -Diff)
% Diff is the set holding all elements of InOSet that are not in NotInOSet.
reached_goal(GoalSituation, Situation) :-
    ord_subtract(GoalSituation, Situation, []). % [] -> no goals not in Situation

% Checks if something is a proper list,
% with a bonus use of generating lists
% of increasing length
list([]).
list([_|T]) :-
    list(T).


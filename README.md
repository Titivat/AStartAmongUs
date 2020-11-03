# AStartAmongUs

To run prolog code: 
Consult all three .pl files: world.pl, goal.pl, astar.pl

To set the task for the robot to do: 
add goals to the goal.pl file
goals available to robbot: 
goal(clean(filter,true)). 
goal(chart(course,true)).
goal(fix(wiring,true)).
goal(start(reactor,true)).
goal(reboot(wifi,true)).

Robot always starts at the cafeteria and stops when completes all goals

To find the shortest path the robot takes run query: 
a_star(S, P).
example input defined in goal.pl: 
goal(clean(filter, true)).
example output:
S = [holding(nothing), chart(course, false), clean(filter, true), door(cafeteria-centerhallway, unlocked), door(cafeteria-northeasthallway, unlocked), door(cafeteria-northwesthallway, unlocked), door(centerhallway-admin, unlocked), door(... - ..., unlocked), door(..., ...)|...],
P = [goto(northeasthallway), goto(weapons), goto(easthallway), goto(o2), clean_filter]
S is the list of states that the robot went through
P is the list of actions that robot made

Actions that robot can to:
goto(X), chart_course, fix_wiring, clean_filter, start_reactor, reboot_wifi




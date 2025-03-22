#EXP-17 Sum the Integers from 1 to N
% Define a rule to calculate sum from 1 to N
sum_to_n(0, 0).  % Base case: sum of 0 is 0
sum_to_n(N, Sum) :- 
    N > 0,        % Ensure N is positive
    N1 is N - 1,  % Decrease N by 1
    sum_to_n(N1, Sum1),  % Recursively calculate sum up to N-1
    Sum is Sum1 + N.     % Add N to the result

% Sample Queries:
% ?- sum_to_n(5, Sum).
% Sum = 15 (1 + 2 + 3 + 4 + 5 = 15)
% ?- sum_to_n(3, Sum).
% Sum = 6 (1 + 2 + 3 = 6)

#EXP-18 Database with Name and DOB
% Facts: person(Name, DOB)
person(john, '1990-05-15').
person(mary, '1985-09-22').
person(alex, '1995-03-10').

% Rule to query person's DOB
get_dob(Name, DOB) :- 
    person(Name, DOB).

% Sample Queries:
% ?- get_dob(john, DOB).
% DOB = '1990-05-15'
% ?- get_dob(mary, DOB).
% DOB = '1985-09-22'
% ?- person(Name, '1995-03-10').
% Name = alex

#EXP-19 Student-Teacher-Subject Code Database
% Facts: teaches(Teacher, SubjectCode), enrolled(Student, SubjectCode)
teaches(mr_smith, cs101).
teaches(ms_jones, math202).
enrolled(john, cs101).
enrolled(mary, math202).
enrolled(alex, cs101).

% Rule: student_teacher(Student, Teacher, SubjectCode)
student_teacher(Student, Teacher, SubjectCode) :-
    enrolled(Student, SubjectCode),
    teaches(Teacher, SubjectCode).

% Sample Queries:
% ?- student_teacher(john, Teacher, Subject).
% Teacher = mr_smith, Subject = cs101
% ?- student_teacher(Student, ms_jones, Subject).
% Student = mary, Subject = math202

#EXP-20 Planets Database
% Facts: planet(Name, DistanceFromSun, Moons)
planet(mercury, 58, 0).
planet(venus, 108, 0).
planet(earth, 150, 1).
planet(mars, 228, 2).

% Rule to query planet details
planet_info(Name, Distance, Moons) :-
    planet(Name, Distance, Moons).

% Sample Queries:
% ?- planet_info(earth, Distance, Moons).
% Distance = 150, Moons = 1
% ?- planet_info(Name, 228, Moons).
% Name = mars, Moons = 2

#EXP-21 Towers of Hanoi
% Rule to solve Towers of Hanoi
hanoi(1, Source, Target, _) :-
    write('Move disk from '), write(Source), write(' to '), write(Target), nl.
hanoi(N, Source, Target, Aux) :-
    N > 1,
    N1 is N - 1,
    hanoi(N1, Source, Aux, Target),  % Move N-1 disks to auxiliary
    hanoi(1, Source, Target, Aux),   % Move the largest disk to target
    hanoi(N1, Aux, Target, Source).  % Move N-1 disks from auxiliary to target

% Sample Query:
% ?- hanoi(3, 'A', 'C', 'B').
% Move disk from A to C
% Move disk from A to B
% Move disk from B to C
% Move disk from A to C
% Move disk from B to A
% Move disk from C to B
% Move disk from A to C
% true

#EXP-22 Bird Fly or Not
% Facts: bird(Name, CanFly)
bird(eagle, yes).
bird(penguin, no).
bird(sparrow, yes).
bird(ostrich, no).

% Rule to check if a bird can fly
can_fly(Bird) :- 
    bird(Bird, yes),
    write(Bird), write(' can fly'), nl.
can_fly(Bird) :- 
    bird(Bird, no),
    write(Bird), write(' cannot fly'), nl.

% Sample Queries:
% ?- can_fly(eagle).
% eagle can fly
% true
% ?- can_fly(penguin).
% penguin cannot fly
% true
% ?- can_fly(Bird).
% eagle can fly
% Bird = eagle ;
% penguin cannot fly
% Bird = penguin ;
% sparrow can fly
% Bird = sparrow ;
% ostrich cannot fly
% Bird = ostrich

#EXP-23 Family Tree
% Facts: parent(Parent, Child), male(Person), female(Person)
parent(john, mary).
parent(john, peter).
parent(mary, ann).
male(john).
male(peter).
female(mary).
female(ann).

% Rule: grandparent
grandparent(GP, GC) :- 
    parent(GP, P), 
    parent(P, GC).

% Rule: sibling
sibling(X, Y) :- 
    parent(P, X), 
    parent(P, Y), 
    X \= Y.

% Sample Queries:
% ?- grandparent(john, ann).
% true
% ?- sibling(mary, peter).
% true
% ?- parent(john, Child).
% Child = mary ;
% Child = peter

#EXP-24 Dieting System Based on Disease
% Facts: diet(Disease, RecommendedFood)
diet(diabetes, 'low sugar fruits').
diet(hypertension, 'low salt veggies').
diet(obesity, 'high fiber grains').

% Rule to suggest diet
suggest_diet(Disease, Food) :-
    diet(Disease, Food),
    write('For '), write(Disease), write(', eat: '), write(Food), nl.

% Sample Queries:
% ?- suggest_diet(diabetes, Food).
% For diabetes, eat: low sugar fruits
% Food = 'low sugar fruits'
% ?- suggest_diet(hypertension, Food).
% For hypertension, eat: low salt veggies
% Food = 'low salt veggies'

#EXP-25 Monkey Banana Problem
% Facts: initial state
at(monkey, floor).
at(box, floor).
at(banana, ceiling).
can_climb(monkey, box).
can_push(monkey, box).

% Rules: actions to reach banana
get_banana :-
    at(monkey, floor),
    at(box, floor),
    write('Monkey pushes box under banana'), nl,
    push_box_under_banana,
    write('Monkey climbs box'), nl,
    climb_box,
    write('Monkey gets banana'), nl.

push_box_under_banana :- 
    at(box, floor), 
    asserta(at(box, under_banana)).

climb_box :- 
    at(box, under_banana), 
    retract(at(monkey, floor)), 
    asserta(at(monkey, box)).

% Sample Query:
% ?- get_banana.
% Monkey pushes box under banana
% Monkey climbs box
% Monkey gets banana
% true

#EXP-26: Fruit and Color with Backtracking
% Facts: fruit_color(Fruit, Color)
fruit_color(apple, red).
fruit_color(banana, yellow).
fruit_color(grape, purple).
fruit_color(apple, green).

% Rule to query fruit and color
find_color(Fruit, Color) :-
    fruit_color(Fruit, Color),
    write('Fruit: '), write(Fruit), write(', Color: '), write(Color), nl.

% Sample Queries:
% ?- find_color(apple, Color).
% Fruit: apple, Color: red
% Color = red ;
% Fruit: apple, Color: green
% Color = green
% ?- find_color(Fruit, yellow).
% Fruit: banana, Color: yellow
% Fruit = banana

#EXP-27: Best First Search Algorithm
% Facts: edge(Node1, Node2, Cost), heuristic(Node, CostToGoal)
edge(a, b, 4).
edge(a, c, 2).
edge(b, d, 5).
edge(c, d, 8).
heuristic(a, 7).
heuristic(b, 6).
heuristic(c, 5).
heuristic(d, 0).

% Best First Search
best_first_search(Start, Goal, Path) :-
    bfs([0-Start], Goal, [], PathRev),
    reverse(PathRev, Path).

bfs([_-Node|Open], Goal, Visited, [Node|Visited]) :- 
    Node = Goal.
bfs([H-Node|Open], Goal, Visited, Path) :-
    findall(Cost-Neigh, (edge(Node, Neigh, C), \+ member(Neigh, Visited), Cost is H + heuristic(Neigh)), NewNodes),
    append(Open, NewNodes), sort(NewNodes, Sorted),
    bfs(Sorted, Goal, [Node|Visited], Path).

% Sample Query:
% ?- best_first_search(a, d, Path).
% Path = [a, c, d]

#EXP-28: Medical Diagnosis
% Facts: symptom(Disease, Symptom)
symptom(flu, fever).
symptom(flu, cough).
symptom(cold, runny_nose).
symptom(cold, cough).

% Rule: diagnose based on symptoms
diagnose(Disease, Symptoms) :-
    findall(S, symptom(Disease, S), DiseaseSymptoms),
    subset(Symptoms, DiseaseSymptoms),
    write('Possible diagnosis: '), write(Disease), nl.

subset([], _).
subset([H|T], List) :- member(H, List), subset(T, List).

% Sample Queries:
% ?- diagnose(Disease, [fever, cough]).
% Possible diagnosis: flu
% Disease = flu
% ?- diagnose(Disease, [runny_nose]).
% Possible diagnosis: cold
% Disease = cold

#EXP-29: Forward Chaining
% Facts
fact(raining).
rule(raining, wet_ground).
rule(wet_ground, slippery).

% Forward chaining to derive new facts
derive :-
    rule(X, Y),
    fact(X),
    \+ fact(Y),
    assertz(fact(Y)),
    write('Derived: '), write(Y), nl,
    derive.
derive.

% Sample Queries:
% ?- derive.
% Derived: wet_ground
% Derived: slippery
% true
% ?- fact(X).
% X = raining ;
% X = wet_ground ;
% X = slippery

#EXP-30: Backward Chaining
% Facts and rules
fact(has_fever).
rule(has_flu, has_fever).
rule(has_flu, has_cough).
rule(is_sick, has_flu).

% Backward chaining to prove a goal
prove(Goal) :-
    fact(Goal),
    write('Proven: '), write(Goal), nl.
prove(Goal) :-
    rule(Goal, Condition),
    prove(Condition).

% Sample Queries:
% ?- prove(is_sick).
% Proven: has_fever
% true
% ?- prove(has_flu).
% Proven: has_fever
% true

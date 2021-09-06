
/*  Code for Project 1, Semester 2, 2021
    Author:         Joshua Carpeggiani
    E-mail:         jcarpeggiani@student.unimelb.edu.au
    Student ID:     999380
    Purpose:        Code for Project 1, Semester 2, 2021. Fillin Puzzle (https://en.wikipedia.org/wiki/Fill-In_(puzzle)).
                    This code allows for a Puzzle specification and a WordList to be input, and the Puzzle will be unified with a solution for that Puzzle.
*/
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).

/*
    puzzle_solution(List1, List2):
    puzzle_solution/2 solves a Fill-in Puzzle.
    Holds true if List1 is a valid puzzle (List of Lists) for WordList as defined here: https://en.wikipedia.org/wiki/Fill-In_(puzzle).
*/
puzzle_solution(Puzzle, WordList) :-
    slots('#', Puzzle, Slots).
    solve(Slots, WordList).


/*
    slots/3 seperates Rows into slots based on Sep, a separator.
    Holds true if Slots is a representation of Rows where each row is cut at Sep.
*/
slots(Sep, Rows, Slots) :-
    transpose(Rows, Columns),
    slice_all(Sep, Rows, RowSlots),
    slice_all(Sep, Columns, ColumnSlots),
    append(RowSlots, ColumnSlots, Slots).

/*
    solve/2 solves a Puzzle by matching slots to the WordList.
    Holds true if Slots is completed with words of WordList.
    TODO: Implement this function.
*/
solve(Slots, WordList) :-
    write(Slots), write(WordList).


/* ----------------------------- Generic Slice Functions --------------------------------- */

/*
    slice_all(Elem, List1, List2):
    slice_all/3 Slices a List of Lists on the Sep separator.
    Holds true if List2 contains List1 with all sub lists cut on separator Elem.
*/
slice_all(_, [], []).

slice_all(Sep, [X|Xs], Sliced) :-
    slice(Sep, X, [], Tmp1),
    include(lengthcheck(1, <), Tmp1, Tmp2),
    slice_all(Sep, Xs, Tmp3),
    append(Tmp2, Tmp3, Sliced).

/*
    lengthcheck(Int, Goal, List):
    lengthcheck/3 is a generic function to check lengths of lists based on an operator.
    Holds true if Goal called on Int, and the Length of List holds.
*/
lengthcheck(Threshold, Op, List) :-
    length(List, ListLength),
    call(Op, Threshold, ListLength).

/*
    slice(Elem, List1, List2, List3):
    slice/4 slices a list on a separator.
    Holds true if List3 is the same as List1, cut on Elem appended with List2.
*/
slice(_, [], Cum, Res) :-
    (Cum == [] ->
        Res = [];
        Res = [Cum]
    ).

slice(Sep, [X|Xs], Cum, [Cum|Sliced]) :-
    nonvar(X), Sep == X,
    slice(Sep, Xs, [], Sliced).

slice(Sep, [X|Xs], Cum, Sliced) :-
    X \== Sep,
    append(Cum, [X], Cum1),
    slice(Sep, Xs, Cum1, Sliced).



:- Puzzle = [[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,#,_,_,_,#,_,_,_,_,_,_],[#,#,#,_,_,_,_,#,_,_,_,#,_,_,_],[_,_,_,_,_,_,#,_,_,_,_,#,#,#,#],[_,_,_,_,_,#,_,_,_,_,_,#,_,_,_],[_,_,_,_,#,_,_,_,_,_,#,_,_,_,_],[_,_,_,#,_,_,_,_,_,#,_,_,_,_,_],[#,#,#,#,_,_,_,_,#,_,_,_,_,_,_],[_,_,_,#,_,_,_,#,_,_,_,_,#,#,#],[_,_,_,_,_,_,#,_,_,_,#,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_]], slots(Puzzle, Slots, '#').
:- Puzzle = [['#',h],[_,_]],slots(Puzzle, Slots, '#').
:- Puzzle = [['#',h,'#'],[_,_,_],['#',_,'#']], WordList = [[h,a,t], [b,a,g]], puzzle_solution(Puzzle, WordList),  write(Puzzle), nl.
%:- WordList = [['A','T','E'],['B','O','X'],['C','U','R'],['D','O','G'],['E','N','D'],['K','I','D'],['M','E','S'],['N','A','N'],['O','N','E'],['P','A','N'],['P','E','T'],['Q','E','D'],['R','I','O'],['R','O','Y'],['S','O','S'],['S','O','T'],['A','L','E','C'],['B','O','E','R'],['B','R','I','E'],['C','O','I','R'],['E','D','N','A'],['E','R','A','S'],['E','R','I','E'],['E','R','N','E'],['E','U','R','O'],['F','A','R','T'],['F','I','G','S'],['G','O','E','S'],['I','N','S','T'],['I','R','A','N'],['I','R','E','S'],['K','A','L','E'],['L','I','A','R'],['N','E','S','T'],['N','I','L','E'],['N','O','S','E'],['O','I','N','K'],['O','R','A','L'],['R','E','N','O'],['R','I','P','E'],['R','O','A','D'],['R','O','O','T'],['S','E','A','L'],['S','N','I','P'],['S','O','N','G'],['S','T','A','R'],['T','A','B','S'],['T','E','E','S'],['T','E','S','T'],['T','I','D','Y'],['U','N','I','T'],['V','E','R','T'],['V','O','L','T'],['X','R','A','Y'],['A','N','O','D','E'],['A','U','N','T','Y'],['A','V','A','S','T'],['C','R','E','N','A'],['D','E','L','E','S'],['E','R','S','E','S'],['G','E','N','I','E'],['G','H','A','N','A'],['I','L','E','U','M'],['I','V','O','R','Y'],['K','N','O','T','S'],['L','I','P','I','D'],['O','A','S','I','S'],['O','R','A','T','E'],['O','R','I','O','N'],['O','X','I','D','E'],['S','C','O','N','E'],['S','C','O','T','S'],['S','L','E','P','T'],['S','N','O','R','E'],['V','E','R','N','E'],['V','I','N','E','S'],['E','X','H','O','R','T'],['I','N','T','U','I','T'],['Q','U','I','R','K','Y'],['T','E','U','T','O','N'],['C','R','I','T','I','C','A','L'],['E','D','G','I','N','E','S','S'],['K','N','I','T','T','I','N','G'],['N','O','I','S','E','T','T','E']], Puzzle = [[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,_,#,_,_,_,_,#,_,_,_,_],[_,_,_,_,#,_,_,_,#,_,_,_,_,_,_],[#,#,#,_,_,_,_,#,_,_,_,#,_,_,_],[_,_,_,_,_,_,#,_,_,_,_,#,#,#,#],[_,_,_,_,_,#,_,_,_,_,_,#,_,_,_],[_,_,_,_,#,_,_,_,_,_,#,_,_,_,_],[_,_,_,#,_,_,_,_,_,#,_,_,_,_,_],[#,#,#,#,_,_,_,_,#,_,_,_,_,_,_],[_,_,_,#,_,_,_,#,_,_,_,_,#,#,#],[_,_,_,_,_,_,#,_,_,_,#,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_],[_,_,_,_,#,_,_,_,_,#,_,_,_,_,_]], puzzle_solution(Puzzle, WordList), write(Puzzle), nl.

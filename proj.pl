/*
Author:   Joshua Carpeggiani <jcarpeggiani@student.unimelb.edu.au>
Purpose:  Code for Project 1, Semester 2, 2021: Fill in puzzle.
*/
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).

puzzle_solution(Puzzle, WordList) :-
    write(Puzzle),
    slots(Puzzle, Slots, '#').

slots(Rows, Slots, Sep) :-
    transpose(Rows, Columns),
    slice_all(Sep, Rows, RowSlots),
    slice_all(Sep, Columns, ColumnSlots),
    append(RowSlots, ColumnSlots, Slots).

slice_all(_, [], []).

slice_all(Sep, [X|Xs], Sliced):-
    slice(Sep, X, [], Tmp1),
    include(lengthcheck(1, <), Tmp1, Tmp2),
    slice_all(Sep, Xs, Tmp3),
    append(Tmp2, Tmp3, Sliced).

lengthcheck(Threshold, Op, List) :-
    length(List, ListLength),
    call(Op, Threshold, ListLength).

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


/*  Code for Project 1, Semester 2, 2021
*    Author:         Joshua Carpeggiani
*    E-mail:         jcarpeggiani@student.unimelb.edu.au
*    Student ID:     999380
*    Purpose:        Code for Project 1, Semester 2, 2021. Fillin Puzzle (https://en.wikipedia.org/wiki/Fill-In_(puzzle)).
*                    This code allows for a Puzzle specification and a WordList to be input,
*                    and the Puzzle will be unified with a solution for that Puzzle.
*/
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).

/*
*    puzzle_solution(-Puzzle: List, +WordList: List):
*    puzzle_solution/2 solves a Fill-in Puzzle.
*    Holds true if List1 is a valid puzzle (List of Lists) for WordList as defined here: https://en.wikipedia.org/wiki/Fill-In_(puzzle).
*
*    Examples:
*
*    ?- Puzzle = [['#',h,'#'],[_,_,_],['#',_,'#']], WordList = [[h,a,t], [b,a,g]], puzzle_solution(Puzzle, WordList).
*    Puzzle = [[#, h, #], [b, a, g], [#, t, #]],
*    WordList = [[h, a, t], [b, a, g]].
*/
puzzle_solution(Puzzle, WordList) :-
    slots('#', 2, Puzzle, Slots),
    unify_slots(Slots, WordList).

/*
*    slots(+Sep: Elem, +Min: Number, +Rows: List, -Slots: List):
*    slots/3 seperates Rows into slots based on Sep, a separator.
*    Holds true if Slots is a representation of Rows where each row is cut at Sep.
*
*    Examples:
*
*    ?- slots('#', 2, [[1, 2, 3, #, 4, 3]], X).
*    X = [[1, 2, 3], [4, 3]].
*/
slots(Sep, Min, Rows, Slots) :-
    transpose(Rows, Columns),
    slice_all(Sep, Min, Rows,    RowSlots),
    slice_all(Sep, Min, Columns, ColumnSlots),
    append(RowSlots, ColumnSlots, Slots).


/* ----------------------------- Generic Slice Functions --------------------------------- */

/*
*    slice_all(+Sep: Elem, +Min: Number, +List1: List, -Sliced: List):
*    slice_all/3 Slices a List of Lists on the Sep separator, with any sliced elem a minimum of Min length otherwise omitted.
*    Holds true if List2 contains List1 with all sub lists cut on separator Elem.
*
*    Examples:
*
*    ?- slice_all('#', 2, [[1, 2, 3, #, 4, 3]], X).
*    X = [[1, 2, 3], [4, 3]].
*/
slice_all(_, _, [], []).
slice_all(Sep, Min, [X|Xs], Sliced) :-
    slice(Sep, X, [], Tmp1),
    include(lengthcheck(Min, =<), Tmp1, Tmp2),
    slice_all(Sep,Min, Xs, Tmp3),
    append(Tmp2, Tmp3, Sliced).

/*
*    lengthcheck(+Threshold: Number, +Op: Goal, +List: List):
*    lengthcheck/3 is a generic function to check lengths of lists based on an operator.
*    Holds true if Goal called on Int, and the Length of List holds.
*
*    Examples:
*
*    ?- lengthcheck(1, <, [1, 2]).
*    true.
*
*    ?- lengthcheck(1, <, [1]).
*    false.
*/
lengthcheck(Threshold, Op, List) :-
    length(List, ListLength),
    call(Op, Threshold, ListLength).

/*
*    slice(+Sep: Elem, +List1: List, +List2: +List, -List3: List):
*    slice/4 slices a list on a separator.
*    Holds true if List3 is the same as List1, cut on Elem appended with List2.
*
*    Examples:
*
*    ?- slice('#', [1, 2, #, 4, 5], [], Out).
*    Out = [[1, 2], [4, 5]].
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


/* ----------------------------- Puzzle Solver Predicates -------------------------------- */

/*
*    unify_slots(-Slots: List, +Words: List):
*    unify_slots/2 unifies Slots in List1 with words in List2.
*    Holds true List1 is unified with List2.
*
*    Examples:
*
*    ?- unify_slots([[B, A, G]], [[b,a,g]]).
*    B = b,
*    A = a,
*    G = g .
*/
unify_slots([], []).
unify_slots(Slots, Words) :-
    unify_slot(Slots, Words, Best, Word),
    exclude(==(Word), Words, WordsLeft),
    exclude(==(Best), Slots, SlotsLeft),
    unify_slots(SlotsLeft,   WordsLeft).


/*
*    unify_slot(-Slots: List, +Words: List, -Slot: List, -Word: List):
*    unify_slot/4 Slot is a unification with Word.
*    Holds true if Slot and Word are unified from Slots and Words.
*
*    Example:
*
*    ?- unify_slot([[B, A, G], [X, Y, Z]], [[b,a,g]], Best, Word).
*    B = b,
*    A = a,
*    G = g,
*    Best = Word, Word = [b, a, g] .
*/
unify_slot([S|Ss], Words, Best, Word) :-
    slot_perms(S, Words, 0, PermCount),
    best_slot(Ss, S, Words, PermCount, Best),
    exclude(\=(Best), Words, Perm),
    member(Word, Perm),
    Best = Word.


/*
*    slot_perms(-Slot: List, +Words: List, +Cummulative: Number, -PermutationCount: Number).
*    slot_perms/4 counts the amount words that can fit in Slot into PermutationCount.
*    Holds true if Permutation count is the amount of Words that can fit in Slot.
*
*    Examples:
*
*    ?- slot_perms([A,B,C], [[h,a,t],[b,a,g]], 0, PermCount).
*    PermCount = 0+1+1 .
*/
slot_perms(_, [], Cum, Cum).
slot_perms(Slot, [W| Ws], Cum, PermCount) :-
    (Slot \= W ->
        slot_perms(Slot, Ws, Cum  , PermCount);
        slot_perms(Slot, Ws, Cum+1, PermCount)
    ).

/*
*    best_slot(+Words: List, +Permutations: Number, +Slots: List, +CurrentBest: Number, -Best: List).
*    best_slot/5 selects the best Slot into Best from Slots given a list of Words.
*    Holds true if Best is the Best selection of Words from Slots.
*
*    Examples:
*
*    ?- best_slot([[A,B,C]], [A, B, C], [[h,a,t],[b,a,g]], 0, Best).
*    Best = [A, B, C].
*/
best_slot([], Best, _, _, Best).
best_slot([S|Ss], CurrentBest, Words, PermCount, Best):-
   slot_perms(S, Words, 0, NewPermCount),
   (NewPermCount < PermCount->
       best_slot(Ss, S,           Words, NewPermCount, Best);
       best_slot(Ss, CurrentBest, Words, PermCount,    Best)
   ).

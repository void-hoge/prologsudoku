num(1).
num(2).
num(3).
num(4).
num(5).
num(6).
num(7).
num(8).
num(9).
blank(0).

print_cell(C) :- blank(C), write('  '), !.
print_cell(C) :- num(C), write(C), write(' '), !.


print_row([]) :- !.
print_row([Cell|Row]) :-
    print_cell(Cell),
    print_row(Row), !.

print_grid([]) :- !.
print_grid([Row|Grid]) :-
    print_row(Row),
    writeln(''),
    print_grid(Grid), !.

solved_row([]) :- !.
solved_row([Cell|Row]) :-
    num(Cell),
    solved_row(Row), !.

solved_grid([]) :- !.
solved_grid([Row|Grid]) :-
    solved_row(Row),
    solved_grid(Grid).

blank_row([Cell|_], T, T) :-
    blank(Cell), !.
blank_row([Cell|Row], T, C) :-
    num(Cell),
    T_ is T + 1,
    blank_row(Row, T_, C), !.

blank_grid([Row|_], T, T, C) :-
    blank_row(Row, 0, C), !.
blank_grid([_|Grid], T, R, C) :-
    T_ is T + 1,
    blank_grid(Grid, T_, R, C), !.

pick([N|_], 0, N) :- !.
pick([_|L], I, N) :-
    I_ is I - 1,
    pick(L, I_, N), !.

row(Grid, R, Row) :-
    pick(Grid, R, Row), !.

col([], _, []) :- !.
col([Row|Grid], C, [Cell|Col]) :-
    pick(Row, C, Cell),
    col(Grid, C, Col), !.

slice(_, 0, 0, []) :- !.
slice([_|Src], Begin, End, Dst) :-
    Begin > 0,
    Begin_ is Begin - 1,
    End_ is End - 1,
    slice(Src, Begin_, End_, Dst), !.
slice([N|Src], 0, End, [N|Dst]) :-
    End > 0,
    End_ is End - 1,
    slice(Src, 0, End_, Dst), !.

block_grid([], _, []).
block_grid([Row|Grid], Cbegin, Block_) :-
    Cend is Cbegin + 3,
    slice(Row, Cbegin, Cend, BlockRow),
    block_grid(Grid, Cbegin, Block),
    append(BlockRow, Block, Block_), !.

block(Grid, Rbegin, Cbegin, Block) :-
    Rend is Rbegin + 3,
    slice(Grid, Rbegin, Rend, GridSlice),
    block_grid(GridSlice, Cbegin, Block), !.

notmembers(0, _, []) :- !.
notmembers(T, L, A) :-
    member(T, L),
    T_ is T - 1,
    notmembers(T_, L, A), !.
notmembers(T, L, [T|A]) :-
    T_ is T - 1,
    notmembers(T_, L, A), !.

candidates(Grid, R, C, Cands) :-
    row(Grid, R, Row),
    col(Grid, C, Col),
    RB is R // 3 * 3,
    CB is C // 3 * 3,
    block(Grid, RB, CB, Block),
    append(Row, Col, L_),
    append(L_, Block, List),
    notmembers(9, List, Cands), !.

placed_row([_|Row], 0, Cand, [Cand|Row]).
placed_row([Cell|Row], C, Cand, [Cell|Placed]) :-
    C_ is C - 1,
    placed_row(Row, C_, Cand, Placed), !.

placed_grid([Row|Grid], 0, C, Cand, [PlacedRow|Grid]) :-
    placed_row(Row, C, Cand, PlacedRow).
placed_grid([Row|Grid], R, C, Cand, [Row|Placed]) :-
    R_ is R - 1,
    placed_grid(Grid, R_, C, Cand, Placed), !.

solve(Grid, Grid) :-
    solved_grid(Grid),
    print_grid(Grid), !.
solve(Grid, Ans) :-
    blank_grid(Grid, 0, R, C),
    candidates(Grid, R, C, Cands),
    member(Cand, Cands),
    placed_grid(Grid, R, C, Cand, Placed),
    solve(Placed, Ans), !.

problem1(
    [
        [0, 2, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 5, 1, 0, 0, 0, 0, 4],
        [0, 0, 0, 0, 0, 0, 0, 0, 7],
        [0, 0, 4, 0, 0, 0, 0, 7, 5],
        [0, 0, 0, 0, 8, 0, 0, 0, 0],
        [0, 3, 0, 0, 2, 9, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 7, 5, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 3, 9, 2, 0]
    ]
).

problem2(
    [
        [2, 5, 3, 0, 0, 0, 9, 0, 8],
        [0, 0, 0, 0, 8, 0, 0, 0, 6],
        [0, 0, 7, 0, 0, 0, 0, 4, 0],
        [0, 7, 0, 0, 0, 0, 0, 0, 0],
        [0, 2, 4, 0, 1, 5, 6, 0, 0],
        [0, 1, 8, 0, 0, 7, 3, 0, 0],
        [0, 0, 0, 6, 0, 0, 7, 1, 0],
        [0, 3, 6, 8, 0, 0, 2, 0, 0],
        [0, 0, 0, 5, 7, 0, 0, 0, 0]
    ]
).

test :-
    problem2(Grid),
    solve(Grid, _).

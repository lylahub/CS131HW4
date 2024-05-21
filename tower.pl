% ntower helpers
row_length(T, N) :-
    length(T, N).

col_length([], _).
col_length([HD | TL], N) :-
    length(HD, N),
    col_length(TL, N).

domain([], _).
domain([HD | TL], N) :-    
    fd_domain(HD, 1, N),
    domain(TL, N).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% transpose 
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

check_edge([], []).
check_edge([HD | TL], [VHD | VTL]) :-
    check_list(HD, VHD),
    check_edge(TL, VTL).

check_edge_rev([], []).
check_edge_rev([HD | TL], [VHD | VTL]) :-
    reverse(HD, RHD),
    check_list(RHD, VHD),
    check_edge_rev(TL, VTL).

check_list(L, V) :-
    count_visible(L, Count, 0),
    V #= Count.
    
count_visible([], 0, _).
count_visible([HD | TL], Count, Max) :-
    HD #> Max,
    count_visible(TL, NewCount, HD),
    Count is NewCount+1.
count_visible([HD | TL], Count, Max) :-
    HD #< Max,
    count_visible(TL, Count, Max).


% ntower
ntower(N, T, C) :-
    row_length(T, N),
    col_length(T, N),
    domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    C = counts(Top, Bot, Left, Right),
    check_edge(TT, Top),
    check_edge_rev(TT, Bot),
    check_edge(T, Left),
    check_edge_rev(T, Right).


% plain_ntower helpers
check_row(_, [], [], []).
check_row(N, [HD | TL], [FHD | FTL], [BHD | BTL]) :-
    length(HD, N),
    maplist(between(1, N), HD),
    check_unique(HD),
    count_visible_plain(HD, 0, 0, FHD),
    reverse(HD, RHD),
    count_visible_plain(RHD, 0, 0, BHD),
    check_row(N, TL, FTL, BTL).
    
check_unique(L):-
    sort(L, Sorted),
    length(L, Len1),
    length(Sorted, Len2),
    Len1 == Len2.
    
count_visible_plain([], Acc, _, Count) :- Count is Acc.
count_visible_plain([HD | TL], Acc, Max, Count) :-
    HD > Max,                                 
    NewAcc is Acc+1,
    count_visible_plain(TL, NewAcc, HD, Count).
count_visible_plain([HD | TL], Acc, Max, Count) :-
    HD < Max,                                                                  
    count_visible_plain(TL, Acc, Max, Count).


% plain_ntower
plain_ntower(N, T, C) :-
    row_length(T, N),
    C = counts(Top, Bot, Left, Right),
    check_row(N, T, Left, Right),
    transpose(T, TT),
    check_row(N, TT, Top, Bot).


% ambiguous    
ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C),
    ntower(N, T2, C),
    T1 \= T2.


% Tests
test_ntower(T) :-
    statistics(cpu_time, [Start | _]),
    ntower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

test_plain_ntower(T) :-
    statistics(cpu_time, [Start | _]),
    plain_ntower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).     


% speedup
speedup(Ratio) :-
    test_ntower(T1),
    test_plain_ntower(T2),
    Ratio is T2/T1.
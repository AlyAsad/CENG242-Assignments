:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_vote_wasted(City, PP) :- not(elected(City, PP, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_candidate_elected(Name, PP) :- 
    candidate(Name, PP, City, Row),
    elected(City, PP, Final),
    Row =< Final.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

candidate_count_from_city([], _, 0).

candidate_count_from_city([H | T], City, Count) :-
    candidate(H, _, City, _),
    candidate_count_from_city(T, City, R),
    Count is R + 1.
    
candidate_count_from_city([H | T], City, Count) :-
    not(candidate(H, _, City, _)),
    candidate_count_from_city(T, City, Count).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_parties(PPs) :- findall(Party, party(Party, _), PPs).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_candidates_from_party(PP, Cands) :- findall(Cand, candidate(Cand, PP, _, _), Cands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_elected_from_party(PP, Cands) :- findall(Cand, is_candidate_elected(Cand, PP), Cands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

election_rate(PP, Perc) :-
    all_candidates_from_party(PP, All),
    all_elected_from_party(PP, Elec),
    length(All, AllL),
    length(Elec, ElecL),
    Perc is ElecL/AllL.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

council_percentage(PP, Perc) :-
    all_elected_from_party(PP, Elec),
    length(Elec, ElecL),
    to_elect(AllL),
    Perc is ElecL/AllL.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%helpers

%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotate(L, R) :-
    append(Left, Right, L),
    append(Right, Left, R),
    Right \= [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%

checker([Curr | _], Work, Eligible) :- 
    not(member(Curr, Work)),
    Eligible = [Curr].

%%%%%%%%%%%%%%%%%%%%%%%%%%%

mainHelper([], Final, Final).

mainHelper([Curr | Tail], Work, Final) :-
    party(PP, Curr),
    all_candidates_from_party(PP, Cands),
    rotate(Cands, Permuted),
    checker(Permuted, Work, Eligible),
    append(Work, Eligible, NewWork),
    mainHelper(Tail, NewWork, Final).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alternative_debate_setups(Desc, Cands) :-
    string_chars(Desc, Chars),
    mainHelper(Chars, [], Cands).
    



























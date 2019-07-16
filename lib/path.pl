%%%% pathname operations


%% return canonical path (with "..", extra slashes and "." removed),
%% as absolute path. All predicates accept strings or atoms

canonical_path(IN, OUT) :-
	absolute_pathname(IN, ALST),
	split_string(ALST, "/", "", OUTLST),
	canonicalize_pathname(OUTLST, CLST),
	append(CLST, LST),
	atom_codes(OUT, LST).

canonicalize_pathname([], []).
canonicalize_pathname(["."|R], R2) :- canonicalize_pathname(R, R2).
canonicalize_pathname([X, ".."|R], R2) :- canonicalize_pathname(R, R2).
canonicalize_pathname([X|R], [[47|X]|R2]) :- canonicalize_pathname(R, R2).

%% make pathname absolute (relative to PWD)

absolute_pathname(FNAME, ANAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	absolute_pathname(FLST, ANAME).
absolute_pathname([47|R], [47|R]).
absolute_pathname(LST, OLST) :-
	getcwd(PWD),
	atom_codes(PWD, PLST),
	append([PLST, "/", LST], OLST).

%% filename without directory part

basename(FNAME, BNAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	basename(FLST, BNAME).
basename(FNAME, BNAME) :-
	append(_, BNAME, FNAME),
	\+memberchk(47, BNAME), !.

%% directory part of filename

dirname(FNAME, DNAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	dirname(FLST, DNAME).
dirname(FNAME, DNAME) :-
	append(DNAME, [47|R], FNAME),
	\+memberchk(47, R), !.

%% file-extension only, fails if filename has no extension

suffix(FNAME, SUF) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	suffix(FLST, SUF).
suffix(FNAME, SUF) :-
	append(_, SUF, FNAME),
	\+memberchk(46, SUF), !,
	FNAME \== SUF.

%% strip file extension

strip_suffix(FNAME, RNAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	strip_suffix(FLST, RNAME).
strip_suffix(FNAME, RNAME) :-
	append(RNAME, [46|R], FNAME),
	\+memberchk(46, R).
strip_suffix(FNAME, FNAME).

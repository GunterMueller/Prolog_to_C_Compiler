main :-
	loop(10000).

loop(N) :-
	between(1, N, _),
	spin,
	fail.
loop(_).

spin :-
	between(1, 100, I),
	sym(I, A),
	jump(A),
	fail.

sym(I, A) :-
	number_codes(I, IL),
	append("a", IL, AL),
	atom_codes(A, AL),
	!.

jump(a1).
jump(a2).
jump(a3).
jump(a4).
jump(a5).
jump(a6).
jump(a7).
jump(a8).
jump(a9).
jump(a10).
jump(a11).
jump(a12).
jump(a13).
jump(a14).
jump(a15).
jump(a16).
jump(a17).
jump(a18).
jump(a19).
jump(a20).
jump(a21).
jump(a22).
jump(a23).
jump(a24).
jump(a25).
jump(a26).
jump(a27).
jump(a28).
jump(a29).
jump(a30).
jump(a31).
jump(a32).
jump(a33).
jump(a34).
jump(a35).
jump(a36).
jump(a37).
jump(a38).
jump(a39).
jump(a40).
jump(a41).
jump(a42).
jump(a43).
jump(a44).
jump(a45).
jump(a46).
jump(a47).
jump(a48).
jump(a49).
jump(a50).
jump(a51).
jump(a52).
jump(a53).
jump(a54).
jump(a55).
jump(a56).
jump(a57).
jump(a58).
jump(a59).
jump(a60).
jump(a61).
jump(a62).
jump(a63).
jump(a64).
jump(a65).
jump(a66).
jump(a67).
jump(a68).
jump(a69).
jump(a70).
jump(a71).
jump(a72).
jump(a73).
jump(a74).
jump(a75).
jump(a76).
jump(a77).
jump(a78).
jump(a79).
jump(a80).
jump(a81).
jump(a82).
jump(a83).
jump(a84).
jump(a85).
jump(a86).
jump(a87).
jump(a88).
jump(a89).
jump(a90).
jump(a91).
jump(a92).
jump(a93).
jump(a94).
jump(a95).
jump(a96).
jump(a97).
jump(a98).
jump(a99).
jump(a100).

/****************************************************
% File Name: pronto_morph_engine.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation

% Morphological Analyzer to be used with
% ProNTo (Prolog Natural Language Toolkit),
% created at the Artificial Intelligence Center
% of The University of Georgia

% Modified from: POEM.PL - Part Of English Morphology
% M. Covington
% 2003 February 12
****************************************************/



% How inflectional suffixes are named:
%
% - All are marked with the prefix operator -/1.
%   (In Prolog, -x is equivalent to -(x).)
%   This makes it easy to distinguish suffixes
%   from words.
%
% - The morphological analyzer makes the distinctions
%   that it can make without a lexicon.
%   We use the ambiguous symbols -s and -ed for morphemes
%   that cannot be unambiguously identified.
%
% -s     Suffix -s, could be either noun plural or verb 3sg
% -pl    Suffix definitely denoting a noun plural (e.g., oxen = ox -pl)
% -sg3   Suffix definitely denoting a verb 3rd person singular (e.g., has = have -sg3)
%
% -ing   Verb ending, never ambiguous
%
% -ed    Suffix -ed, denoting the past or -en form of a regular verb
% -past  Suffix definitely denoting a verb past tense form (e.g., ran = run -past)
% -en    Suffix definitely denoting an -en form of a verb  (e.g., eaten = eat -en)
%
% -er    Suffix denoting the comparative form of the adjective or adverb
% -est   Suffix denoting the superlative form of the adjective or adverb




% morph_tokens(+Tokens,-List)
%  Converts the output of pronto_morph_tokenizer.pl (also et.pl) to a list of morphemes.
%   (i.e. w([t,e,s,t,i,n,g] --> [[test,-ing]]
%  OR
%   (i.e. [w([t,e,s,t,i,n,g])] -->  [[test,-ing]]
%  OR
%   (i.e. [w([t,e,s,t,i,n,g]),w([i,t])] -->  [[test,-ing],[it]]
 
morph_tokens([w(Chars)|Tokens],Morphs) :-    % handles list of token(s) as input
   !,
   morph(Chars,Rest,Morphs),    
   morph_tokens(Tokens,Rest).

morph_tokens([_|Tokens],Morphs) :-           % handles list of token(s) as input
   % numeric or special-character token
   morph_tokens(Tokens,Morphs).

morph_tokens(Token,Morphs) :-                % handles single token as input
   \+ is_list(Token),
   morph_tokens([Token],Morphs).

morph_tokens([],[]).


% morph_tokens_bag(+Tokens,-List)
%  Same as morph_tokens/2 except that it returns every alternative
%  analysis in a list of lists
%   (i.e. )
%   [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]] 

morph_tokens_bag(Token,List) :-
   \+ is_list(Token),
   findall(Alternative,
	   morph_tokens(Token,Alternative),
	   List).
morph_tokens_bag([First|RestTokens],[List|RestList]) :-
   findall(Alternative ,
	   morph_tokens(First,Alternative),
	   List),
   morph_tokens_bag(RestTokens,RestList).
morph_tokens_bag([],[]).


% morph_atoms(+AtomWord,-List)
%  Converts an atom to a list of morphemes
%   (i.e. testing --> [[test,-ing]]
%  OR
%   (i.e. [testing] --> [[test,-ing]]
%  OR
%  Converts a list of atoms to a list of morpheme lists
%   (i.e. [testing,one,two,three] --> [[test,-ing],[one],[two],[three]]

morph_atoms([AtomWord|Rest],List) :-	   % handles list of atom(s) as input
   atom_chars(AtomWord,RawList),
   morph(RawList,RestResult,List),
   morph_atoms(Rest,RestResult).

morph_atoms(SingleAtom,List) :-            % handles single atoms as input
   \+ is_list(SingleAtom),
   morph_atoms([SingleAtom],List).

morph_atoms([],[]).


% morph_atoms_bag(+Atoms,-List)
%  Same as morph_atoms/2 except that it returns evey alternative
%  analysis in a list of lists
%  i.e.)
%  [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]]

morph_atoms_bag(Token,List) :-
   \+ is_list(Token),
   findall(Alternative,
	   morph_atoms(Token,Alternative),
	   List).
morph_atoms_bag([First|RestTokens],[List|RestList]) :-
    findall(Alternative ,
	    morph_atoms(First,Alternative),
	    List),
    morph_atoms_bag(RestTokens,RestList).
morph_atoms_bag([],[]).



% morph(+Characters,-List)
%  Converts a list of characters to a list of morphemes.
%   (i.e. [r,u,n,n,i,n,g] --> [run,-ing]
%  OR
%  Converts a list of character lists to a list of morpheme lists
%   (i.e. [[r,u,n,n,i,n,g],[f,a,s,t,e,r]] --> [[run,-ing],[fast,-er]]

morph_chars([Chars|Rest],List) :-          % handles list of character lists as input
   is_list(Chars),
   morph(Chars,RestResult,List),
   morph_chars(Rest,RestResult).

morph_chars([Chars|Rest],List) :-          % handles a single list of characters as input
   \+ is_list(Chars),
   morph_chars([[Chars|Rest]],List).

morph_chars([],[]).


% morph_chars_bag(+Tokens,-List) :-
%  Same as morph_chars/2 except that it returns every alternative
%  analysis as a list of lists
% i.e.
% [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]] 

morph_chars_bag([C|CharList],List) :-
    \+ is_list(C),
    findall(Alternatives,
	    morph_chars(CharList,Alternatives),
	    List).
morph_chars_bag([First|RestTokens],[List|RestList]) :-
    findall(Alternatives,
	    morph_chars(First,Alternatives),
	    List),
    morph_chars_bag(RestTokens,RestList).
morph_chars_bag([],[]).

% morph(+Characters,-Tail,-OpenList)
%  Like morph/2, but creates an open list ending with Tail.
%  This is where the real work is done.

morph(Chars,Tail,[[Root,Suffix]|Tail]) :-
   atom_chars(Atom,Chars),                   % quicker to look up an atom than a list
   irregular_form(Atom,Tail,[Root,Suffix|Tail]),         % check to see if word is irregular
   !.                                        

morph(Chars,Tail,[[Word]|Tail]) :-
   atom_chars(Word,Chars).                   % always an option that word is root

morph(Chars,Tail,[[RootWord,Suffix]|Tail]) :-  % tries to break up word into root and suffix
   find_suffix(Chars,Root,Suffix),
   atom_chars(RootWord,Root).



% find_suffix(+Characters,-Root,-Suffix)
%  Applies split_suffix to a word at all positions.

find_suffix(Chars,Root,Suffix) :-
   split_suffix(Chars,Root,Suffix).

find_suffix([C|Chars],[C|Root],Suffix) :-
   find_suffix(Chars,Root,Suffix).

   


% suffix(?Chars,?Morpheme)
%  If Chars is a suffix, Morpheme is the description of it.
%  Note that the suffix -s is also hard-coded into split_suffix
%  in various places.
%
suffix([s],-s).         
suffix([e,d],-ed).
suffix([i,n,g],-ing).
suffix([e,r],-er).
suffix([e,s,t],-est).


% vowel(?Char)
%  Char is a vowel.
%
vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).
vowel(y).


/****************************************************
% File Name: pronto_morph_irreg_adj.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation
%
% This file is to be used as part of the morphological analyzer
% that is a component of ProNTo (Prolog Natural Language
% Toolkit), made at the Artificial Intelligence Center of
% the University of Georgia (www.ai.uga.edu).
%
% Special thanks:
% 1)To the wordnet project for their list words that are exceptions to the spelling rules.
% 2)To Dr. Covington for his guidence on the project.
******************************************************/

% irregular_form(+Atom,-Tail,-List)
%  Interprets an irregular form (as an open list)


irregular_form( bluer,X,[blue,-er| X ]).
irregular_form( cagier,X,[cagey,-er| X ]).
irregular_form( cagiest,X,[cagey,-est| X ]).
irregular_form( dicier,X,[dicey,-er| X ]).
irregular_form( diciest,X,[dicey,-est| X ]).
irregular_form( dopier,X,[dopey,-er| X ]).
irregular_form( dopiest,X,[dopey,-est| X ]).
irregular_form( eerier,X,[eerie,-er| X ]).
irregular_form( freer,X,[free,-er| X ]).
irregular_form( gooier,X,[gooey,-er| X ]).
irregular_form( gooiest,X,[gooey,-est| X ]).
irregular_form( homier,X,[homey,-er| X ]).
irregular_form( homiest,X,[homey,-est| X ]).
irregular_form( truer,X,[true,-er| X ]).

/****************************************************
% File Name: pronto_morph_irreg_adv.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation
%
% This file is to be used as part of the morphological analyzer
% that is a component of ProNTo (Prolog Natural Language
% Toolkit), made at the Artificial Intelligence Center of
% the University of Georgia (www.ai.uga.edu).
%
% Special thanks:
% 1)To the wordnet project for their list words that are exceptions to the spelling rules.
% 2)To Dr. Covington for his guidence on the project.
******************************************************/


% irregular_form(+Atom,-Tail,-List)
%  Interprets an irregular form (as an open list)

irregular_form( best,X,[well,-est| X ]).
irregular_form( better,X,[well,-er| X ]).
irregular_form( deeper,X,[deeply,-er| X ]).
irregular_form( farther,X,[far,-er| X ]).
irregular_form( further,X,[far,-er| X ]).

/****************************************************
% File Name: pronto_morph_irreg_noun.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation
%
% This file is to be used as part of the morphological analyzer
% that is a component of ProNTo (Prolog Natural Language
% Toolkit), made at the Artificial Intelligence Center of
% the University of Georgia (www.ai.uga.edu).
%
% Special thanks:
% 1)To the wordnet project for their list words that are exceptions to the spelling rules.
% 2)To Dr. Covington for his guidence on the project.
******************************************************/


% irregular_form(+Atom,-Tail,-List)
%  Interprets an irregular form (as an open list)

irregular_form( children,X,[child,-pl| X ]).

% from my adapted wordnet exception files
irregular_form( acciaccature,X,[acciaccatura,-pl| X ]).
irregular_form( achaemenidae,X,[achaemenid,-pl| X ]).
irregular_form( adieux,X,[adieu,-pl| X ]).
irregular_form( amninia,X,[amnion,-pl| X ]).
irregular_form( amoririni,X,[amorino,-pl| X ]).
irregular_form( amphicia,X,[amphithecium,-pl| X ]).
irregular_form( anatyxes,X,[anaptyxis,-pl| X ]).
irregular_form( androsphinges,X,[androsphinx,-pl| X ]).
irregular_form( andtheridia,X,[antheridium,-pl| X ]).
irregular_form( anlagen,X,[anlage,-pl| X ]).
irregular_form( antefixa,X,[antefix,-pl| X ]).
irregular_form( antheridiia,X,[antheridium,-pl| X ]).
irregular_form( anthraces,X,[anthrax,-pl| X ]).
irregular_form( aphides,X,[aphis,-pl| X ]).
irregular_form( appoggiature,X,[appoggiatura,-pl| X ]).
irregular_form( apsides,X,[apsis,-pl| X ]).
irregular_form( araglis,X,[argali,-pl| X ]).
irregular_form( ariette,X,[arietta,-pl| X ]).
irregular_form( armamentariia,X,[armamentarium,-pl| X ]).
irregular_form( artal,X,[rotl,-pl| X ]).
irregular_form( artel,X,[rotl,-pl| X ]).
irregular_form( ascidcidia,X,[ascidium,-pl| X ]).
irregular_form( aurar,X,[eyrir,-pl| X ]).
irregular_form( banditti,X,[bandit,-pl| X ]).
irregular_form( barklice,X,[barklouse,-pl| X ]).
irregular_form( basidiia,X,[basidium,-pl| X ]).
irregular_form( basileis,X,[basileus,-pl| X ]).
irregular_form( beadsmen,X,[beadsman,-pl| X ]).
irregular_form( beadsmen,X,[bedesman,-pl| X ]).
irregular_form( beeves,X,[beef,-pl| X ]).
irregular_form( behooves,X,[behoof,-pl| X ]).
irregular_form( bersaglieri,X,[bersagliere,-pl| X ]).
irregular_form( bicennaries,X,[bicentenary,-pl| X ]).
irregular_form( bicennaries,X,[bicentennial,-pl| X ]).
irregular_form( bijoux,X,[bijou,-pl| X ]).
irregular_form( bok,X,[boschbok,-pl| X ]).
irregular_form( booklice,X,[booklouse,-pl| X ]).
irregular_form( boraces,X,[borax,-pl| X ]).
irregular_form( brainchildren,X,[brainchild,-pl| X ]).
irregular_form( brethren,X,[brother,-pl| X ]).
irregular_form( broadleaves,X,[broadleaf,-pl| X ]).
irregular_form( buckteeth,X,[bucktooth,-pl| X ]).
irregular_form( bunde,X,[bund,-pl| X ]).
irregular_form( bushbok,X,[boschbok,-pl| X ]).
irregular_form( bushboks,X,[boschbok,-pl| X ]).
irregular_form( calces,X,[calx,-pl| X ]).
irregular_form( calyces,X,[calyx,-pl| X ]).
irregular_form( canzoni,X,[canzone,-pl| X ]).
irregular_form( capita,X,[caput,-pl| X ]).
irregular_form( capricci,X,[capriccio,-pl| X ]).
irregular_form( carabinieri,X,[carabiniere,-pl| X ]).
irregular_form( carides,X,[caryatid,-pl| X ]).
irregular_form( caryopsides,X,[caryopsis,-pl| X ]).
irregular_form( cavatine,X,[cavatina,-pl| X ]).
irregular_form( cephalothoraces,X,[cephalothorax,-pl| X ]).
irregular_form( cercariiae,X,[cercaria,-pl| X ]).
irregular_form( cestuses,X,[caestus,-pl| X ]).
irregular_form( chadarim,X,[cheder,-pl| X ]).
irregular_form( challoth,X,[hallah,-pl| X ]).
irregular_form( chalutzim,X,[chalutz,-pl| X ]).
irregular_form( chasidim,X,[chasid,-pl| X ]).
irregular_form( chassidim,X,[chassid,-pl| X ]).
irregular_form( chazanim,X,[chazan,-pl| X ]).
irregular_form( chedarim,X,[cheder,-pl| X ]).
irregular_form( cherubim,X,[cherub,-pl| X ]).
irregular_form( chitarroni,X,[chitarrone,-pl| X ]).
irregular_form( choriamambi,X,[choriambus,-pl| X ]).
irregular_form( choux,X,[chou,-pl| X ]).
irregular_form( chrysalides,X,[chrysalis,-pl| X ]).
irregular_form( cicale,X,[cicala,-pl| X ]).
irregular_form( ciceroni,X,[cicerone,-pl| X ]).
irregular_form( clani,X,[clarino,-pl| X ]).
irregular_form( clanos,X,[clarino,-pl| X ]).
irregular_form( clostridiia,X,[clostridium,-pl| X ]).
irregular_form( cloverleaves,X,[cloverleaf,-pl| X ]).
irregular_form( coccyges,X,[coccyx,-pl| X ]).
irregular_form( collegigia,X,[collegium,-pl| X ]).
irregular_form( columbariia,X,[columbarium,-pl| X ]).
irregular_form( comedones,X,[comedo,-pl| X ]).
irregular_form( concertanti,X,[concertante,-pl| X ]).
irregular_form( condottieri,X,[condottiere,-pl| X ]).
irregular_form( conidnidia,X,[conidium,-pl| X ]).
irregular_form( conversazioni,X,[conversazione,-pl| X ]).
irregular_form( cornua,X,[cornu,-pl| X ]).
irregular_form( corpora,X,[corpus,-pl| X ]).
irregular_form( cruces,X,[crux,-pl| X ]).
irregular_form( crura,X,[crus,-pl| X ]).
irregular_form( cryings,X,[cry,-pl| X ]).
irregular_form( ctenidiia,X,[ctenidium,-pl| X ]).
irregular_form( custodes,X,[custos,-pl| X ]).
irregular_form( cyclopes,X,[cyclops,-pl| X ]).
irregular_form( cylikes,X,[cylix,-pl| X ]).
irregular_form( cylikes,X,[cylix,-pl| X ]).
irregular_form( daymio,X,[daimio,-pl| X ]).
irregular_form( daymios,X,[daimio,-pl| X ]).
irregular_form( definientia,X,[definiens,-pl| X ]).
irregular_form( delphiniia,X,[delphinium,-pl| X ]).
irregular_form( dermatotoses,X,[dermatosis,-pl| X ]).
irregular_form( diaerses,X,[diaeresis,-pl| X ]).
irregular_form( diathses,X,[diathesis,-pl| X ]).
irregular_form( dibbukkim,X,[dibbuk,-pl| X ]).
irregular_form( dilettanti,X,[dilettante,-pl| X ]).
irregular_form( dive,X,[diva,-pl| X ]).
irregular_form( djinn,X,[djinni,-pl| X ]).
irregular_form( djinn,X,[djinny,-pl| X ]).
irregular_form( dogteeth,X,[dogtooth,-pl| X ]).
irregular_form( dormice,X,[dormouse,-pl| X ]).
irregular_form( duona,X,[duodenum,-pl| X ]).
irregular_form( duonas,X,[duodenum,-pl| X ]).
irregular_form( dwarves,X,[dwarf,-pl| X ]).
irregular_form( dybbukkim,X,[dybbuk,-pl| X ]).
irregular_form( eisteddfodau,X,[eisteddfod,-pl| X ]).
irregular_form( endothecicia,X,[endothecium,-pl| X ]).
irregular_form( entia,X,[ens,-pl| X ]).
irregular_form( ephemerides,X,[ephemeris,-pl| X ]).
irregular_form( epicalyces,X,[epicalyx,-pl| X ]).
irregular_form( epicedidia,X,[epicedium,-pl| X ]).
irregular_form( epididymides,X,[epididymis,-pl| X ]).
irregular_form( epiglottides,X,[epiglottis,-pl| X ]).
irregular_form( epyllilia,X,[epyllion,-pl| X ]).
irregular_form( eupatridae,X,[eupatrid,-pl| X ]).
irregular_form( eyeteeth,X,[eyetooth,-pl| X ]).
irregular_form( fabliaux,X,[fabliau,-pl| X ]).
irregular_form( fedayeen,X,[fedayee,-pl| X ]).
irregular_form( fellaheen,X,[fellah,-pl| X ]).
irregular_form( fellahin,X,[fellah,-pl| X ]).
irregular_form( femora,X,[femur,-pl| X ]).
irregular_form( fermate,X,[fermata,-pl| X ]).
irregular_form( festschriften,X,[festschrift,-pl| X ]).
irregular_form( fieldmice,X,[fieldmouse,-pl| X ]).
irregular_form( filariiae,X,[filaria,-pl| X ]).
irregular_form( flagstaves,X,[flagstaff,-pl| X ]).
irregular_form( flamines,X,[flamen,-pl| X ]).
irregular_form( flittermice,X,[flittermouse,-pl| X ]).
irregular_form( floreant,X,[floreat,-pl| X ]).
irregular_form( flyleaves,X,[flyleaf,-pl| X ]).
irregular_form( foreteeth,X,[foretooth,-pl| X ]).
irregular_form( frauen,X,[frau,-pl| X ]).
irregular_form( frontes,X,[frons,-pl| X ]).
irregular_form( gammadidia,X,[gammadion,-pl| X ]).
irregular_form( geese,X,[goose,-pl| X ]).
irregular_form( gemboks,X,[gemsbok,-pl| X ]).
irregular_form( gembucks,X,[gemsbuck,-pl| X ]).
irregular_form( gemeinschaften,X,[gemeinschaft,-pl| X ]).
irregular_form( genera,X,[genus,-pl| X ]).
irregular_form( gentes,X,[gens,-pl| X ]).
irregular_form( genua,X,[genu,-pl| X ]).
irregular_form( gesellschaften,X,[gesellschaft,-pl| X ]).
irregular_form( gestalten,X,[gestalt,-pl| X ]).
irregular_form( glandes,X,[glans,-pl| X ]).
irregular_form( glochidcia,X,[glochidium,-pl| X ]).
irregular_form( glottides,X,[glottis,-pl| X ]).
irregular_form( godchildren,X,[godchild,-pl| X ]).
irregular_form( gonidiia,X,[gonidium,-pl| X ]).
irregular_form( goninia,X,[gonion,-pl| X ]).
irregular_form( gorgoneineia,X,[gorgoneion,-pl| X ]).
irregular_form( gospopoda,X,[gospodin,-pl| X ]).
irregular_form( goyim,X,[goy,-pl| X ]).
irregular_form( grafen,X,[graf,-pl| X ]).
irregular_form( grandchildren,X,[grandchild,-pl| X ]).
irregular_form( groszy,X,[grosz,-pl| X ]).
irregular_form( gurnard,X,[gurnar,-pl| X ]).
irregular_form( gynecea,X,[gynecium,-pl| X ]).
irregular_form( gynoecea,X,[gynoecium,-pl| X ]).
irregular_form( hadarim,X,[heder,-pl| X ]).
irregular_form( haematozozoa,X,[haematozoon,-pl| X ]).
irregular_form( haeredes,X,[haeres,-pl| X ]).
irregular_form( haftaroth,X,[haftarah,-pl| X ]).
irregular_form( haggadoth,X,[haggada,-pl| X ]).
irregular_form( haleru,X,[haler,-pl| X ]).
irregular_form( hallot,X,[hallah,-pl| X ]).
irregular_form( halloth,X,[hallah,-pl| X ]).
irregular_form( halluces,X,[hallux,-pl| X ]).
irregular_form( haphtaroth,X,[haphtarah,-pl| X ]).
irregular_form( hasidim,X,[hasid,-pl| X ]).
irregular_form( hassidim,X,[hassid,-pl| X ]).
irregular_form( hazzanim,X,[hazzan,-pl| X ]).
irregular_form( heldentenore,X,[heldentenor,-pl| X ]).
irregular_form( hematozozoa,X,[hematozoon,-pl| X ]).
irregular_form( heraclidae,X,[heraclid,-pl| X ]).
irregular_form( heraklidae,X,[heraklid,-pl| X ]).
irregular_form( herbariia,X,[herbarium,-pl| X ]).
irregular_form( herren,X,[herr,-pl| X ]).
irregular_form( hieracosphinges,X,[hieracosphinx,-pl| X ]).
irregular_form( honorariia,X,[honorarium,-pl| X ]).
irregular_form( hooves,X,[hoof,-pl| X ]).
irregular_form( hymenoptera,X,[hymenopteran,-pl| X ]).
irregular_form( hynia,X,[hymenium,-pl| X ]).
irregular_form( hyniums,X,[hymenium,-pl| X ]).
irregular_form( hyraces,X,[hyrax,-pl| X ]).
irregular_form( ibo,X,[igbo,-pl| X ]).
irregular_form( igorrorote,X,[igorrote,-pl| X ]).
irregular_form( imagines,X,[imago,-pl| X ]).
irregular_form( imperiria,X,[imperium,-pl| X ]).
irregular_form( incudes,X,[incus,-pl| X ]).
irregular_form( intagli,X,[intaglio,-pl| X ]).
irregular_form( interleaves,X,[interleaf,-pl| X ]).
irregular_form( interreges,X,[interrex,-pl| X ]).
irregular_form( irides,X,[iris,-pl| X ]).
irregular_form( jinn,X,[jinni,-pl| X ]).
irregular_form( jura,X,[jus,-pl| X ]).
irregular_form( kaddishim,X,[kaddish,-pl| X ]).
irregular_form( kalmuck,X,[kalmuc,-pl| X ]).
irregular_form( keeshonden,X,[keeshond,-pl| X ]).
irregular_form( kibbutzim,X,[kibbutz,-pl| X ]).
irregular_form( kronen,X,[krone,-pl| X ]).
irregular_form( kronur,X,[krona,-pl| X ]).
irregular_form( kylikes,X,[kylix,-pl| X ]).
irregular_form( kylikes,X,[kylix,-pl| X ]).
irregular_form( lacunaria,X,[lacunar,-pl| X ]).
irregular_form( lapithae,X,[lapith,-pl| X ]).
irregular_form( larynges,X,[larynx,-pl| X ]).
irregular_form( latu,X,[lat,-pl| X ]).
irregular_form( leges,X,[lex,-pl| X ]).
irregular_form( lei,X,[leu,-pl| X ]).
irregular_form( lentigines,X,[lentigo,-pl| X ]).
irregular_form( lepidoptera,X,[lepidopteran,-pl| X ]).
irregular_form( leva,X,[lev,-pl| X ]).
irregular_form( lice,X,[louse,-pl| X ]).
irregular_form( limites,X,[limes,-pl| X ]).
irregular_form( lire,X,[lira,-pl| X ]).
irregular_form( listente,X,[sente,-pl| X ]).
irregular_form( litai,X,[lit,-pl| X ]).
irregular_form( litai,X,[litas,-pl| X ]).
irregular_form( litu,X,[litas,-pl| X ]).
irregular_form( lixiviia,X,[lixivium,-pl| X ]).
irregular_form( loaves,X,[loaf,-pl| X ]).
irregular_form( loggie,X,[loggia,-pl| X ]).
irregular_form( lomenmenta,X,[lomentum,-pl| X ]).
irregular_form( lumbus,X,[lumbi,-pl| X ]).
irregular_form( lustra,X,[lustre,-pl| X ]).
irregular_form( lymphangitides,X,[lymphangitis,-pl| X ]).
irregular_form( maare,X,[maar,-pl| X ]).
irregular_form( machzorim,X,[machzor,-pl| X ]).
irregular_form( madornos,X,[madrono,-pl| X ]).
irregular_form( mahzorim,X,[mahzor,-pl| X ]).
irregular_form( makuta,X,[likuta,-pl| X ]).
irregular_form( maloti,X,[loti,-pl| X ]).
irregular_form( marchese,X,[marchesa,-pl| X ]).
irregular_form( marchesi,X,[marchese,-pl| X ]).
irregular_form( maremme,X,[maremma,-pl| X ]).
irregular_form( markkaa,X,[markka,-pl| X ]).
irregular_form( matzoth,X,[matzo,-pl| X ]).
irregular_form( megilloth,X,[megillah,-pl| X ]).
irregular_form( menservants,X,[manservant,-pl| X ]).
irregular_form( mesdames,X,[madame,-pl| X ]).
irregular_form( mesdemoiselles,X,[mademoiselle,-pl| X ]).
irregular_form( mesentertera,X,[mesenteron,-pl| X ]).
irregular_form( mesothoraces,X,[mesothorax,-pl| X ]).
irregular_form( messeigneurs,X,[monseigneur,-pl| X ]).
irregular_form( messieurs,X,[monsieur,-pl| X ]).
irregular_form( metanephroi,X,[metanephros,-pl| X ]).
irregular_form( metathoraces,X,[metathorax,-pl| X ]).
irregular_form( mezuzoth,X,[mezuzah,-pl| X ]).
irregular_form( mice,X,[mouse,-pl| X ]).
irregular_form( midrashim,X,[midrash,-pl| X ]).
irregular_form( milieux,X,[milieu,-pl| X ]).
irregular_form( minyanim,X,[minyan,-pl| X ]).
irregular_form( miracidiia,X,[miracidium,-pl| X ]).
irregular_form( mishnayoth,X,[mishna,-pl| X ]).
irregular_form( mishnayoth,X,[mishnah,-pl| X ]).
irregular_form( mitzvoth,X,[mitzvah,-pl| X ]).
irregular_form( monopteroi,X,[monopteros,-pl| X ]).
irregular_form( moshavim,X,[moshav,-pl| X ]).
irregular_form( moslim,X,[moslem,-pl| X ]).
irregular_form( moslims,X,[moslem,-pl| X ]).
irregular_form( mucrones,X,[mucro,-pl| X ]).
irregular_form( muskallunge,X,[muskellunge,-pl| X ]).
irregular_form( mythoi,X,[mythos,-pl| X ]).
irregular_form( naoi,X,[naos,-pl| X ]).
irregular_form( nasopharynges,X,[nasopharynx,-pl| X ]).
irregular_form( necropoleis,X,[necropolis,-pl| X ]).
irregular_form( nephridiia,X,[nephridium,-pl| X ]).
irregular_form( nibelungen,X,[nibelung,-pl| X ]).
irregular_form( novelle,X,[novella,-pl| X ]).
irregular_form( occipita,X,[occiput,-pl| X ]).
irregular_form( oceanariia,X,[oceanarium,-pl| X ]).
irregular_form( ommatidtidia,X,[ommatidium,-pl| X ]).
irregular_form( onagri,X,[onager,-pl| X ]).
irregular_form( ora,X,[os,-pl| X ]).
irregular_form( orthoptertera,X,[orthopteron,-pl| X ]).
irregular_form( osar,X,[os,-pl| X ]).
irregular_form( ossa,X,[os,-pl| X ]).
irregular_form( oxen,X,[ox,-pl| X ]).
irregular_form( paise,X,[paisa,-pl| X ]).
irregular_form( panettoni,X,[panettone,-pl| X ]).
irregular_form( paramenta,X,[parament,-pl| X ]).
irregular_form( parashoth,X,[parashah,-pl| X ]).
irregular_form( parietes,X,[paries,-pl| X ]).
irregular_form( parulides,X,[parulis,-pl| X ]).
irregular_form( pastorali,X,[pastorale,-pl| X ]).
irregular_form( patresfamilias,X,[paterfamilias,-pl| X ]).
irregular_form( pease,X,[pea,-pl| X ]).
irregular_form( pectines,X,[pecten,-pl| X ]).
irregular_form( pedes,X,[pes,-pl| X ]).
irregular_form( pekingese,X,[pekinese,-pl| X ]).
irregular_form( pence,X,[penny,-pl| X ]).
irregular_form( penetralium,X,[penetralia,-pl| X ]).
irregular_form( pennia,X,[penni,-pl| X ]).
irregular_form( perionychiia,X,[perionychium,-pl| X ]).
irregular_form( pfennige,X,[pfennig,-pl| X ]).
irregular_form( pharynges,X,[pharynx,-pl| X ]).
irregular_form( pithoi,X,[pithos,-pl| X ]).
irregular_form( planetariia,X,[planetarium,-pl| X ]).
irregular_form( plasmodesdesmata,X,[plasmodesma,-pl| X ]).
irregular_form( ploughmen,X,[ploughman,-pl| X ]).
irregular_form( ploughmen,X,[plowman,-pl| X ]).
irregular_form( poleis,X,[polis,-pl| X ]).
irregular_form( polliniia,X,[pollinium,-pl| X ]).
irregular_form( polyzoariia,X,[polyzoarium,-pl| X ]).
irregular_form( pontes,X,[pons,-pl| X ]).
irregular_form( postliminiia,X,[postliminium,-pl| X ]).
irregular_form( predelle,X,[predella,-pl| X ]).
irregular_form( prese,X,[presa,-pl| X ]).
irregular_form( principiia,X,[principium,-pl| X ]).
irregular_form( proboscides,X,[proboscis,-pl| X ]).
irregular_form( promycelilia,X,[promycelium,-pl| X ]).
irregular_form( pronephra,X,[pronephros,-pl| X ]).
irregular_form( pronephroi,X,[pronephros,-pl| X ]).
irregular_form( prothalamimia,X,[prothalamion,-pl| X ]).
irregular_form( prothalamimia,X,[prothalamium,-pl| X ]).
irregular_form( prothoraces,X,[prothorax,-pl| X ]).
irregular_form( protozoa,X,[protozoan,-pl| X ]).
irregular_form( proventricutriculi,X,[proventriculus,-pl| X ]).
irregular_form( pupariia,X,[puparium,-pl| X ]).
irregular_form( pycnidiia,X,[pycnidium,-pl| X ]).
irregular_form( pygidiia,X,[pygidium,-pl| X ]).
irregular_form( pyxides,X,[pyxis,-pl| X ]).
irregular_form( pyxidiia,X,[pyxidium,-pl| X ]).
irregular_form( qaddishim,X,[qaddish,-pl| X ]).
irregular_form( quarterstaves,X,[quarterstaff,-pl| X ]).
irregular_form( rachides,X,[rhachis,-pl| X ]).
irregular_form( rearmice,X,[rearmouse,-pl| X ]).
irregular_form( reis,X,[real,-pl| X ]).
irregular_form( remiges,X,[remex,-pl| X ]).
irregular_form( reremice,X,[rearmouse,-pl| X ]).
irregular_form( reremice,X,[reremouse,-pl| X ]).
irregular_form( residuua,X,[residuum,-pl| X ]).
irregular_form( retiararii,X,[retiarius,-pl| X ]).
irregular_form( rhachides,X,[rhachis,-pl| X ]).
irregular_form( ricercacari,X,[ricercare,-pl| X ]).
irregular_form( ricercari,X,[ricercare,-pl| X ]).
irregular_form( roma,X,[rom,-pl| X ]).
irregular_form( salespeople,X,[salesperson,-pl| X ]).
irregular_form( salpinges,X,[salpinx,-pl| X ]).
irregular_form( sassanidae,X,[sassanid,-pl| X ]).
irregular_form( scarves,X,[scarf,-pl| X ]).
irregular_form( schatchonim,X,[schatchen,-pl| X ]).
irregular_form( schatchonim,X,[shadchan,-pl| X ]).
irregular_form( schuln,X,[schul,-pl| X ]).
irregular_form( schutzstaffeln,X,[schutzstaffel,-pl| X ]).
irregular_form( scoleces,X,[scolex,-pl| X ]).
irregular_form( seleucidae,X,[seleucid,-pl| X ]).
irregular_form( septariia,X,[septarium,-pl| X ]).
irregular_form( seraphim,X,[seraph,-pl| X ]).
irregular_form( shabbasim,X,[shabbas,-pl| X ]).
irregular_form( shabbatim,X,[shabbat,-pl| X ]).
irregular_form( shadchanim,X,[shadchan,-pl| X ]).
irregular_form( shammosim,X,[shammas,-pl| X ]).
irregular_form( shammosim,X,[shammes,-pl| X ]).
irregular_form( sheaves,X,[sheaf,-pl| X ]).
irregular_form( shinleaves,X,[shinleaf,-pl| X ]).
irregular_form( shittim,X,[shittah,-pl| X ]).
irregular_form( shofroth,X,[shofar,-pl| X ]).
irregular_form( shofroth,X,[shophar,-pl| X ]).
irregular_form( shophroth,X,[shophar,-pl| X ]).
irregular_form( shrewmice,X,[shrewmouse,-pl| X ]).
irregular_form( shuln,X,[shul,-pl| X ]).
irregular_form( siddurim,X,[siddur,-pl| X ]).
irregular_form( sigloi,X,[siglos,-pl| X ]).
irregular_form( signore,X,[signora,-pl| X ]).
irregular_form( signori,X,[signior,-pl| X ]).
irregular_form( signori,X,[signore,-pl| X ]).
irregular_form( signorine,X,[signorina,-pl| X ]).
irregular_form( sincipita,X,[sinciput,-pl| X ]).
irregular_form( sinfonie,X,[sinfonia,-pl| X ]).
irregular_form( snaggleteeth,X,[snaggletooth,-pl| X ]).
irregular_form( socmen,X,[socman,-pl| X ]).
irregular_form( socmen,X,[sokeman,-pl| X ]).
irregular_form( solfeggi,X,[solfeggio,-pl| X ]).
irregular_form( sovkhozy,X,[sovkhoz,-pl| X ]).
irregular_form( sphinges,X,[sphinx,-pl| X ]).
irregular_form( springhase,X,[springhaas,-pl| X ]).
irregular_form( spumoni,X,[spumone,-pl| X ]).
irregular_form( stapedes,X,[stapes,-pl| X ]).
irregular_form( startsy,X,[starets,-pl| X ]).
irregular_form( stepchildren,X,[stepchild,-pl| X ]).
irregular_form( stipites,X,[stipes,-pl| X ]).
irregular_form( stirpes,X,[stirps,-pl| X ]).
irregular_form( stotinki,X,[stotinka,-pl| X ]).
irregular_form( stotkini,X,[stotinka,-pl| X ]).
irregular_form( stylopes,X,[stylops,-pl| X ]).
irregular_form( subgenera,X,[subgenus,-pl| X ]).
irregular_form( substrasta,X,[substratum,-pl| X ]).
irregular_form( sudatotoria,X,[sudatorium,-pl| X ]).
irregular_form( syringes,X,[syrinx,-pl| X ]).
irregular_form( tallaisim,X,[tallith,-pl| X ]).
irregular_form( tallitoth,X,[tallith,-pl| X ]).
irregular_form( teeth,X,[tooth,-pl| X ]).
irregular_form( teraphim,X,[teraph,-pl| X ]).
irregular_form( terata,X,[teras,-pl| X ]).
irregular_form( teredines,X,[teredo,-pl| X ]).
irregular_form( testudines,X,[testudo,-pl| X ]).
irregular_form( therses,X,[thyrse,-pl| X ]).
irregular_form( thickleaves,X,[thickleaf,-pl| X ]).
irregular_form( thieves,X,[thief,-pl| X ]).
irregular_form( tholoi,X,[tholos,-pl| X ]).
irregular_form( thoraces,X,[thorax,-pl| X ]).
irregular_form( titmice,X,[titmouse,-pl| X ]).
irregular_form( topoi,X,[topos,-pl| X ]).
irregular_form( tricliniia,X,[triclinium,-pl| X ]).
irregular_form( triviia,X,[trivium,-pl| X ]).
irregular_form( turves,X,[turf,-pl| X ]).
irregular_form( ubermenschen,X,[ubermensch,-pl| X ]).
irregular_form( uigurs,X,[uighur,-pl| X ]).
irregular_form( umbones,X,[umbo,-pl| X ]).
irregular_form( uncicini,X,[uncinus,-pl| X ]).
irregular_form( uredidia,X,[uredium,-pl| X ]).
irregular_form( uredines,X,[uredo,-pl| X ]).
irregular_form( vasa,X,[vas,-pl| X ]).
irregular_form( vertigines,X,[vertigo,-pl| X ]).
irregular_form( vires,X,[vis,-pl| X ]).
irregular_form( vivariia,X,[vivarium,-pl| X ]).
irregular_form( voces,X,[vox,-pl| X ]).
irregular_form( volte,X,[volta,-pl| X ]).
irregular_form( wanderjahre,X,[wanderjahr,-pl| X ]).
irregular_form( wharves,X,[wharf,-pl| X ]).
irregular_form( woodlice,X,[woodlouse,-pl| X ]).
irregular_form( yeshivahs,X,[yeshiva,-pl| X ]).
irregular_form( yeshivoth,X,[yeshiva,-pl| X ]).
irregular_form( yogin,X,[yogi,-pl| X ]).
irregular_form( zoeas,X,[zoaea,-pl| X ]).

/****************************************************
% File Name: pronto_morph_irreg_verb.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation
%
% This file is to be used as part of the morphological analyzer
% that is a component of ProNTo (Prolog Natural Language
% Toolkit), made at the Artificial Intelligence Center of
% the University of Georgia (www.ai.uga.edu).
%
% Special thanks:
% 1)To the wordnet project for their list words that are exceptions to the spelling rules.
% 2)To Dr. Covington for his guidence on the project.
******************************************************/


% irregular_form(+Atom,-Tail,-List)
%  Interprets an irregular form (as an open list)


irregular_form( ran,      X, [run, -past  | X ]).
irregular_form( run,      X, [run, -en  | X ]).
irregular_form( eaten,    X, [eat, -en | X] ).
irregular_form( has,      X, [have, -sg3  | X] ).
irregular_form( had,      X, [have, -ed | X] ).

% contractions minus the apostrophe
%
irregular_form( cant,    X, [can,   not | X] ).
irregular_form( ill,    X, [i,   will | X] ).
irregular_form( didnt,    X, [did,   not | X] ).
irregular_form( doesnt,   X, [do, -sg3, not | X ]).    % this one has three
irregular_form( wouldnt,    X, [would,   not | X] ).
irregular_form( couldnt,    X, [could,   not | X] ).
irregular_form( shouldnt,    X, [should,   not | X] ).
irregular_form( wasnt,    X, [is, -past, not | X] ).
irregular_form( isnt,    X, [is,   not | X] ).
irregular_form( mustnt,    X, [must,   not | X] ).
irregular_form( youll,    X, [you,   will | X] ).


% adapted from modified exception list from wordnet
%
irregular_form( abode,X,[abide,-past| X ]).
irregular_form( arcked,X,[arc,-ed| X ]).
irregular_form( arcking,X,[arc,-ing| X ]).
irregular_form( arose,X,[arise,-past| X ]).
irregular_form( ate,X,[eat,-past| X ]).
irregular_form( awoke,X,[awake,-past| X ]).
irregular_form( awoken,X,[awake,-en| X ]).
irregular_form( backbit,X,[backbite,-past| X ]).
irregular_form( backbitten,X,[backbite,-en| X ]).
irregular_form( backslid,X,[backslide,-past| X ]).
irregular_form( backslidden,X,[backslide,-en| X ]).
irregular_form( bade,X,[bid,-past| X ]).
irregular_form( beaten,X,[beat,-en| X ]).
irregular_form( became,X,[become,-past| X ]).
irregular_form( befallen,X,[befall,-en| X ]).
irregular_form( befell,X,[befall,-past| X ]).
irregular_form( began,X,[begin,-past| X ]).
irregular_form( begat,X,[beget,-past| X ]).
irregular_form( begot,X,[beget,-past| X ]).
irregular_form( begotten,X,[beget,-en| X ]).
irregular_form( begun,X,[begin,-en| X ]).
irregular_form( beheld,X,[behold,-past| X ]).
irregular_form( beholden,X,[behold,-en| X ]).
irregular_form( belying,X,[belie,-ing| X ]).
irregular_form( bent,X,[bend,-past| X ]).
irregular_form( besought,X,[beseech,-past| X ]).
irregular_form( bespoke,X,[bespeak,-past| X ]).
irregular_form( bespoken,X,[bespeak,-en| X ]).
irregular_form( bethought,X,[bethink,-past| X ]).
irregular_form( betook,X,[betake,-past| X ]).
irregular_form( bidden,X,[bid,-en| X ]).
irregular_form( bit,X,[bite,-past| X ]).
irregular_form( bitten,X,[bite,-en| X ]).
irregular_form( bivouacked,X,[bivouac,-past| X ]).
irregular_form( bivouacking,X,[bivouac,-ing| X ]).
irregular_form( bled,X,[bleed,-past| X ]).
irregular_form( blest,X,[bless,-past| X ]).
irregular_form( blew,X,[blow,-past| X ]).
irregular_form( blown,X,[blow,-en| X ]).
irregular_form( bore,X,[bear,-past| X ]).
irregular_form( born,X,[bear,-en| X ]).
irregular_form( borne,X,[bear,-en| X ]).
irregular_form( bought,X,[buy,-past| X ]).
irregular_form( bound,X,[bind,-past| X ]).
irregular_form( bred,X,[breed,-past| X ]).
irregular_form( broke,X,[break,-past| X ]).
irregular_form( broken,X,[break,-en| X ]).
irregular_form( brought,X,[bring,-past| X ]).
irregular_form( browbeaten,X,[browbeat,-en| X ]).
irregular_form( built,X,[build,-past| X ]).
irregular_form( burnt,X,[burn,-past| X ]).
irregular_form( bypast,X,[bypass,-past| X ]).
irregular_form( came,X,[come,-past| X ]).
irregular_form( caught,X,[catch,-past| X ]).
irregular_form( chevied,X,[chivy,-past| X ]).
irregular_form( chevies,X,[chivy,-sg| X ]).
irregular_form( chevying,X,[chivy,-ing| X ]).
irregular_form( chose,X,[choose,-past| X ]).
irregular_form( chosen,X,[choose,-en| X ]).
irregular_form( clad,X,[clothe,-past| X ]).
irregular_form( cleft,X,[cleave,-past| X ]).
irregular_form( clept,X,[clepe,-past| X ]).
irregular_form( clove,X,[cleave,-past| X ]).
irregular_form( cloven,X,[cleave,-en| X ]).
irregular_form( clung,X,[cling,-past| X ]).
irregular_form( countersank,X,[countersink,-past| X ]).
irregular_form( countersunk,X,[countersink,-past| X ]).
irregular_form( crept,X,[creep,-past| X ]).
irregular_form( crossbred,X,[crossbreed,-past| X ]).
irregular_form( curst,X,[curse,-past| X ]).
irregular_form( dealt,X,[deal,-past| X ]).
irregular_form( did,X,[do,-past| X ]).
irregular_form( disenthralls,X,[disenthral,-sg| X ]).
irregular_form( done,X,[do,-past| X ]).
irregular_form( dought,X,[dow,-past| X ]).
irregular_form( dove,X,[dive,-past| X ]).
irregular_form( drank,X,[drink,-past| X ]).
irregular_form( drawn,X,[draw,-en| X ]).
irregular_form( dreamt,X,[dream,-past| X ]).
irregular_form( drew,X,[draw,-past| X ]).
irregular_form( drove,X,[drive,-past| X ]).
irregular_form( drunk,X,[drink,-past| X ]).
irregular_form( dug,X,[dig,-past| X ]).
irregular_form( dwelt,X,[dwell,-past| X ]).
irregular_form( dying,X,[die,-ing| X ]).
irregular_form( enwound,X,[enwind,-past| X ]).
irregular_form( fallen,X,[fall,-en| X ]).
irregular_form( fed,X,[feed,-past| X ]).
irregular_form( fell,X,[fall,-past| X ]).
irregular_form( fled,X,[flee,-past| X ]).
irregular_form( flew,X,[fly,-past| X ]).
irregular_form( floodlit,X,[floodlight,-past| X ]).
irregular_form( flown,X,[fly,-en| X ]).
irregular_form( flung,X,[fling,-past| X ]).
irregular_form( forbad,X,[forbid,-past| X ]).
irregular_form( forbade,X,[forbid,-past| X ]).
irregular_form( forbidden,X,[forbid,-en| X ]).
irregular_form( forbore,X,[forbear,-past| X ]).
irregular_form( forborne,X,[forbear,-past| X ]).
irregular_form( foregone,X,[forego,-past| X ]).
irregular_form( foreknew,X,[foreknow,-past| X ]).
irregular_form( foreknown,X,[foreknow,-en| X ]).
irregular_form( foreran,X,[forerun,-past| X ]).
irregular_form( foresaw,X,[foresee,-past| X ]).
irregular_form( foreshown,X,[foreshow,-en| X ]).
irregular_form( forespoke,X,[forespeak,-past| X ]).
irregular_form( forespoken,X,[forespeak,-en| X ]).
irregular_form( foretold,X,[foretell,-past| X ]).
irregular_form( forewent,X,[forego,-past| X ]).
irregular_form( forgave,X,[forgive,-past| X ]).
irregular_form( forgone,X,[forgo,-past| X ]).
irregular_form( forgot,X,[forget,-past| X ]).
irregular_form( forgotten,X,[forget,-en| X ]).
irregular_form( forsook,X,[forsake,-past| X ]).
irregular_form( forspoke,X,[forspeak,-past| X ]).
irregular_form( forspoken,X,[forspeak,-en| X ]).
irregular_form( forswore,X,[forswear,-past| X ]).
irregular_form( forsworn,X,[forswear,-en| X ]).
irregular_form( forwent,X,[forgo,-past| X ]).
irregular_form( fought,X,[fight,-past| X ]).
irregular_form( found,X,[find,-past| X ]).
irregular_form( frolicked,X,[frolic,-past| X ]).
irregular_form( frolicking,X,[frolic,-ing| X ]).
irregular_form( froze,X,[freeze,-past| X ]).
irregular_form( frozen,X,[freeze,-en| X ]).
irregular_form( gainsaid,X,[gainsay,-past| X ]).
irregular_form( gan,X,[gin,-past| X ]).
irregular_form( gave,X,[give,-past| X ]).
irregular_form( gelt,X,[geld,-past| X ]).
irregular_form( ghostwritten,X,[ghostwrite,-en| X ]).
irregular_form( ghostwrote,X,[ghostwrite,-past| X ]).
irregular_form( gilt,X,[gild,-past| X ]).
irregular_form( girt,X,[gird,-past| X ]).
irregular_form( gnawn,X,[gnaw,-en| X ]).
irregular_form( gone,X,[go,-past| X ]).
irregular_form( got,X,[get,-past| X ]).
irregular_form( gotten,X,[get,-en| X ]).
irregular_form( grew,X,[grow,-past| X ]).
irregular_form( gript,X,[grip,-past| X ]).
irregular_form( ground,X,[grind,-past| X ]).
irregular_form( grown,X,[grow,-en| X ]).
irregular_form( hacksawn,X,[hacksaw,-en| X ]).
irregular_form( hamstrung,X,[hamstring,-past| X ]).
irregular_form( handfed,X,[handfeed,-past| X ]).
irregular_form( held,X,[hold,-past| X ]).
irregular_form( hewn,X,[hew,-en| X ]).
irregular_form( hid,X,[hide,-past| X ]).
irregular_form( hidden,X,[hide,-en| X ]).
irregular_form( hogtying,X,[hogtie,-ing| X ]).
irregular_form( honied,X,[honey,-past| X ]).
irregular_form( hove,X,[heave,-past| X ]).
irregular_form( hung,X,[hang,-past| X ]).
irregular_form( inbred,X,[inbreed,-past| X ]).
irregular_form( indwelt,X,[indwell,-past| X ]).
irregular_form( inlaid,X,[inlay,-past| X ]).
irregular_form( interbred,X,[interbreed,-past| X ]).
irregular_form( interlaid,X,[interlay,-past| X ]).
irregular_form( interpled,X,[interplead,-past| X ]).
irregular_form( interwove,X,[interweave,-past| X ]).
irregular_form( interwoven,X,[interweave,-en| X ]).
irregular_form( inwove,X,[inweave,-past| X ]).
irregular_form( inwoven,X,[inweave,-en| X ]).
irregular_form( kent,X,[ken,-past| X ]).
irregular_form( kept,X,[keep,-past| X ]).
irregular_form( knelt,X,[kneel,-past| X ]).
irregular_form( knew,X,[know,-past| X ]).
irregular_form( known,X,[know,-en| X ]).
irregular_form( ladyfied,X,[ladify,-past| X ]).
irregular_form( ladyfies,X,[ladify,-sg| X ]).
irregular_form( ladyfying,X,[ladify,-ing| X ]).
irregular_form( laid,X,[lay,-past| X ]).
irregular_form( lain,X,[lie,-en| X ]).
irregular_form( lay,X,[lie,-past| X ]).
irregular_form( leant,X,[lean,-past| X ]).
irregular_form( leapt,X,[leap,-past| X ]).
irregular_form( learnt,X,[learn,-past| X ]).
irregular_form( led,X,[lead,-past| X ]).
irregular_form( left,X,[leave,-past| X ]).
irregular_form( lent,X,[lend,-past| X ]).
irregular_form( lit,X,[light,-past| X ]).
irregular_form( lost,X,[lose,-past| X ]).
irregular_form( lying,X,[lie,-ing| X ]).
irregular_form( made,X,[make,-past| X ]).
irregular_form( meant,X,[mean,-past| X ]).
irregular_form( met,X,[meet,-past| X ]).
irregular_form( mimicked,X,[mimic,-past| X ]).
irregular_form( mimicking,X,[mimic,-ing| X ]).
irregular_form( misbecame,X,[misbecome,-past| X ]).
irregular_form( misdealt,X,[misdeal,-past| X ]).
irregular_form( misgave,X,[misgive,-past| X ]).
irregular_form( mislaid,X,[mislay,-past| X ]).
irregular_form( misled,X,[mislead,-past| X ]).
irregular_form( mispled,X,[misplead,-past| X ]).
irregular_form( misspelt,X,[misspell,-past| X ]).
irregular_form( misspent,X,[misspend,-past| X ]).
irregular_form( mistook,X,[mistake,-past| X ]).
irregular_form( misunderstood,X,[misunderstand,-past| X ]).
irregular_form( molten,X,[melt,-en| X ]).
irregular_form( mown,X,[mow,-en| X ]).
irregular_form( outbidden,X,[outbid,-en| X ]).
irregular_form( outbred,X,[outbreed,-past| X ]).
irregular_form( outdid,X,[outdo,-past| X ]).
irregular_form( outdone,X,[outdo,-en| X ]).
irregular_form( outdrawn,X,[outdraw,-en| X ]).
irregular_form( outdrew,X,[outdraw,-past| X ]).
irregular_form( outfought,X,[outfight,-past| X ]).
irregular_form( outgone,X,[outgo,-en| X ]).
irregular_form( outgrew,X,[outgrow,-past| X ]).
irregular_form( outgrown,X,[outgrow,-en| X ]).
irregular_form( outlaid,X,[outlay,-past| X ]).
irregular_form( outran,X,[outrun,-past| X ]).
irregular_form( outridden,X,[outride,-en| X ]).
irregular_form( outrode,X,[outride,-past| X ]).
irregular_form( outshone,X,[outshine,-past| X ]).
irregular_form( outshot,X,[outshoot,-past| X ]).
irregular_form( outsold,X,[outsell,-past| X ]).
irregular_form( outstood,X,[outstand,-past| X ]).
irregular_form( outthought,X,[outthink,-past| X ]).
irregular_form( outwent,X,[outgo,-past| X ]).
irregular_form( outwore,X,[outwear,-past| X ]).
irregular_form( outworn,X,[outwear,-en| X ]).
irregular_form( overbidden,X,[overbid,-en| X ]).
irregular_form( overblew,X,[overblow,-past| X ]).
irregular_form( overblown,X,[overblow,-en| X ]).
irregular_form( overbore,X,[overbear,-past| X ]).
irregular_form( overborne,X,[overbear,-en| X ]).
irregular_form( overbuilt,X,[overbuild,-past| X ]).
irregular_form( overcame,X,[overcome,-past| X ]).
irregular_form( overdid,X,[overdo,-past| X ]).
irregular_form( overdone,X,[overdo,-past| X ]).
irregular_form( overdrawn,X,[overdraw,-en| X ]).
irregular_form( overdrew,X,[overdraw,-past| X ]).
irregular_form( overdrove,X,[overdrive,-past| X ]).
irregular_form( overflew,X,[overfly,-past| X ]).
irregular_form( overflown,X,[overfly,-en| X ]).
irregular_form( overgrew,X,[overgrow,-past| X ]).
irregular_form( overgrown,X,[overgrow,-en| X ]).
irregular_form( overhung,X,[overhang,-past| X ]).
irregular_form( overlaid,X,[overlay,-past| X ]).
irregular_form( overlain,X,[overlie,-en| X ]).
irregular_form( overlay,X,[overlie,-past| X ]).
irregular_form( overlying,X,[overlie,-ing| X ]).
irregular_form( overpaid,X,[overpay,-past| X ]).
irregular_form( overpast,X,[overpass,-past| X ]).
irregular_form( overran,X,[overrun,-past| X ]).
irregular_form( overridden,X,[override,-en| X ]).
irregular_form( overrode,X,[override,-past| X ]).
irregular_form( oversaw,X,[oversee,-past| X ]).
irregular_form( oversewn,X,[oversew,-en| X ]).
irregular_form( overshot,X,[overshoot,-past| X ]).
irregular_form( overslept,X,[oversleep,-past| X ]).
irregular_form( oversold,X,[oversell,-past| X ]).
irregular_form( overspent,X,[overspend,-past| X ]).
irregular_form( overspilt,X,[overspill,-past| X ]).
irregular_form( overthrew,X,[overthrow,-past| X ]).
irregular_form( overthrown,X,[overthrow,-past| X ]).
irregular_form( overtook,X,[overtake,-past| X ]).
irregular_form( overwound,X,[overwind,-past| X ]).
irregular_form( overwritten,X,[overwrite,-en| X ]).
irregular_form( overwrote,X,[overwrite,-past| X ]).
irregular_form( paid,X,[pay,-past| X ]).
irregular_form( panicked,X,[panic,-past| X ]).
irregular_form( panicking,X,[panic,-ing| X ]).
irregular_form( partook,X,[partake,-past| X ]).
irregular_form( pasquilled,X,[pasquinade,-past| X ]).
irregular_form( pasquilling,X,[pasquinade,-ing| X ]).
irregular_form( pent,X,[pen,-past| X ]).
irregular_form( physicked,X,[physic,-past| X ]).
irregular_form( physicking,X,[physic,-ing| X ]).
irregular_form( picnicked,X,[picnic,-past| X ]).
irregular_form( picnicking,X,[picnic,-ing| X ]).
irregular_form( pled,X,[plead,-past| X ]).
irregular_form( prepaid,X,[prepay,-past| X ]).
irregular_form( programmes,X,[program,-sg| X ]).
irregular_form( prologed,X,[prologue,-past| X ]).
irregular_form( prologing,X,[prologue,-ing| X ]).
irregular_form( prologs,X,[prologue,-sg| X ]).
irregular_form( quartersawn,X,[quartersaw,-en| X ]).
irregular_form( rang,X,[ring,-past| X ]).
irregular_form( rebuilt,X,[rebuild,-past| X ]).
irregular_form( redid,X,[redo,-past| X ]).
irregular_form( redone,X,[redo,-past| X ]).
irregular_form( reft,X,[reave,-past| X ]).
irregular_form( remade,X,[remake,-past| X ]).
irregular_form( rent,X,[rend,-past| X ]).
irregular_form( repaid,X,[repay,-past| X ]).
irregular_form( reran,X,[rerun,-past| X ]).
irregular_form( resat,X,[resit,-past| X ]).
irregular_form( resewn,X,[resew,-en| X ]).
irregular_form( rethought,X,[rethink,-past| X ]).
irregular_form( retold,X,[retell,-past| X ]).
irregular_form( retook,X,[retake,-past| X ]).
irregular_form( rewound,X,[rewind,-past| X ]).
irregular_form( rewritten,X,[rewrite,-en| X ]).
irregular_form( rewrote,X,[rewrite,-past| X ]).
irregular_form( ridden,X,[ride,-en| X ]).
irregular_form( rode,X,[ride,-past| X ]).
irregular_form( rose,X,[rise,-past| X ]).
irregular_form( rove,X,[reeve,-past| X ]).
irregular_form( rung,X,[ring,-past| X ]).
irregular_form( said,X,[say,-past| X ]).
irregular_form( sang,X,[sing,-past| X ]).
irregular_form( sank,X,[sink,-past| X ]).
irregular_form( sat,X,[sit,-past| X ]).
irregular_form( saw,X,[see,-past| X ]).
irregular_form( sawn,X,[saw,-en| X ]).
irregular_form( sent,X,[send,-past| X ]).
irregular_form( sewn,X,[sew,-en| X ]).
irregular_form( shat,X,[shit,-past| X ]).
irregular_form( shellacked,X,[shellac,-past| X ]).
irregular_form( shellacking,X,[shellac,-ing| X ]).
irregular_form( shent,X,[shend,-past| X ]).
irregular_form( shewn,X,[shew,-en| X ]).
irregular_form( shod,X,[shoe,-past| X ]).
irregular_form( shone,X,[shine,-past| X ]).
irregular_form( shook,X,[shake,-past| X ]).
irregular_form( shot,X,[shoot,-past| X ]).
irregular_form( shown,X,[show,-en| X ]).
irregular_form( shrank,X,[shrink,-past| X ]).
irregular_form( shrove,X,[shrive,-past| X ]).
irregular_form( shrunk,X,[shrink,-past| X ]).
irregular_form( shrunken,X,[shrink,-en| X ]).
irregular_form( sicked,X,[sic,-past| X ]).
irregular_form( sicking,X,[sic,-ing| X ]).
irregular_form( sightsaw,X,[sightsee,-past| X ]).
irregular_form( skydove,X,[skydive,-past| X ]).
irregular_form( slain,X,[slay,-past| X ]).
irregular_form( slept,X,[sleep,-past| X ]).
irregular_form( slew,X,[slay,-past| X ]).
irregular_form( slid,X,[slide,-past| X ]).
irregular_form( slidden,X,[slide,-en| X ]).
irregular_form( slung,X,[sling,-past| X ]).
irregular_form( slunk,X,[slink,-past| X ]).
irregular_form( smelt,X,[smell,-past| X ]).
irregular_form( smit,X,[smite,-past| X ]).
irregular_form( smitten,X,[smite,-en| X ]).
irregular_form( smote,X,[smite,-sg| X ]).
irregular_form( sold,X,[sell,-past| X ]).
irregular_form( soothsaid,X,[soothsay,-past| X ]).
irregular_form( sought,X,[seek,-past| X ]).
irregular_form( sown,X,[sow,-en| X ]).
irregular_form( spat,X,[spit,-past| X ]).
irregular_form( sped,X,[speed,-past| X ]).
irregular_form( spellbound,X,[spellbind,-past| X ]).
irregular_form( spelt,X,[spell,-past| X ]).
irregular_form( spent,X,[spend,-past| X ]).
irregular_form( spilt,X,[spill,-past| X ]).
irregular_form( spoilt,X,[spoil,-past| X ]).
irregular_form( spoke,X,[speak,-past| X ]).
irregular_form( spoken,X,[speak,-en| X ]).
irregular_form( spotlit,X,[spotlight,-past| X ]).
irregular_form( sprang,X,[spring,-past| X ]).
irregular_form( sprung,X,[spring,-past| X ]).
irregular_form( spun,X,[spin,-past| X ]).
irregular_form( stank,X,[stink,-past| X ]).
irregular_form( stilettoeing,X,[stiletto,-ing| X ]).
irregular_form( stole,X,[steal,-past| X ]).
irregular_form( stolen,X,[steal,-en| X ]).
irregular_form( stood,X,[stand,-past| X ]).
irregular_form( stove,X,[stave,-past| X ]).
irregular_form( strewn,X,[strew,-en| X ]).
irregular_form( stridden,X,[stride,-en| X ]).
irregular_form( strode,X,[stride,-past| X ]).
irregular_form( strove,X,[strive,-past| X ]).
irregular_form( strown,X,[strow,-past| X ]).
irregular_form( struck,X,[strike,-past| X ]).
irregular_form( strung,X,[string,-past| X ]).
irregular_form( stuck,X,[stick,-past| X ]).
irregular_form( stung,X,[sting,-past| X ]).
irregular_form( stunk,X,[stink,-past| X ]).
irregular_form( stymying,X,[stymie,-ing| X ]).
irregular_form( sung,X,[sing,-past| X ]).
irregular_form( sunk,X,[sink,-past| X ]).
irregular_form( sunken,X,[sink,-en| X ]).
irregular_form( swam,X,[swim,-past| X ]).
irregular_form( swept,X,[sweep,-past| X ]).
irregular_form( swollen,X,[swell,-en| X ]).
irregular_form( swore,X,[swear,-past| X ]).
irregular_form( sworn,X,[swear,-past| X ]).
irregular_form( swum,X,[swim,-past| X ]).
irregular_form( swung,X,[swing,-past| X ]).
irregular_form( talcked,X,[talc,-past| X ]).
irregular_form( talcking,X,[talc,-ing| X ]).
irregular_form( taught,X,[teach,-past| X ]).
irregular_form( taxying,X,[taxi,-ing| X ]).
irregular_form( thought,X,[think,-past| X ]).
irregular_form( threw,X,[throw,-past| X ]).
irregular_form( throve,X,[thrive,-past| X ]).
irregular_form( thrown,X,[throw,-past| X ]).
irregular_form( told,X,[tell,-past| X ]).
irregular_form( took,X,[take,-past| X ]).
irregular_form( tore,X,[tear,-past| X ]).
irregular_form( torn,X,[tear,-past| X ]).
irregular_form( torrify,X,[torrefy,-past| X ]).
irregular_form( trafficked,X,[traffic,-past| X ]).
irregular_form( trafficking,X,[traffic,-ing| X ]).
irregular_form( trameled,X,[trammel,-past| X ]).
irregular_form( trameling,X,[trammel,-ing| X ]).
irregular_form( tramelled,X,[trammel,-past| X ]).
irregular_form( tramelling,X,[trammel,-ing| X ]).
irregular_form( transfixt,X,[transfix,-past| X ]).
irregular_form( trod,X,[tread,-past| X ]).
irregular_form( trodden,X,[tread,-en| X ]).
irregular_form( tying,X,[tie,-ing| X ]).
irregular_form( typewritten,X,[typewrite,-en| X ]).
irregular_form( typewrote,X,[typewrite,-past| X ]).
irregular_form( unbent,X,[unbend,-past| X ]).
irregular_form( unbound,X,[unbind,-past| X ]).
irregular_form( unclad,X,[unclothe,-past| X ]).
irregular_form( underbought,X,[underbuy,-past| X ]).
irregular_form( underfed,X,[underfeed,-past| X ]).
irregular_form( undergirt,X,[undergird,-past| X ]).
irregular_form( undergone,X,[undergo,-past| X ]).
irregular_form( underlaid,X,[underlay,-past| X ]).
irregular_form( underlain,X,[underlie,-past| X ]).
irregular_form( underlay,X,[underlie,-past| X ]).
irregular_form( underlying,X,[underlie,-ing| X ]).
irregular_form( underpaid,X,[underpay,-past| X ]).
irregular_form( undershot,X,[undershoot,-past| X ]).
irregular_form( undersold,X,[undersell,-past| X ]).
irregular_form( understood,X,[understand,-past| X ]).
irregular_form( undertook,X,[undertake,-past| X ]).
irregular_form( underwent,X,[undergo,-past| X ]).
irregular_form( underwritten,X,[underwrite,-en| X ]).
irregular_form( underwrote,X,[underwrite,-past| X ]).
irregular_form( undid,X,[undo,-past| X ]).
irregular_form( undone,X,[undo,-past| X ]).
irregular_form( unfroze,X,[unfreeze,-past| X ]).
irregular_form( unfrozen,X,[unfreeze,-en| X ]).
irregular_form( unlaid,X,[unlay,-past| X ]).
irregular_form( unlearnt,X,[unlearn,-past| X ]).
irregular_form( unmade,X,[unmake,-past| X ]).
irregular_form( unrove,X,[unreeve,-past| X ]).
irregular_form( unsaid,X,[unsay,-past| X ]).
irregular_form( unslung,X,[unsling,-past| X ]).
irregular_form( unspoke,X,[unspeak,-past| X ]).
irregular_form( unstrung,X,[unstring,-past| X ]).
irregular_form( unstuck,X,[unstick,-past| X ]).
irregular_form( unswore,X,[unswear,-past| X ]).
irregular_form( unsworn,X,[unswear,-past| X ]).
irregular_form( untaught,X,[unteach,-past| X ]).
irregular_form( unthought,X,[unthink,-past| X ]).
irregular_form( untrod,X,[untread,-past| X ]).
irregular_form( untrodden,X,[untread,-en| X ]).
irregular_form( untying,X,[untie,-ing| X ]).
irregular_form( unwound,X,[unwind,-past| X ]).
irregular_form( upbuilt,X,[upbuild,-past| X ]).
irregular_form( upheld,X,[uphold,-past| X ]).
irregular_form( uphove,X,[upheave,-past| X ]).
irregular_form( uprose,X,[uprise,-past| X ]).
irregular_form( upsprang,X,[upspring,-past| X ]).
irregular_form( upsprung,X,[upspring,-past| X ]).
irregular_form( upswept,X,[upsweep,-past| X ]).
irregular_form( upswollen,X,[upswell,-en| X ]).
irregular_form( upswung,X,[upswing,-past| X ]).
irregular_form( vying,X,[vie,-ing| X ]).
irregular_form( waylaid,X,[waylay,-past| X ]).
irregular_form( went,X,[go,-past| X ]).
irregular_form( wept,X,[weep,-past| X ]).
irregular_form( winterfed,X,[winterfeed,-past| X ]).
irregular_form( wiredrawn,X,[wiredraw,-en| X ]).
irregular_form( wiredrew,X,[wiredraw,-past| X ]).
irregular_form( withdrawn,X,[withdraw,-en| X ]).
irregular_form( withdrew,X,[withdraw,-past| X ]).
irregular_form( withheld,X,[withhold,-past| X ]).
irregular_form( withstood,X,[withstand,-past| X ]).
irregular_form( woke,X,[wake,-past| X ]).
irregular_form( woken,X,[wake,-en| X ]).
irregular_form( won,X,[win,-past| X ]).
irregular_form( wore,X,[wear,-past| X ]).
irregular_form( worn,X,[wear,-en| X ]).
irregular_form( wound,X,[wind,-past| X ]).
irregular_form( wove,X,[weave,-past| X ]).
irregular_form( woven,X,[weave,-en| X ]).
irregular_form( written,X,[write,-en| X ]).
irregular_form( wrote,X,[write,-past| X ]).
irregular_form( wrought,X,[work,-past| X ]).
irregular_form( wrung,X,[wring,-past| X ]).
irregular_form( ycleped,X,[clepe,-past| X ]).
irregular_form( yclept,X,[clepe,-past| X ]).

/****************************************************
% pronto_morph_spelling_rules.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation

% Morphological Analyzer to be used with
% ProNTo (Prolog Natural Language Toolkit),
% created at the Artificial Intelligence Center
% of The University of Georgia
****************************************************/




% *******************************************************
% COMMENT OUT SPELLING RULES THAT YOU DO NOT WANT TO USE!
% *******************************************************
% split_suffix(+Characters,-Root,-Suffix)
%
%  Splits a word into root and suffix.
%  Must line up with the end of the word
%  (that's the job of find_suffix, which calls it).
%  Fails if there is no suffix.
%  Instantiates Category to the syntactic category
%  to which this suffix attaches.
% *******************************************************

split_suffix( X , [] , S ):-
	suffix(	X , S ).

% Suffixes with doubled consonants in root
split_suffix([z,z,e,s],[z],-s).  % "quizzes", "whizzes"
split_suffix([s,s,e,s],[s],-s).  % "gasses" (verb), "crosses"
% TOO VAGUE????
split_suffix([V,C,C,V2|Rest],[V,C],Suffix) :-
   vowel(V), \+ vowel(C), vowel(V2), suffix([V2|Rest],Suffix).

% y changing to i and -s changing to -es simultaneously
split_suffix([C,i,e,s],[C,y],-s) :- \+ vowel(C).

% y changes to i after consonant, before suffix beg. w vowel
split_suffix([C,i,X|Rest],[C,y],Suffix) :-
   \+ vowel(C), \+ (X = i), suffix([_|Rest],Suffix).

% -es = -s after s (etc.)
split_suffix([s,h,e,s],[s,h],-s).
split_suffix([c,h,e,s],[c,h],-s).
split_suffix([s,e,s],[s],-s).
split_suffix([z,e,s],[z],-s).
split_suffix([x,e,s],[x],-s).

% verb spelling rules
split_suffix([e,n],[e],-en).         % (ex. forgiven --> forgive)

% -est = superlative inflection
split_suffix( [ e , s , t ] , [ e ] , -est ). % example is "finest"
split_suffix( [ C , C , e , s , t ] , [ C ] , -est ) :-  % example is "reddest"
	\+ vowel( C ).

% should this be here??
split_suffix( [ i , e , s , t ] , [ y ] , -est ). % example is "craziest"

% -er = comparative inflection
split_suffix( [ C , C , e , r ] , [ C ] , -er ) :-  % example is "redder"
	\+ vowel( C ).
split_suffix( [i , e , r ] , [ y ] , -er ). % example is "crazier"

% Final e drops out before a suffix beg. with a vowel
split_suffix([C,V|Rest],[C,e],Suffix) :-
   \+ vowel(C), vowel(V), suffix([V|Rest],Suffix).


% spelling rules for English words
% that have foriegn or scientific origins
%
split_suffix([l,v,e,s],[l,f],-pl).
split_suffix([e,a,u,x],[e,a,u],-pl).
split_suffix([i],[u,s],-pl).
split_suffix([i,a],[i,u,m],-pl).     
split_suffix([a],[u,m],-pl).         % (ex. antra --> antrum)
split_suffix([a,e],[a],-pl).         % (ex. amoebae --> amoeba)
split_suffix([s,e,s],[s,i,s],-pl).   % (ex. amniocenteses --> amniocenteses)
split_suffix([x,e,s],[x,i,s],-pl).   % (ex. apomixes apomixis)
split_suffix([i,c,e,s],[e,x],-pl).   % (ex. apices apex)
split_suffix([i,c,e,s],[i,x],-pl).   % (ex. appendices appendix)
split_suffix([i,m],[i],-pl).	     % (ex. ashkenazim ashkenazi)
split_suffix([e,s],[],-pl).          % (ex. banjoes --> banjo)
split_suffix([i,n,a],[e,n],-pl).     % (ex. praenomina --> praenomen)
split_suffix([i,a],[e],-pl).	     % (ex. qualia --> quale)
split_suffix([a,t,a],[a],-pl).       % (ex. trymata --> tryma)
split_suffix([i,a],[i,o,n],-pl).     % (ex. acromia --> acromion) 
split_suffix([a],[o,n],-pl).         % (ex. entera --> enteron)
split_suffix([v,e,s],[f,e],-pl).     % (ex. knives --> knife)
split_suffix([i],[o], -pl).          % (ex. maestri --> maestro)
split_suffix([f,e,e,t],[f,o,o,t],-pl).     % (ex. blackfeet --> blackfoot)
split_suffix([a,e],[e],-pl).               % (ex. phylae --> phyle) 
split_suffix([i],[],-pl).		   % (ex. pirogi --> pirog)
split_suffix([e,s],[i,s],-pl).             % (ex. vermes --> vermis)

fof('$o.',axiom,(
    $o ),
    introduced(language,[ level(0) ],[ ]),
    []).

fof(cat,axiom,(
    cat ),
    introduced(language,[ level(0) ],[ ]),
    []).

fof(human,axiom,(
    human ),
    introduced(language,[ level(0) ],[ ]),
    []).

thf('$o.owns',negated_conjecture,(
    owns: ( human * cat ) > $o ),
    inference(interpretation,[ level(1) ],[ '$o.' ]),
    []).

tcf('$o.owns.jon',plain,(
    jon: human ),
    inference(interpretation,[ level(2) ],[ '$o.owns' ]),
    []).

tcf('$o.owns.jon.arlene',plain,(
    arlene: cat ),
    inference(interpretation,[ level(3) ],[ '$o.owns.jon' ]),
    []).

tcf('$o.owns.jon.garfield',plain,(
    garfield: cat ),
    inference(interpretation,[ level(3) ],[ '$o.owns.jon' ]),
    []).

tcf('$o.owns.jon.nermal',plain,(
    nermal: cat ),
    inference(interpretation,[ level(3) ],[ '$o.owns.jon' ]),
    []).

thf('cat.arlene',negated_conjecture,(
    arlene: cat ),
    inference(interpretation,[ level(1) ],[ cat ]),
    []).

thf('cat.garfield',negated_conjecture,(
    garfield: cat ),
    inference(interpretation,[ level(1) ],[ cat ]),
    []).

thf('cat.loves',negated_conjecture,(
    loves: cat > cat ),
    inference(interpretation,[ level(1) ],[ cat ]),
    []).

thf('cat.nermal',negated_conjecture,(
    nermal: cat ),
    inference(interpretation,[ level(1) ],[ cat ]),
    []).

tcf('cat.loves.arlene',plain,(
    arlene: cat ),
    inference(interpretation,[ level(2) ],[ 'cat.loves' ]),
    []).

tcf('cat.loves.garfield',plain,(
    garfield: cat ),
    inference(interpretation,[ level(2) ],[ 'cat.loves' ]),
    []).

tcf('cat.loves.nermal',plain,(
    nermal: cat ),
    inference(interpretation,[ level(2) ],[ 'cat.loves' ]),
    []).

thf('human.jon',negated_conjecture,(
    jon: human ),
    inference(interpretation,[ level(1) ],[ human ]),
    []).

tcf('$true.$o',conjecture,(
    $true ),
    inference(interpretation,[ level(4) ],[ '$o.owns.jon.garfield' ]),
    []).

tcf('$false.$o',conjecture,(
    $false ),
    inference(interpretation,[ level(4) ],[ '$o.owns.jon.arlene', '$o.owns.jon.nermal' ]),
    []).

tcf('garfield.cat',conjecture,(
    garfield: cat ),
    inference(interpretation,[ level(4) ],[ 'cat.garfield', 'cat.loves.garfield', 'cat.loves.arlene', 'cat.loves.nermal' ]),
    []).

tcf('arlene.cat',conjecture,(
    arlene: cat ),
    inference(interpretation,[ level(4) ],[ 'cat.arlene' ]),
    []).

tcf('nermal.cat',conjecture,(
    nermal: cat ),
    inference(interpretation,[ level(4) ],[ 'cat.nermal' ]),
    []).

tcf('jon.human',conjecture,(
    jon: human ),
    inference(interpretation,[ level(4) ],[ 'human.jon' ]),
    []).

fof('$o.d',axiom,(
    $o ),
    inference(type,[ level(5) ],[ '$false.$o', '$true.$o' ]),
    []).

fof('cat.d',axiom,(
    cat ),
    inference(type,[ level(5) ],[ 'arlene.cat', 'garfield.cat', 'loves.cat', 'nermal.cat' ]),
    []).

fof('human.d',axiom,(
    human ),
    inference(type,[ level(5) ],[ 'jon.human' ]),
    []).

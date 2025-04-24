%----Iokaste is a parent of Oedipus.
fof(iokaste_oedipus,axiom,(
    parent_of(iokaste,oedipus) )).

%----Iokaste is a parent of Polyneikes.
fof(iokaste_polyneikes,axiom,(
    parent_of(iokaste,polyneikes) )).

%----Oedipus is a parent of Polyneikes.
fof(oedipus_polyneikes,axiom,(
    parent_of(oedipus,polyneikes) )).

%----Polyneikes is a parent of Thersandros.
fof(polyneikes_thersandros,axiom,(
    parent_of(polyneikes,thersandros) )).

%----Oedipus is a patricide.
fof(oedipus_patricidal,axiom,(
    patricide(oedipus) )).

%----Thersandros is not a patricide.
fof(thersandros_not_patricidal,axiom,(
    ~ patricide(thersandros) )).

%----Adding new axioms to constrin the problem
fof(only_people,axiom,
    ! [X] : (X = iokaste | X = oedipus | X = polyneikes | X = thersandros) ).
fof(different_people,axiom,
    ( iokaste != oedipus
    & iokaste != polyneikes
    & iokaste != thersandros
    & oedipus != polyneikes
    & oedipus != thersandros
    & polyneikes != thersandros ) ).
fof(only_parents,axiom,
    ! [P,C] :
      ( parent_of(P,C)
     => ( ( P = iokaste & C = oedipus )
        | ( P = iokaste & C = polyneikes )
        | ( P = oedipus & C = polyneikes )
        | ( P = polyneikes & C = thersandros ) ) ) ).
        

%----Prove that Iokaste is a parent of a patricide who is a parent of
%----somebody who is a patricide (original has last conjunct negated).
fof(iokaste_parent_particide_parent_patricide,conjecture,(
    ? [P,NP] :
      ( parent_of(iokaste,P)
      & patricide(P)
      & parent_of(P,NP)
      & patricide(NP) ) )).

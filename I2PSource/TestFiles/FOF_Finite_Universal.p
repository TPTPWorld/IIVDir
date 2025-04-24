fof(different_people,axiom,
    ( iokaste != oedipus & iokaste != polyneikes & iokaste != thersandros
    & oedipus != polyneikes & oedipus != thersandros
    & polyneikes != thersandros ) ).
fof(iokaste_oedipus,axiom,
    ( parent_of(iokaste,oedipus) & parent_of(iokaste,polyneikes) ) ).
fof(oedipus_polyneikes,axiom,
    parent_of(oedipus,polyneikes) ).
fof(polyneikes_thersandros,axiom,
    parent_of(polyneikes,thersandros) ).
fof(no_self_parent,axiom,
    ! [X] : ~ parent_of(X,X) ).
fof(oedipus_patricidal,axiom,
    patricide(oedipus) ).
fof(thersandros_not_patricidal,axiom,
    ~ patricide(thersandros) ).
fof(iokaste_parent_particide_parent_not_patricide,conjecture,
    ? [P,NP] :
      ( parent_of(iokaste,P) & patricide(P) & parent_of(P,NP)
      & patricide(NP) ) ).  

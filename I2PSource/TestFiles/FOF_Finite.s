fof(domain, fi_domain,
  (![X] : (X = "1" | X = "2" | X = "3" | X = "4"))
).

fof(iokaste, fi_functors,
  ( (iokaste = "3")
  )
).

fof(oedipus, fi_functors,
  ( (oedipus = "4")
  )
).

fof(parent_of, fi_predicates,
  ( (parent_of("1","1") <=> $false)
  & (parent_of("1","2") <=> $true)
  & (parent_of("1","3") <=> $false)
  & (parent_of("1","4") <=> $false)
  & (parent_of("2","1") <=> $false)
  & (parent_of("2","2") <=> $false)
  & (parent_of("2","3") <=> $false)
  & (parent_of("2","4") <=> $false)
  & (parent_of("3","1") <=> $true)
  & (parent_of("3","2") <=> $false)
  & (parent_of("3","3") <=> $false)
  & (parent_of("3","4") <=> $true)
  & (parent_of("4","1") <=> $true)
  & (parent_of("4","2") <=> $false)
  & (parent_of("4","3") <=> $false)
  & (parent_of("4","4") <=> $false)
  )
).

fof(patricide, fi_predicates,
  ( (patricide("1") <=> $false)
  & (patricide("2") <=> $false)
  & (patricide("3") <=> $false)
  & (patricide("4") <=> $true)
  )
).

fof(polyneikes, fi_functors,
  ( (polyneikes = "1")
  )
).

fof(sP1_only_parents_or, fi_predicates,
  ( (sP1_only_parents_or("1","1") <=> $false)
  & (sP1_only_parents_or("1","2") <=> $false)
  & (sP1_only_parents_or("1","3") <=> $false)
  & (sP1_only_parents_or("1","4") <=> $false)
  & (sP1_only_parents_or("2","1") <=> $true)
  & (sP1_only_parents_or("2","2") <=> $false)
  & (sP1_only_parents_or("2","3") <=> $false)
  & (sP1_only_parents_or("2","4") <=> $false)
  & (sP1_only_parents_or("3","1") <=> $false)
  & (sP1_only_parents_or("3","2") <=> $false)
  & (sP1_only_parents_or("3","3") <=> $false)
  & (sP1_only_parents_or("3","4") <=> $false)
  & (sP1_only_parents_or("4","1") <=> $false)
  & (sP1_only_parents_or("4","2") <=> $false)
  & (sP1_only_parents_or("4","3") <=> $true)
  & (sP1_only_parents_or("4","4") <=> $false)
  )
).

fof(thersandros, fi_functors,
  ( (thersandros = "2")
  )
).

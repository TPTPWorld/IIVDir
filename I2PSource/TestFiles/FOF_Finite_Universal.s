% domain size is 4
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
  & (parent_of("1","4") <=> $true)
  & (parent_of("2","1") <=> $true)
  & (parent_of("2","2") <=> $false)
  & (parent_of("2","3") <=> $true)
  & (parent_of("2","4") <=> $true)
  & (parent_of("3","1") <=> $true)
  & (parent_of("3","2") <=> $true)
  & (parent_of("3","3") <=> $false)
  & (parent_of("3","4") <=> $true)
  & (parent_of("4","1") <=> $true)
  & (parent_of("4","2") <=> $true)
  & (parent_of("4","3") <=> $true)
  & (parent_of("4","4") <=> $false)
  )
).

fof(patricide, fi_predicates,
  ( ![X1] : (patricide(X1) <=> $true)
  )
).

fof(polyneikes, fi_functors,
  ( (polyneikes = "1")
  )
).

fof(thersandros, fi_functors,
  ( (thersandros = "2")
  )
).

tff(declare_human,type,human : $tType).
tff(declare_cat,type,cat : $tType).
tff(declare_cat1,type,garfield : cat).
tff(declare_cat2,type,arlene : cat).
tff(declare_cat3,type,nermal : cat).
tff(declare_human1,type,jon : human).
tff(declare_loves,type,loves :  cat > cat).
tff(declare_owns,type,owns :  (human * cat) > $o).
%tff('declare_$i1',type,'fmb_$i_1' : $i).
%
%tff('finite_domain_$i',interpretation,
%      ! [X : $i]  :  (
%         X = 'fmb_$i_1'
%      ) ).

tff(finite_domain_human,interpretation,
      ! [X : human]  :  (
         X = jon
      ) ).

tff(finite_domain_cat,interpretation,
      ! [X : cat]  :  (
         X = garfield | X = arlene | X = nermal
      ) ).

tff(distinct_domain_cat,interpretation,
         garfield != arlene & garfield != nermal & arlene != nermal
).

tff(predicate_loves,interpretation,
    ( loves(garfield) = garfield
    & loves(arlene) = garfield
    & loves(nermal) = garfield ) ).

tff(predicate_owns,interpretation,
           owns(jon,garfield)
         & ~owns(jon,arlene)
         & ~owns(jon,nermal)

).


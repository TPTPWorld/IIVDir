tff('declare_$i1',type,'fmb_$i_1' : $i).
tff('finite_domain_$i',interpretation,
      ! [X : $i]  :  (
         X = 'fmb_$i_1'
      ) ).

tff(declare_human,type,human : $tType).
tff(declare_human1,type,john : human).
tff(declare_human2,type,fmb_human_2 : human).
tff(finite_domain_human,interpretation,
      ! [X : human]  :  (
         X = john | X = fmb_human_2
      ) ).

tff(distinct_domain_human,interpretation,
         john != fmb_human_2
).

tff(declare_grade,type,grade : $tType).
tff(declare_grade1,type,a : grade).
tff(declare_grade2,type,f : grade).
tff(finite_domain_grade,interpretation,
      ! [X : grade]  :  (
         X = a | X = f
      ) ).

tff(distinct_domain_grade,interpretation,
         a != f
).

tff(declare_grade_of,type,grade_of :  (human) > grade).
tff(function_grade_of,interpretation,
           grade_of(john) = f
         & grade_of(fmb_human_2) = a

).

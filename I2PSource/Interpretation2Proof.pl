%==================================================================================================
:-consult('TPTP2X/tptp2X.main').
:-use_module(library(qsave)).
:-use_module(library(lists)).
:-use_module(library(prolog_debug)).
%% :-set_prolog_flag(on_error,halt).
%==================================================================================================
%--------------------------------------------------------------------------------------------------
conjuntify([Conjunct],Conjunct):-
    !.

conjuntify([Conjunct|RestOfConjuncts],Conjunct & RestConjunctified):-
    conjuntify(RestOfConjuncts,RestConjunctified).

%--------------------------------------------------------------------------------------------------
deconjuntify(LHS & RHS,InterpretationFormulae):-
    !,
    deconjuntify(LHS,LHSInterpretationFormulae),
    deconjuntify(RHS,RHSInterpretationFormulae),
    append(LHSInterpretationFormulae,RHSInterpretationFormulae,InterpretationFormulae).

deconjuntify(Formula,[Formula]).

%--------------------------------------------------------------------------------------------------
convert_predicate_formula_to_literal(LHS <=> '$true',LHS):-
    !.

convert_predicate_formula_to_literal(LHS <=> '$false',~LHS):-
    !.

convert_predicate_formula_to_literal(LHS <=> RHS,Literal):-
    convert_predicate_formula_to_literal(RHS <=> LHS,Literal),
    !.

convert_predicate_formula_to_literal(Literal,Literal).

%--------------------------------------------------------------------------------------------------
convert_predicate_formulae_to_list([],[]):-
    !.

convert_predicate_formulae_to_list(FormulaConjunctionList,LiteralList):-
    conjuntify(FormulaConjunctionList,Conjunction),
    deconjuntify(Conjunction,ListOfPredicateFormulae),
    findall(PredicateLiteral,
(member(PredicateFormula,ListOfPredicateFormulae),
 convert_predicate_formula_to_literal(PredicateFormula,PredicateLiteral)),
LiteralList).

%--------------------------------------------------------------------------------------------------
%----New format with interpretation roles
convert_interpretation_to_list(InterpretationAnnotatedFormulae,InterpretationFormulae,
OtherAnnotatedFormulae):-
    member(AnInterpretationAnnotatedFormula,InterpretationAnnotatedFormulae),
    AnInterpretationAnnotatedFormula =.. [tff,_,interpretation|_],
    !,
    findall(InterpretationInterpretationFormula,
(member(InterpretationAnnotatedFormula,InterpretationAnnotatedFormulae),
 InterpretationAnnotatedFormula =.. [_,_,interpretation,Conjunction|_],
 deconjuntify(Conjunction,InterpretationInterpretationFormulae),
 member(InterpretationInterpretationFormula,InterpretationInterpretationFormulae)),
InterpretationFormulae),
    findall(OtherAnnotatedFormula,
(member(OtherAnnotatedFormula,InterpretationAnnotatedFormulae),
 \+ (OtherAnnotatedFormula =.. [_,_,interpretation|_])),
OtherAnnotatedFormulae).

%----Old format with fi_* roles
convert_interpretation_to_list(InterpretationAnnotatedFormulae,InterpretationFormulae,[]):-
    member(FOFDomainAnnotatedFormula,InterpretationAnnotatedFormulae),
    FOFDomainAnnotatedFormula =.. [fof,_,fi_domain,DomainFormula|_],
    !,
    findall(FunctorFormula,
(member(FOFFunctorsAnnotatedFormula,InterpretationAnnotatedFormulae),
 FOFFunctorsAnnotatedFormula =.. [fof,_,fi_functors,FunctorFormula|_]),
FunctorsFormulae),
    findall(PredicateFormula,
(member(FOFPredicatesAnnotatedFormula,InterpretationAnnotatedFormulae),
 FOFPredicatesAnnotatedFormula =.. [fof,_,fi_predicates,PredicateFormula|_]),
FOFPredicatesConjunctionList),
    convert_predicate_formulae_to_list(FOFPredicatesConjunctionList,PredicateLiterals),
    append([DomainFormula|FunctorsFormulae],PredicateLiterals,InterpretationFormulae).

%--------------------------------------------------------------------------------------------------
looks_like_a_variable_promotion(PVar,DVar,LHS,RHS,PromotionFunction):-
    var(PVar),
    var(DVar),
    PVar == LHS,
    RHS =.. [PromotionFunction,Var],
    DVar == Var,
    !.

looks_like_a_variable_promotion(PVar,DVar,LHS,RHS,PromotionFunction):-
    looks_like_a_variable_promotion(PVar,DVar,RHS,LHS,PromotionFunction).

%--------------------------------------------------------------------------------------------------
%----Explicit types in TF0 interpretations
extract_types(InterpretationAnnotatedFormulae,InterpretationFormulae,ProblemTypes,
DomainTypes,PromotionFunctions):-
    member(TFFAnnotatedFormula,InterpretationAnnotatedFormulae),
    TFFAnnotatedFormula =.. [tff,_,interpretation|_],
    !,
    tptp2X_findall_setof1(Type,member(tff(_,type,Type: '$tType'),InterpretationAnnotatedFormulae),
AllDeclaredTypes),
    tptp2X_findall_setof1(Type,
( ( member(tff(_,type,_: Type),InterpretationAnnotatedFormulae)
  ; member(tff(_,type,_: _ > Type),InterpretationAnnotatedFormulae) ),
 member(Type,['$i','$o','$int','$rat','$real'])),
AllDefinedTypes),
    append(AllDeclaredTypes,AllDefinedTypes,AllTypes),
%DEBUG write('All types are :'),write(AllTypes),nl,
%----Types that use promotions
    findall(ProblemType-DomainType-PromotionFunction,
(member(! [P:ProblemType] : ? [D:DomainType] : '$tptp_equal'(LHS,RHS),InterpretationFormulae),
 member(ProblemType,AllTypes),
 member(DomainType,AllTypes),
 looks_like_a_variable_promotion(P,D,LHS,RHS,PromotionFunction) ),
TypesAndPromotions),
    findall(ProblemType,member(ProblemType-_-_,TypesAndPromotions),PromotionProblemTypes),
    ( PromotionProblemTypes = [] 
   -> ProblemTypes = AllTypes 
    ; append(AllDefinedTypes,PromotionProblemTypes,ProblemTypes) ),
    findall(DomainType,member(_-DomainType-_,TypesAndPromotions),PromotionDomainTypes),
    ( PromotionDomainTypes = [] 
   -> DomainTypes = AllTypes 
    ; append(AllDefinedTypes,PromotionDomainTypes,DomainTypes) ),
    findall(PromotionFunction,member(_-_-PromotionFunction,TypesAndPromotions),PromotionFunctions).

%----Implicit types in FOF interpretations
extract_types(InterpretationAnnotatedFormulae,_,['$i','$o'],['$i','$o'],[]):-
    member(FOFAnnotatedFormula,InterpretationAnnotatedFormulae),
    FOFAnnotatedFormula =.. [fof,_,interpretation|_],
    !.

%----Implicit types in FOF old style interpretations
extract_types(InterpretationAnnotatedFormulae,_,['$i','$o'],['$i','$o'],[]):-
    member(FOFAnnotatedFormula,InterpretationAnnotatedFormulae),
    FOFAnnotatedFormula =.. [fof,_,fi_domain|_],
    !.

%--------------------------------------------------------------------------------------------------
extract_type_from_signature(ArgumentTypes > ResultType,[ArgumentTypes,ResultType]):-
    !.

extract_type_from_signature(Type,[none,Type]).

%--------------------------------------------------------------------------------------------------
extract_domain_elements_from_equalities('$tptp_equal'(Var,Element),[Element]):-
    var(Var),
    !.

extract_domain_elements_from_equalities('$tptp_equal'(Element,Var),[Element]):-
    var(Var),
    !.

extract_domain_elements_from_equalities(('$tptp_equal'(LHS,RHS)|RestOfDisjunction),Elements):-
    extract_domain_elements_from_equalities('$tptp_equal'(LHS,RHS),ElementList),
    extract_domain_elements_from_equalities(RestOfDisjunction,RestOfElements),
    append(ElementList,RestOfElements,Elements).

%-------------------------------------------------------------------------------------------------
make_args_signature(0,_,none):-
    !.

make_args_signature(1,ArgType,ArgType):-
    !.

%----This works only because all the args are the same type. Doing ArgType * RestOfSignature gets 
%----the associativity wrong.
make_args_signature(Arity,ArgType,RestOfSignature * ArgType):-
    Arity > 1,
    ReducedArity is Arity - 1,
    make_args_signature(ReducedArity,ArgType,RestOfSignature).

%-------------------------------------------------------------------------------------------------
convert_structures_to_types([],_,_,[]).

convert_structures_to_types([Symbol/Arity|RestOfStructures],ArgType,ResultType,
[Symbol-[ArgsSignature,ResultType]|RestOfTypes]):-
    make_args_signature(Arity,ArgType,ArgsSignature),
    convert_structures_to_types(RestOfStructures,ArgType,ResultType,RestOfTypes).

%-------------------------------------------------------------------------------------------------
%----The ProblemTypes and DomainTypes are lists of individual types. The ProblemSymbolTypes and
%----DomainElementTypes are lists of symbol-[argtypes,resulttype]. If there are no args it's none.
%----TF0 case where types are extracted from type role formulae
extract_symbol_types(InterpretationAnnotatedFormulae,_InterpretationFormulae,PromotionFunctions,
ProblemTypes,DomainTypes,ProblemSymbolTypes, ['$true'-[none,'$o'],'$false'-[none,'$o']|DomainElementTypes]):-
    member(TFFAnnotatedFormula,InterpretationAnnotatedFormulae),
    TFFAnnotatedFormula =.. [tff,_,interpretation|_],
    !,
    findall(ProblemSymbol-[ArgumentTypes,ResultType],
(member(tff(_,type,ProblemSymbol: Signature),InterpretationAnnotatedFormulae),
 \+ member(ProblemSymbol,PromotionFunctions),
 extract_type_from_signature(Signature,[ArgumentTypes,ResultType]),
 member(ResultType,ProblemTypes)),
ProblemSymbolTypes),
    findall(DomainElement-[ArgumentTypes,ResultType],
(member(tff(_,type,DomainElement: Signature),InterpretationAnnotatedFormulae),
 extract_type_from_signature(Signature,[ArgumentTypes,ResultType]),
%----Only $true and $false are type $o, avoid catching problem symbols.
 ResultType \= '$o',
 member(ResultType,DomainTypes)),
DomainElementTypes).

%----FOF case where types are defaulted
extract_symbol_types(InterpretationAnnotatedFormulae,InterpretationFormulae,[],_,_,
ProblemSymbolTypes,['$true'-[none,'$o'],'$false'-[none,'$o']|DomainElementTypes]):-
    member(FOFAnnotatedFormula,InterpretationAnnotatedFormulae),
    ( FOFAnnotatedFormula =.. [fof,_,fi_domain,(! [_] : Equalities)|_]
    ; ( FOFAnnotatedFormula =.. [fof,_,interpretation|_],
        member(! [_] : Equalities,InterpretationFormulae) ) ),
    !,
    extract_domain_elements_from_equalities(Equalities,DomainElements),
    examine_formulae_for_predicates(InterpretationAnnotatedFormulae,_,
PredicateStructuresWithEquals,_),
    select('$tptp_equal'/2,PredicateStructuresWithEquals,PredicateStructures),
    convert_structures_to_types(PredicateStructures,'$i','$o',PredicateTypes),
    examine_formulae_for_functors(InterpretationAnnotatedFormulae,FunctorAndDomainStructures,_),
    convert_structures_to_types(FunctorAndDomainStructures,'$i','$i',FunctorAndDomainTypes),
    findall(Element-Type,
(member(Element-Type,FunctorAndDomainTypes),
 member(Element,DomainElements)),
DomainElementTypes),
    findall(Element-Type,
(member(Element-Type,FunctorAndDomainTypes),
 \+ member(Element,DomainElements)),
FunctorTypes),
    append(FunctorTypes,PredicateTypes,ProblemSymbolTypes),
%DEBUG write('The problem types are '),writeq(ProblemSymbolTypes),nl,
true.

%--------------------------------------------------------------------------------------------------
universally_quantified_symbol_interpretation(! [Variable] : QuantifiedFormula,Variable,'$i',
LiteralTerm):-
    QuantifiedFormula =.. [Symbol,_,_],
    member(Symbol,['<=>','=','$tptp_equal']),
    convert_predicate_formula_to_literal(QuantifiedFormula,LiteralTerm),
    !.

universally_quantified_symbol_interpretation(! [Variable : Type] : QuantifiedFormula,Variable,
Type,LiteralTerm):-
    QuantifiedFormula =.. [Symbol,_,_],
    member(Symbol,['<=>','=','$tptp_equal']),
    convert_predicate_formula_to_literal(QuantifiedFormula,LiteralTerm),
    !.

%--------------------------------------------------------------------------------------------------
expand_universal_interpretations([],_,_,[]).

expand_universal_interpretations([UniversalFormula|RestInterpretationFormulae],ProblemSymbolTypes,
DomainElementTypes,ExpandedInterpretationFormulae):-
    universally_quantified_symbol_interpretation(UniversalFormula,Variable,Type,QuantifiedFormula),
    !,
    findall(QuantifiedFormula,member(Variable-[none,Type],DomainElementTypes),Instances),
    expand_universal_interpretations(RestInterpretationFormulae,ProblemSymbolTypes,
DomainElementTypes,RestOfInstances),
    append(Instances,RestOfInstances,ExpandedInterpretationFormulae).

expand_universal_interpretations([NonUniversalFormula|RestInterpretationFormulae],
ProblemSymbolTypes,DomainElementTypes,[NonUniversalFormula|RestExpandedInterpretationFormulae]):-
    expand_universal_interpretations(RestInterpretationFormulae,ProblemSymbolTypes,
DomainElementTypes,RestExpandedInterpretationFormulae).

%--------------------------------------------------------------------------------------------------
demote_all_args(PromotedArgs,PromotionFunctions,DemotedArgs):-
    findall(DemotedArg,
(member(PromotedArg,PromotedArgs),
 PromotedArg =.. [PromotionFunction,DemotedArg],
 member(PromotionFunction,PromotionFunctions)),
DemotedArgs),
    length(PromotedArgs,Arity),
    length(DemotedArgs,Arity).

%--------------------------------------------------------------------------------------------------
%----Case for FOF when there are no promotion functions
looks_like_a_function_interpretation(LHS,RHS,ProblemSymbolTypes,DomainElementTypes,[],LHS,RHS):-
    nonvar(LHS),
    nonvar(RHS),
%----RHS is a domain element
%----Check everything looks right
    member(RHS-_,DomainElementTypes),
%----LHS is a functor applied to domain elements
    LHS =.. [Functor|DomainElements],
%----Check everything looks right
    member(Functor-_,ProblemSymbolTypes),
    \+ ( member(Element,DomainElements),
         \+ member(Element-_,DomainElementTypes) ),
    !.

looks_like_a_function_interpretation(LHS,RHS,ProblemSymbolTypes,DomainElementTypes,[],Term,
DomainElement):-
%----Test to avoid some loops
    nonvar(LHS),
    member(LHS-_,DomainElementTypes),
    looks_like_a_function_interpretation(RHS,LHS,ProblemSymbolTypes,DomainElementTypes,[],Term,
DomainElement),
    !.

%----Case for TF0 when there are promotion functions
looks_like_a_function_interpretation(LHS,RHS,ProblemSymbolTypes,DomainElementTypes,
[PromotionFunctionsHead|PromotionFunctionsTail],DemotedTerm,DemotedDomainElement):-
    nonvar(LHS),
    nonvar(RHS),
%---RHS is a promotion function applied to a domain element
    RHS =.. [PromotionFunction,DemotedDomainElement],
%----Check everything looks right
    member(PromotionFunction,[PromotionFunctionsHead|PromotionFunctionsTail]),
    member(DemotedDomainElement-_,DomainElementTypes),
%----LHS is a functor applied to args that are promotion functions applied to domain elements
    LHS =.. [Functor|PromotedArgs],
%----Check everything looks right
    member(Functor-_,ProblemSymbolTypes),
    demote_all_args(PromotedArgs,[PromotionFunctionsHead|PromotionFunctionsTail],DemotedArgs),
    \+ ( member(DemotedArg,DemotedArgs),
         \+ member(DemotedArg-_,DomainElementTypes) ),
    !,
%----Build the terms without the promotion functions
    DemotedTerm =.. [Functor|DemotedArgs].

looks_like_a_function_interpretation(LHS,RHS,ProblemSymbolTypes,DomainElementTypes,
[PromotionFunctionsHead|PromotionFunctionsTail],DemotedTerm,DemotedDomainElement):-
    nonvar(LHS),
    LHS =.. [PromotionFunction,DemotedDomainElement],
%----Check everything looks right
    member(PromotionFunction,[PromotionFunctionsHead|PromotionFunctionsTail]),
    looks_like_a_function_interpretation(RHS,LHS,ProblemSymbolTypes,DomainElementTypes,
[PromotionFunctionsHead|PromotionFunctionsTail],DemotedTerm,DemotedDomainElement).

%--------------------------------------------------------------------------------------------------
%----Case of problem types and symbols are used for interpretation types and symbols. Cannot cut
%----but happily, because this is called inside a findall, it allows a mixture.
%----Constants are interpreted as themselves
extract_function_list(_InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,[],
[ResultType,Symbol,Symbol,ResultType]):-
    member(Symbol-[none,ResultType],ProblemSymbolTypes),
    member(Symbol-[none,ResultType],DomainElementTypes).

%----Functions are interpreted as specified
extract_function_list(InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,[],
FunctionInterpretationList):-
    member(Symbol-[ArgumentTypes,ResultType],ProblemSymbolTypes),
    member(Symbol-[ArgumentTypes,ResultType],DomainElementTypes),
    ArgumentTypes \= none,
    member(SymbolInterpretation,InterpretationFormulae),
    SymbolInterpretation =.. ['$tptp_equal',LHS,RHS],
    LHS =.. [Symbol|DomainArgs],
    append([ResultType,Symbol|DomainArgs],[RHS,ResultType],FunctionInterpretationList).

%----Case when promotion functions are used
extract_function_list(InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,
[PromotionFunction|RestPromotionFunctions],FunctionInterpretationList):-
    member('$tptp_equal'(LHS,RHS),InterpretationFormulae),
    looks_like_a_function_interpretation(LHS,RHS,ProblemSymbolTypes,DomainElementTypes,
[PromotionFunction|RestPromotionFunctions],Term,DomainElement),
    Term =.. [Functor|Args],
    member(Functor-[_,FunctorType],ProblemSymbolTypes),
    member(DomainElement-[_,DomainType],DomainElementTypes),
    append([FunctorType,Functor|Args],[DomainElement,DomainType],FunctionInterpretationList).
    
%--------------------------------------------------------------------------------------------------
%----Case for a negative literal, is interpreted as $false
looks_like_a_predicate_interpretation(~ PromotedAtom,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,DemotedAtom,'$false'):-
    !,
    looks_like_a_predicate_interpretation(PromotedAtom,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,DemotedAtom,'$true').

%----Case for a FOF atom, is interpreted as $true
looks_like_a_predicate_interpretation(Atom,ProblemSymbolTypes,DomainElementTypes,[],Atom,'$true'):-
    Atom =.. [Predicate|DomainElements],
%----Check everything looks right
    member(Predicate-_,ProblemSymbolTypes),
    \+ ( member(Element,DomainElements),
         \+ member(Element-_,DomainElementTypes) ),
    !.

%----Case for a TF0 atom, is interpreted as $true
looks_like_a_predicate_interpretation(PromotedAtom,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,DemotedAtom,'$true'):-
    PromotedAtom =.. [Predicate|PromotedArgs],
%----Check everything looks right
    member(Predicate-_,ProblemSymbolTypes),
    demote_all_args(PromotedArgs,PromotionFunctions,DemotedArgs),
%----Check everything looks right
    \+ ( member(DemotedArg,DemotedArgs),
         \+ member(DemotedArg-_,DomainElementTypes) ),
    DemotedAtom =.. [Predicate|DemotedArgs].
    
%--------------------------------------------------------------------------------------------------
%----Case when promotion functions are used
extract_predicate_list(InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,PredicateInterpretationList):-
    member(Literal,InterpretationFormulae),
    looks_like_a_predicate_interpretation(Literal,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,Atom,TruthValue),
    Atom =.. [Predicate|Args],
    member(Predicate-[_,PredicateType],ProblemSymbolTypes),
    append([PredicateType,Predicate|Args],[TruthValue,'$o'],PredicateInterpretationList).

%--------------------------------------------------------------------------------------------------
extract_interpretation_lists(InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,
PromotionFunctions,InterpretationLists):-
    findall(InterpretationList,extract_function_list(InterpretationFormulae,ProblemSymbolTypes,
DomainElementTypes,PromotionFunctions,InterpretationList),FunctionLists),
%DEBUG write('Function  lists are : '),writeq(FunctionLists),nl,
    findall(InterpretationList,extract_predicate_list(InterpretationFormulae,ProblemSymbolTypes,
DomainElementTypes,PromotionFunctions,InterpretationList),PredicateLists),
%DEBUG write('Predicate lists are : '),writeq(PredicateLists),nl,
    append(FunctionLists,PredicateLists,InterpretationLists).

%--------------------------------------------------------------------------------------------------
fix_type_names('$i',Domain,Name):-
    !,
    make_node_name(['$i',Domain],0,Name).

fix_type_names('$o',Domain,Name):-
    !,
    make_node_name(['$o',Domain],0,Name).

fix_type_names(TypeName,Domain,Name):-
    !,
    make_node_name([TypeName,Domain],-1,Name).

%----Used for leaf types - their names get a '.' if $i or $o, otherwise keep the type name
fix_type_names('$i','$i.'):-
    !.

fix_type_names('$o','$o.'):-
    !.

fix_type_names(Name,Name).

%--------------------------------------------------------------------------------------------------
make_node_name([Type],0,TypeDot):-
    !,
    fix_type_names(Type,TypeDot).

%----What a hack
make_node_name(['$i.'|Rest],_Level,Name):-
    !,
    atomic_list_concat(['$i'|Rest],'.',Name).

make_node_name(['$o.'|Rest],_Level,Name):-
    !,
    atomic_list_concat(['$o'|Rest],'.',Name).

make_node_name(List,_Level,Name):-
    !,
    atomic_list_concat(List,'.',Name).

%--------------------------------------------------------------------------------------------------
%----Can't specify the type for $true and $false
fix_defined_type_formula(Formula:'$o',Formula):-
    ( Formula = '$true' ; Formula = '$false'),
    !.

fix_defined_type_formula(Formula:'$i',Atom: '$i'):-
    string(Formula),
    !,
    string_chars(Formula,Characters),
    append(['"'|Characters],['"'],QuotedStringChars),
    string_chars(QuotedFormula,QuotedStringChars),
    atom_string(Atom,QuotedFormula).

fix_defined_type_formula(Formula:Type,Formula:Type):-
    !.

fix_defined_type_formula(Formula,Formula).

%--------------------------------------------------------------------------------------------------
fix_type_signature_for_formula(Formula:[none,ResultType],Formula:ResultType,Formula,ResultType):-
    !.

fix_type_signature_for_formula(Formula:[ArgTypes,ResultType],Formula:((ArgTypes) > ResultType),
Formula,ResultType):-
    !.

fix_type_signature_for_formula(Symbol:Type,Symbol:Type,Symbol,Type):-
    !.

fix_type_signature_for_formula(Type,Type,Type,Type).

%--------------------------------------------------------------------------------------------------
%----Case for leaf types
make_proof_node(Type,Language,_,Role,SourceType,Rule,0,AnnotatedFormula):-
    !,
    make_node_name([Type],0,Name),
    Source =.. [SourceType,Rule,[level(0)],[]],
    AnnotatedFormula =.. [Language,Name,Role,Type,Source].
    
%----Case for a symbol
make_proof_node(Formula:Type,Language,ParentName,Role,SourceType,Rule,Level,AnnotatedFormula):-
    Level = 1,
    !,
    fix_type_signature_for_formula(Formula:Type,TypedFormula,Symbol,_),
    make_node_name([ParentName,Symbol],1,Name),
    fix_defined_type_formula(TypedFormula,FixedFormula),
    Source =.. [SourceType,Rule,[level(Level)],[ParentName]],
    AnnotatedFormula =.. [Language,Name,Role,FixedFormula,Source].
    
%----Case of domain elements with no parents
make_proof_node(Formula:Type,Language,[],Role,SourceType,Rule,Level,AnnotatedFormula):-
    Level > 1,
    !,
    fix_type_signature_for_formula(Formula:Type,TypedFormula,Symbol,ResultType),
    make_node_name([Symbol,ResultType],0,Name),
    fix_defined_type_formula(TypedFormula,FixedFormula),
    Source =.. [SourceType,Rule,[level(Level)],[]],
    AnnotatedFormula =.. [Language,Name,Role,FixedFormula,Source].
    
%----Case for domain elements with parents
make_proof_node(Formula:Type,Language,[ParentName|MoreParentNames],Role,SourceType,Rule,Level,
AnnotatedFormula):-
    Level > 1,
    !,
    fix_type_signature_for_formula(Formula:Type,TypedFormula,Symbol,ResultType),
    make_node_name([Symbol,ResultType],0,Name),
    fix_defined_type_formula(TypedFormula,FixedFormula),
    Source =.. [SourceType,Rule,[level(Level)],[ParentName|MoreParentNames]],
    AnnotatedFormula =.. [Language,Name,Role,FixedFormula,Source].

%----Case for symbol args
make_proof_node(Formula:Type,Language,ParentName,Role,SourceType,Rule,Level,AnnotatedFormula):-
    Level > 1,
    !,
    fix_type_signature_for_formula(Formula:Type,TypedFormula,Symbol,_),
    make_node_name([ParentName,Symbol],Level,Name),
    fix_defined_type_formula(TypedFormula,FixedFormula),
    Source =.. [SourceType,Rule,[level(Level)],[ParentName]],
    AnnotatedFormula =.. [Language,Name,Role,FixedFormula,Source].
    
%----Case for domain types at the bottom
make_proof_node(Type,Language,[ParentName1|RestOfParentNames],Role,SourceType,Rule,Level,
AnnotatedFormula):-
    Level > 1,
    !,
    fix_type_signature_for_formula(Type,TypedFormula,_Symbol,_), 
%----Last line does nothing right now, see SHITTO(1)
    fix_type_names(Type,d,NameForType),
    fix_defined_type_formula(TypedFormula,FixedFormula),
%    make_node_name([Symbol],0,Name),
    Source =.. [SourceType,Rule,[level(Level)],[ParentName1|RestOfParentNames]],
    AnnotatedFormula =.. [Language,NameForType,Role,FixedFormula,Source].
    
%--------------------------------------------------------------------------------------------------
make_leaf_nodes(Formulae,AnnotatedFormulae):-
    findall(AnnotatedFormula,
(member(Formula,Formulae),
 make_proof_node(Formula,fof,_,axiom,introduced,language,0,AnnotatedFormula)),
AnnotatedFormulae).

%--------------------------------------------------------------------------------------------------
combine_prefix_triples([],[],[],0).

combine_prefix_triples([PrefixAnnotatedFormula-NextPrefix-NextLevel|RestPrefixTriples],
[PrefixAnnotatedFormula|RestAnnotatedFormulae],[NextPrefix|RestNextPrefixes],
PrefixInterpretationLevel):-
    combine_prefix_triples(RestPrefixTriples,RestAnnotatedFormulae,RestNextPrefixes,
RestInterpretationLevel),
    PrefixInterpretationLevel is max(NextLevel,RestInterpretationLevel).

%--------------------------------------------------------------------------------------------------
make_term_nodes([],_,_,_Level,[],0).

%----AllSymbolTypes is a list of symbol-[argtypes,resulttype]
make_term_nodes([Prefix|RestPrefixes],InterpretationLists,AllSymbolTypes,Level,
TermAnnotatedFormulae,InterpretationLevel):-
%DEBUG write('------------- make term nodes for: '),write(Prefix),nl,
    ParentLevel is Level - 1,
    tptp2X_findall_setof1(ParentName-SymbolForNode,
%----Get an interpretation list
(member(InterpretationList,InterpretationLists),
%----Pull apart only if the list starts with the type under consideration
 append(Prefix,[SymbolForNode|AfterPrefix],InterpretationList),
%----Check there is a symbol and domain element and its type
 length(AfterPrefix,AfterPrefixLength),
 AfterPrefixLength >= 2,
%----If the prefix is a list of the type then make a leaf name, else a parent name
 ( Prefix = [Type]
-> make_node_name(Prefix,0,ParentName)
 ; make_node_name(Prefix,ParentLevel,ParentName))),
NextPairs),
%DEBUG write('%----Made the parent name for symbol pairs: '),nl,
%DEBUG write(NextPairs),nl,
    findall(PrefixAnnotatedFormula-NextPrefix-NextLevel,
(member(ParentName-SymbolForNode,NextPairs),
 member(SymbolForNode-Type,AllSymbolTypes),
 (Level > 1
-> ( Role = plain,
     Language = tcf )
 ; ( Role = negated_conjecture,
     Language = thf )),
 make_proof_node(SymbolForNode:Type,Language,ParentName,Role,inference,interpretation,Level,
PrefixAnnotatedFormula),
 NextLevel is Level + 1,
 append(Prefix,[SymbolForNode],NextPrefix)),
PrefixTriples),
%DEBUG write('%----Made the prefix triples: '),nl,
%DEBUG write(PrefixTriples),nl,
    combine_prefix_triples(PrefixTriples,PrefixAnnotatedFormulae,NextPrefixes,
PrefixInterpretationLevel),
    NextLevel is Level + 1,
    make_term_nodes(NextPrefixes,InterpretationLists,AllSymbolTypes,NextLevel,
NextTermAnnotatedFormulae,NextInterpretationLevel),
    append(PrefixAnnotatedFormulae,NextTermAnnotatedFormulae,ThisAnnotatedFormulae),
    ThisInterpretationLevel is max(PrefixInterpretationLevel,NextInterpretationLevel),
    make_term_nodes(RestPrefixes,InterpretationLists,AllSymbolTypes,Level,
RestTermAnnotatedFormulae,RestInterpretationLevel),
    append(ThisAnnotatedFormulae,RestTermAnnotatedFormulae,TermAnnotatedFormulae),
    InterpretationLevel is max(ThisInterpretationLevel,RestInterpretationLevel).

%--------------------------------------------------------------------------------------------------
make_domain_element_nodes(InterpretationLists,DomainElementTypes,Level,
DomainElementAnnotatedFormulae):-
    ParentLevel is Level - 1,
    findall(DomainElementAnnotatedFormula,
(member(Element-[none,Type],DomainElementTypes),
 findall(ParentName,
   (member(InterpretationList,InterpretationLists),
    append(Prefix,[Element,Type],InterpretationList),
    make_node_name(Prefix,ParentLevel,ParentName)),
   ParentNames),
 make_proof_node(Element:Type,tcf,ParentNames,conjecture,inference,interpretation,Level,
DomainElementAnnotatedFormula)),
DomainElementAnnotatedFormulae).

%--------------------------------------------------------------------------------------------------
make_domain_type_nodes(DomainElementTypes,Level,DomainTypeAnnotatedFormulae):-
    tptp2X_findall_setof1(Type,member(_-[_,Type],DomainElementTypes),DomainTypes),
    findall(DomainTypeAnnotatedFormula,
(member(Type,DomainTypes),
 tptp2X_findall_setof1(ParentName,
%----SHITTO(1) would like to have domain element types with arguments, but fof won't allow it.
   (member(Element-[_,Type],DomainElementTypes),
    make_node_name([Element,Type],0,ParentName)),
   ParentNames),
 make_proof_node(Type,fof,ParentNames,axiom,inference,type,Level,DomainTypeAnnotatedFormula)),
DomainTypeAnnotatedFormulae).

%   make_formula_with_type(ArgumentTypes,Type,Formula),
%--------------------------------------------------------------------------------------------------
make_nodes_from_interpretation_lists(InterpretationLists,ProblemSymbolTypes,DomainElementTypes,
ProofAnnotatedFormulae):-
%DEBUG write('%----Make nodes for: '),nl,
%DEBUG write(InterpretationLists),nl,
    tptp2X_findall_setof1(Type,member(_-[_,Type],ProblemSymbolTypes),ProblemTypes),
    make_leaf_nodes(ProblemTypes,ProblemTypeAnnotatedFormulae),
%DEBUG write('%----Leaf nodes are: '),nl,
%DEBUG output_tptp_formulae(tptp,long,ProblemTypeAnnotatedFormulae),nl,
    tptp2X_findall_setof1([Type],member(_-[_,Type],ProblemSymbolTypes),SymbolTypePrefixes),
%DEBUG write('%----Type prefixes are: '),nl,
%DEBUG write(SymbolTypePrefixes),nl,
    tptp2X_findall_setof1(SymbolType,
( member(SymbolType,ProblemSymbolTypes) ; member(SymbolType,DomainElementTypes) ),
AllSymbolTypes),
    make_term_nodes(SymbolTypePrefixes,InterpretationLists,AllSymbolTypes,1,TermAnnotatedFormulae,
InterpretationLevel),
%DEBUG write('%----Term nodes are: '),nl,
%DEBUG output_tptp_formulae(tptp,long,TermAnnotatedFormulae),nl,
%DEBUG write('The level for domain elements is '),write(InterpretationLevel),nl,
    append(ProblemTypeAnnotatedFormulae,TermAnnotatedFormulae,InterpretationAnnotatedFormulae),
    make_domain_element_nodes(InterpretationLists,DomainElementTypes,InterpretationLevel,
DomainElementAnnotatedFormulae),
%DEBUG write('%----Domain elements are: '),nl,
%DEBUG output_tptp_formulae(tptp,long,DomainElementAnnotatedFormulae),nl,
    DomainTypesLevel is InterpretationLevel + 1,
    make_domain_type_nodes(DomainElementTypes,DomainTypesLevel,DomainTypeAnnotatedFormulae),
%DEBUG write('%----Domain types are: '),nl,
%DEBUG output_tptp_formulae(tptp,long,DomainTypeAnnotatedFormulae),nl,
    append(DomainElementAnnotatedFormulae,DomainTypeAnnotatedFormulae,DomainAnnotatedFormulae),
    append(InterpretationAnnotatedFormulae,DomainAnnotatedFormulae,ProofAnnotatedFormulae).

%--------------------------------------------------------------------------------------------------
ensure_IIV_compatible(_ProblemTypes,_DomainTypes,[_|_]).

%--------------------------------------------------------------------------------------------------
convert_interpretation_to_proof(InterpretationAnnotatedFormulae,ProofAnnotatedFormulae):-
    convert_interpretation_to_list(InterpretationAnnotatedFormulae,InterpretationFormulae,
_OtherAnnotatedFormulae),
%DEBUG write('The formula list is'),nl,writeq(InterpretationFormulae),nl,
    extract_types(InterpretationAnnotatedFormulae,InterpretationFormulae,ProblemTypes,DomainTypes,
PromotionFunctions),
%DEBUG write('The formulae      are '),nl,writeq(InterpretationFormulae),nl,
%DEBUG write('The problem types are '),nl,writeq(ProblemTypes),nl,
%DEBUG write('The domain  types are '),nl,writeq(DomainTypes),nl,
%DEBUG write('The promotions    are '),nl,writeq(PromotionFunctions),nl,
    ensure_IIV_compatible(ProblemTypes,DomainTypes,PromotionFunctions),
    extract_symbol_types(InterpretationAnnotatedFormulae,InterpretationFormulae,PromotionFunctions,
ProblemTypes,DomainTypes,ProblemSymbolTypes,DomainElementTypes),
%DEBUG write('The problem symbol types are '),nl,writeq(ProblemSymbolTypes),nl,
%DEBUG write('The domain element types are '),nl,writeq(DomainElementTypes),nl,
    expand_universal_interpretations(InterpretationFormulae,ProblemSymbolTypes,DomainElementTypes,
ExpandedInterpretationFormulae),
%DEBUG write('The expanded interpretation formulae are '),write(ExpandedInterpretationFormulae),nl,
    extract_interpretation_lists(ExpandedInterpretationFormulae,ProblemSymbolTypes,
DomainElementTypes,PromotionFunctions,InterpretationLists),
%DEBUG write('Interpretations are : '),write(InterpretationLists),nl,
    make_nodes_from_interpretation_lists(InterpretationLists,ProblemSymbolTypes,DomainElementTypes,
ProofAnnotatedFormulae).
    
%--------------------------------------------------------------------------------------------------
remove_interpretation_subroles(AnnotatedFormulae,TypeFormula):-
    member(TypeFormula,AnnotatedFormulae),
    TypeFormula =.. [tff,_,type|_].

remove_interpretation_subroles(AnnotatedFormulae,InterpretationFormula):-
    member(FirstAnnotatedFormula,AnnotatedFormulae),
    FirstAnnotatedFormula =.. [tff,Name,Role|OtherStuff],
    ( Role = interpretation
    ; Role = (interpretation - _) ),
    InterpretationFormula =.. [tff,Name,interpretation|OtherStuff].
%--------------------------------------------------------------------------------------------------
convert_interpretation_file(InterpretationFile):-
    read_formulae_from_file(InterpretationFile,AnnotatedFormulae,_),
    findall(InterpretationFormula,
    remove_interpretation_subroles(AnnotatedFormulae,InterpretationFormula),InterpretationFormulae),
%DEBUG write('InterpretationFormulae: '),write(InterpretationFormulae),nl,
    convert_interpretation_to_proof(InterpretationFormulae,ProofAnnotatedFormulae),
    write('% SZS status Success for '),write(InterpretationFile),nl,
    write('% SZS output start ListOfFormulae for '),write(InterpretationFile),nl,
    output_tptp_formulae(tptp,long,ProofAnnotatedFormulae),nl,
    write('% SZS output end ListOfFormulae for '),write(InterpretationFile),nl,
    !,
    true.

convert_interpretation_file(InterpretationFile):-
    write('% SZS status NoSuccess for '),write(InterpretationFile),nl.

%--------------------------------------------------------------------------------------------------
run:-
    current_prolog_flag(argv,[InterpretationFile]),
%DEBUG write(InterpretationFile),nl,
    convert_interpretation_file(InterpretationFile).

run:-
    write('Usage:'),nl.

%--------------------------------------------------------------------------------------------------
save:-
    set_prolog_flag(verbose,silent),
    qsave_program('Interpretation2Proof',[stand_alone(true),op(save),toplevel(run)]).
%--------------------------------------------------------------------------------------------------
%==================================================================================================

% Verificación inicial: la fórmula debe ser compuesta, si no lo es, se lanza un error
% Este predicado evita evaluar términos sueltos o mal construidos
evaluacion(Formula, _) :-
    (\+ compound(Formula) -> throw(error('El término no es compuesto: ~w', [Formula])), true).

% ----------------------------------------------
% CASO 1: FÓRMULAS ATÓMICAS (predicados simples)
% ----------------------------------------------
evaluacion(Formula, Valor) :-
    functor(Formula, Predicado, Aridad),
    Formula =.. [Predicado | Argumentos],
    interpretacion(Predicado, Aridad, PredicadoInt),
    valoracion_lista(Argumentos, ArgumentosInt),
    PredicadoEv =.. [PredicadoInt | ArgumentosInt],
    (\+ ground(Formula) ->
        throw(error('La fórmula no es cerrada: ~w', [Formula]));
        true),
    call(PredicadoEv, Valor).

% ----------------------------------------------
% CASO 2: FÓRMULAS LÓGICAS CON OPERADORES
% (~, /\, \/, =>, <=>)
% ----------------------------------------------
evaluacion(Formula, Valor) :-
    functor(Formula, Operador, Aridad),
    Formula =.. [Operador | Argumentos],
    operador(Operador, Aridad, OperadorInt),
    evaluacion_lista(Argumentos, ArgumentosEv),
    FormulaEv =.. [OperadorInt | ArgumentosEv],
    (\+ ground(Formula) ->
        throw(error('La fórmula no es cerrada: ~w', [Formula]));
        true),
    call(FormulaEv, Valor).

% ----------------------------------------------
% CASO 3: CUANTIFICADOR UNIVERSAL (forAll) "PARA TODO"
% ----------------------------------------------
evaluacion(Formula, Valor) :-
    Formula =.. [forAll, Variable, Subformula],
    ((at_least_one_valor(Variable, Subformula, f),
        Valor = f, !);
        Valor = v, !).

% ----------------------------------------------
% CASO 4: CUANTIFICADOR EXISTENCIAL (exists) "EXISTE"
% ----------------------------------------------
evaluacion(Formula, Valor) :-
    Formula =.. [exists, Variable, Subformula],
    (at_least_one_valor(Variable, Subformula, v),
        Valor = v, !;   
        Valor = f, !).

% --------------------------------------------------------------------
% Evaluacion interna del cuantificador:
% Recorre el dominio para encontrar una asignación de Var tal que la
% fórmula evaluada con ella tenga el valor buscado.
% --------------------------------------------------------------------
at_least_one_valor(Variable, Formula, Valor) :-
    valoracion(Variable, ElementoDominio),
    copy_term([Variable], Formula, [VariableCopiada], FormulaCopiada),
    VariableCopiada = ElementoDominio,
    evaluacion(FormulaCopiada, Valor), !.


% --------------------------------------------------------------------
% evaluacion_lista(+Argumentos, -ListaValores)
% Evalúa recursivamente una lista de subformulaórmulas
% --------------------------------------------------------------------
evaluacion_lista([], []).
evaluacion_lista([Cabeza | Cola], [CabezaEvaluada | ColaEvaluada]) :-
    evaluacion(Cabeza, CabezaEvaluada),
    evaluacion_lista(Cola, ColaEvaluada).


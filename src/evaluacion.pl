% --------------------------------------------------
% EVALUACIÓN DE FÓRMULAS LÓGICAS
% --------------------------------------------------
:- discontiguous interpretacion/3.
:- multifile interpretacion/3.
:- dynamic operador/3.
:- [formulas].              % Definición de operadores
:- [valoracion].            % Definición de valoración


% Verificación inicial: la fórmula debe ser compuesta, si no lo es, se lanza un error
% Este predicado evita evaluar términos sueltos o mal construidos
evaluacion(Formula, _) :-
    (\+ compound(Formula) ->
        throw(error('El término no es compuesto: ~w', [Formula])), true).

% ----------------------------------------------
% CASO 1: FÓRMULAS ATÓMICAS (predicados simples)
% ----------------------------------------------
evaluacion(FormulaAtomica, ValorVerdad) :-
    functor(FormulaAtomica, Predicado, Aridad),
    FormulaAtomica =.. [Predicado | Argumentos],
    interpretacion(Predicado, Aridad, PredicadoInterpretado),
    valoracion_lista(Argumentos, ArgumentosInterpretados),
    PredicadoEvaluado =.. [PredicadoInterpretado | ArgumentosInterpretados],
    (\+ ground(FormulaAtomica) ->
        throw(error('La fórmula no es cerrada: ~w', [FormulaAtomica]));
        true),
    ( call(PredicadoEvaluado, ValorVerdad) ->
        true
    ; ValorVerdad = f  % <- Si no hay hecho, entonces es falso
    ).

% ----------------------------------------------
% CASO 2: FÓRMULAS LÓGICAS CON OPERADORES
% (~, /\, \/, =>, <=>)
% ----------------------------------------------
evaluacion(FormulaLogica, ValorVerdad) :-
    functor(FormulaLogica, Operador, Aridad),
    FormulaLogica =.. [Operador | Subformulas],
    operador(Operador, Aridad, OperadorInterpretado),
    evaluacion_lista(Subformulas, Subvalores),
    FormulaEvaluada =.. [OperadorInterpretado | Subvalores],
    (\+ ground(FormulaLogica) ->
        throw(error('La fórmula no es cerrada: ~w', [FormulaLogica]));
        true),
    call(FormulaEvaluada, ValorVerdad).

% ----------------------------------------------
% CASO 3: CUANTIFICADOR UNIVERSAL (forAll) "PARA TODO"
% ----------------------------------------------
evaluacion(Formula, ValorVerdad) :-
    Formula =.. [forAll, Variable, Subformula],
    ( at_least_one_valor(Variable, Subformula, f) ->
        ValorVerdad = f, !
    ;   ValorVerdad = v, !
    ).

% ----------------------------------------------
% CASO 4: CUANTIFICADOR EXISTENCIAL (exists) "EXISTE"
% ----------------------------------------------
evaluacion(Formula, ValorVerdad) :-
    Formula =.. [exists, Variable, Subformula],
    ( at_least_one_valor(Variable, Subformula, v) ->
        ValorVerdad = v, !
    ;   ValorVerdad = f, !
    ).

% --------------------------------------------------------------------
% Evaluacion interna del cuantificador:
% Recorre el dominio para encontrar una asignación de Var tal que la
% fórmula evaluada con ella tenga el valor buscado.
% --------------------------------------------------------------------
at_least_one_valor(Variable, Formula, _) :-
    valoracion(Variable, ElementoDominio),
    copy_term([Variable], Formula, [VariableCopiada], FormulaCopiada),
    VariableCopiada = ElementoDominio,
    evaluacion(FormulaCopiada, _), !.

% --------------------------------------------------------------------
% evaluacion_lista(+Subformulas, -ListaValores)
% Evalúa recursivamente una lista de subfórmulas
% --------------------------------------------------------------------
evaluacion_lista([], []).
evaluacion_lista([Cabeza | Cola], [CabezaEvaluada | ColaEvaluada]) :-
    evaluacion(Cabeza, CabezaEvaluada),
    evaluacion_lista(Cola, ColaEvaluada).


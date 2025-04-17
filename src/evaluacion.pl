% --------------------------------------------------
% EVALUACIÓN DE FÓRMULAS LÓGICAS
% --------------------------------------------------

:- discontiguous interpretacion/3.
:- multifile interpretacion/3.
:- dynamic interpretacion_operadores/3.

% Definición de operadores lógicos
:- op(300, fy, [~]).    % Negación
:- op(400, yfx, [^]).   % Conjunción
:- op(450, yfx, [\/]).  % Disyunción
:- op(700, xfy, [=>]).  % Implicación
:- op(700, xfy, [<=>]). % Doble implicación (bicondicional)

% Inclusión de definiciones externas
:- [formulas].              % Definición de operadores
:- [valoracion].            % Definición de valoración


% Verificación inicial: la fórmula debe ser compuesta
% Si no lo es, se lanza un error
% Este predicado evita evaluar términos sueltos o mal construidos
% como constantes sin argumentos ni operadores
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
% (~, ^, \/, =>, <=>)
% ----------------------------------------------
evaluacion(FormulaLogica, ValorVerdad) :-
    functor(FormulaLogica, Operador, Aridad),
    FormulaLogica =.. [Operador | Subformulas],
    interpretacion_operadores(Operador, Aridad, OperadorInterpretado),
    evaluacion_lista(Subformulas, Subvalores),
    FormulaEvaluada =.. [OperadorInterpretado | Subvalores],
    (\+ ground(FormulaLogica) ->
        throw(error('La fórmula no es cerrada: ~w', [FormulaLogica]));
        true),
    call(FormulaEvaluada, ValorVerdad).

% ----------------------------------------------
% CASO 3: CUANTIFICADOR UNIVERSAL (forAll) "PARA TODO"
% ----------------------------------------------
evaluacion(forAll(Variable, Subformula), ValorVerdad) :-
    ( \+ at_least_one_valor(Variable, Subformula, falso) ->
        ValorVerdad = verdadero
    ;   ValorVerdad = falso ).

% ----------------------------------------------
% CASO 4: CUANTIFICADOR EXISTENCIAL (exists) "EXISTE"
% ----------------------------------------------
evaluacion(exists(Variable, Subformula), ValorVerdad) :-
    ( at_least_one_valor(Variable, Subformula, verdadero) ->
        ValorVerdad = verdadero
    ;   ValorVerdad = falso ).

% --------------------------------------------------------------------
% Evaluacion interna del cuatificador:
% Recorre el dominio para encontrar una asignación de Var tal que la
% fórmula evaluada con ella tenga el valor buscado.
% --------------------------------------------------------------------
at_least_one_valor(Variable, Formula, ValorBuscado) :-
    valoracion(Variable, ElementoDominio),
    copy_term([Variable], Formula, [VariableCopiada], FormulaCopiada),
    VariableCopiada = ElementoDominio,
    evaluacion(FormulaCopiada, ValorEvaluado),
    ValorEvaluado == ValorBuscado.

% --------------------------------------------------------------------
% evaluacion_lista(+Subformulas, -ListaValores)
% Evalúa recursivamente una lista de subfórmulas
% --------------------------------------------------------------------
evaluacion_lista([], []).
evaluacion_lista([Cabeza | Cola], [CabezaEvaluada | ColaEvaluada]) :-
    evaluacion(Cabeza, CabezaEvaluada),
    evaluacion_lista(Cola, ColaEvaluada).


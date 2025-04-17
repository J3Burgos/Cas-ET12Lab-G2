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
:- [operadores].            % Definición de operadores
:- [valoracion].            % Definición de valoración

% Las constantes se interpretan como sí mismas
interpretacion(SimboloConstante, 0, SimboloConstante).


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
    call(PredicadoEvaluado, ValorVerdad).

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


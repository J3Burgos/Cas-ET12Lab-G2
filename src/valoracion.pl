% ------------------------------
% VALORACIÓN DE TÉRMINOS
% ------------------------------

:- discontiguous interpretacion/3.
:- multifile interpretacion/3.
:- dynamic dominio/1.

interpretacion(X, 0, X).

% --------------------------------------------------
% valoracion(+Termino, -ValorInterpretado)
% CASO 1: Variables — se extraen valores del dominio
% Si el término es una variable, se asocia a todos los posibles
% valores del dominio.
% --------------------------------------------------
valoracion(Variable, ValorInterpretado) :-
    var(Variable),
    dominio(Dominio),
    member(ValorInterpretado, Dominio).

% --------------------------------------------------
% CASO 2: Constantes — se interpretan directamente
% Si es una constante (átomo), se busca su interpretación.
% --------------------------------------------------
valoracion(Constante, ValorInterpretado) :-
    atomic(Constante),
    interpretacion(Constante, 0, ValorInterpretado).

% --------------------------------------------------
% CASO 3: Términos compuestos (funciones con argumentos)
% Se evalúa recursivamente cada argumento y luego se aplica
% la función correspondiente definida en la interpretación.
% --------------------------------------------------
valoracion(TerminoCompuesto, ValorInterpretado) :-
    compound(TerminoCompuesto),
    functor(TerminoCompuesto, NombreFunctor, NumeroArgumentos),
    TerminoCompuesto =.. [NombreFunctor | Argumentos],
    interpretacion(NombreFunctor, NumeroArgumentos, FunctorInterpretado),
    valoracion_lista(Argumentos, ArgumentosInterpretados),
    FuncionAplicada =.. [FunctorInterpretado | ArgumentosInterpretados],
    call(FuncionAplicada, ValorInterpretado).

% --------------------------------------------------
% valoracion_lista(+ListaTerminos, -ListaValoresInterpretados)
% Evalúa una lista de términos lógicos.
% --------------------------------------------------
valoracion_lista([], []).
valoracion_lista([Cabeza | Cola], [CabezaInterpretada | ColaInterpretada]) :-
    valoracion(Cabeza, CabezaInterpretada),
    valoracion_lista(Cola, ColaInterpretada).
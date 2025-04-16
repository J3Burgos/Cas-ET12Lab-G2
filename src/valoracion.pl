% ------------------------------
% VALORACIÓN DE TÉRMINOS
% ------------------------------

:- discontiguous interpretacion/3.
:- multifile interpretacion/3.
:- dynamic dominio/1.


% valoracion(+Termino, -ValorInterpretado)

% Caso 1: Variables — se extraen valores del dominio
valoracion(Variable, ValorInterpretado) :-
    var(Variable),
    dominio(Dominio),
    member(ValorInterpretado, Dominio).

% Caso 2: Constantes — se interpreta directamente
valoracion(Constante, ValorInterpretado) :-
    atomic(Constante),
    interpretacion(Constante, 0, ValorInterpretado).

% Caso 3: Términos compuestos (funciones con argumentos)
valoracion(TerminoCompuesto, ValorInterpretado) :-
    compound(TerminoCompuesto),
    functor(TerminoCompuesto, NombreFunctor, NumeroArgumentos),
    TerminoCompuesto =.. [NombreFunctor | Argumentos],
    interpretacion(NombreFunctor, NumeroArgumentos, FunctorInterpretado),
    valoracion_lista(Argumentos, ArgumentosInterpretados),
    FuncionAplicada =.. [FunctorInterpretado | ArgumentosInterpretados],
    call(FuncionAplicada, ValorInterpretado).

% valoracion_lista(+ListaTerminos, -ListaValoresInterpretados)
valoracion_lista([], []).
valoracion_lista([Cabeza | Cola], [CabezaInterpretada | ColaInterpretada]) :-
    valoracion(Cabeza, CabezaInterpretada),
    valoracion_lista(Cola, ColaInterpretada).
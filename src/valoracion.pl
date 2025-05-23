% --------------------------------------------------
% CASO 1: Variables — se extraen valores del dominio
% Si el término es una variable, se asocia a todos los posibles valores del dominio.
% --------------------------------------------------
valoracion(Variable, Valor) :-
    var(Variable),
    dominio(Dominio),
    member(Valor, Dominio).

% --------------------------------------------------
% CASO 2: Constantes — se interpretan directamente
% Si es una constante (átomo), se busca su interpretación.
% --------------------------------------------------
valoracion(Constante, Valor) :-
    atomic(Constante),
    interpretacion(Constante, 0, Valor).

% --------------------------------------------------
% CASO 3: Términos compuestos (funciones con argumentos)
% Se evalúa recursivamente cada argumento y luego se aplica
% la función correspondiente definida en la interpretación.
% --------------------------------------------------
valoracion(Termino, Valor) :-
    compound(Termino),
    functor(Termino, Functor, Aridad),
    Termino =.. [Functor | Argumentos],
    interpretacion(Functor, Aridad, FunctorInt),
    lista_valoraciones(Argumentos, ArgumentosInt),
    Funcion =.. [FunctorInt | ArgumentosInt],
    call(Funcion, Valor).

% --------------------------------------------------
% Evalúa una lista de términos lógicos.
% --------------------------------------------------
lista_valoraciones([], []).
lista_valoraciones([Cabeza | Cola], [CabezaInterpretada | ColaInterpretada]) :-
    valoracion(Cabeza, CabezaInterpretada),
    lista_valoraciones(Cola, ColaInterpretada).
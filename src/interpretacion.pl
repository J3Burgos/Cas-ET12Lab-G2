% --------------------------------------------------
% INTERPRETACION MUNDO DE FORMAS
% --------------------------------------------------

dominio([1, 2, 3]).

% Constantes
interpretacion(a, 0, 1).
interpretacion(b, 0, 2).
interpretacion(c, 0, 3).

% Relación al_lado
interpretacion(al_lado, 2, es_vecino).
es_vecino(1, 2, v).
es_vecino(2, 1, v).
es_vecino(2, 3, v).
es_vecino(3, 2, v).

% Relación forma
interpretacion(forma, 2, tiene_forma).
tiene_forma(1, cuadrado, v).
tiene_forma(2, circulo, v).
tiene_forma(3, triangulo, v).

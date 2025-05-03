% Sintaxis de operadores lógicos
:- op(300, fy,  [~]).
:- op(400, yfx, [/\]).
:- op(450, yfx, [\/]).
:- op(700, xfy, [=>]).
:- op(700, xfy, [<=>]).

% operador(Simbolo, Aridad, Tipo).
operador(~, 1, negacion).
operador(/\, 2, conjuncion).
operador(\/, 2, disyuncion).
operador(=>, 2, condicional).
operador(<=>, 2, bicondicional).

% --------------------------------------------------
% TABLAS DE VERDAD
% --------------------------------------------------

% Cambiar valor de verdadero a falso y viceversa
negacion(v, f).
negacion(f, v).

% Verdad si ambos son verdaderos, falso en caso contrario
conjuncion(v, v, v).
conjuncion(v, f, f).
conjuncion(f, v, f).
conjuncion(f, f, f).

% Verdad si al menos uno es verdadero, falso en caso contrario
disyuncion(v, v, v).
disyuncion(v, f, v).
disyuncion(f, v, v).
disyuncion(f, f, f).

% Verdadero si el primero implica al segundo, falso en caso contrario
condicional(v, v, v).
condicional(v, f, f).
condicional(f, v, v).
condicional(f, f, v).

% Verdadero si ambos son iguales, falso en caso contrario
bicondicional(v, v, v).
bicondicional(v, f, f).
bicondicional(f, v, f).
bicondicional(f, f, v).

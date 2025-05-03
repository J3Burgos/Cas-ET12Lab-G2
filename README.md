# Cas-ET12Lab-G2

- Desarrollar un programa capaz de evaluar fórmulas de la lógica de predicados en el contexto de interpretaciones con dominios finitos

## Ejemplos

- Sentecias Verdadera

forma(a, circulo).
forma(b, triangulo).
color(a, rojo).
color(c, azul).
al_lado(a, b).                  % usando constantes

color(a, rojo) /\ forma(a, circulo).
color(b, verde) \/ color(b, azul).
~color(c, rojo).
color(a, rojo) => forma(a, circulo).

exists(X, color(X, rojo)).
exists(X, forma(X, triangulo)).
exists(X, (color(X, verde) /\ forma(X, triangulo))).
forall(X, exists(Y, color(X, Y))).

- Sentencia Falsa

forma(a, triangulo).
forma(c, triangulo).
color(b, azul).
color(c, rojo).
color(c, verde).
forma(c, circulo).

color(a, azul) /\ forma(a, triangulo).
color(b, rojo) \/ forma(b, cuadrado).
~color(a, rojo).
color(a, azul) => forma(a, triangulo).

forall(X, color(X, rojo)).
forall(X, forma(X, circulo)).
exists(X, (forma(X, cuadrado) /\ color(X, rojo))).  % cuadrado es c (f3) que es azul
forall(X, exists(Y, al_lado(X, Y))).  % f3 no tiene nadie al lado

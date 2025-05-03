# Cas-ET12Lab-G2

- Desarrollar un programa capaz de evaluar fórmulas de la lógica de predicados en el contexto de interpretaciones con dominios finitos

## Objetivo

Desarrollar un entorno interactivo que permita:

- Definir interpretaciones (constantes, funciones, relaciones).
- Cargar interpretaciones desde archivos.
- Evaluar fórmulas lógicas con conectores y cuantificadores.
- Verificar su valor de verdad dentro del modelo cargado.

## Requisitos

- SWI-Prolog

## Instrucciones

1. Ejecutar el archivo `interfaz.pl` en SWI-Prolog.
2. Usar el menú principal para:
   - Definir una interpretación (`opción 0`)
   - Cargar un archivo de interpretación (`opción 1`)
   - Ver la interpretación activa (`opción 2`)
   - Evaluar fórmulas (`opción 3`)
3. Escribir fórmulas como:

   ```prolog
   color(a, rojo) /\ forma(a, circulo).
   exists(X, forma(X, triangulo)).
   forall(X, exists(Y, al_lado(X, Y))).
   ```

## Sintaxis de operadores

| Lógico        | Símbolo Prolog | Significado     |
|---------------|----------------|-----------------|
| Negación      | `~`            | No              |
| Conjunción    | `/\`          | Y               |
| Disyunción    | `\/`          | O               |
| Condicional   | `=>`           | Si...entonces   |
| Bicondicional | `<=>`          | Equivalencia    |
| Cuantificadores | `forall`, `exists` | Para todo / Existe |

## Ejemplos de evaluación

### ✅ Fórmulas Verdaderas

```prolog
forma(a, circulo).
forma(b, triangulo).
color(a, rojo).
color(c, azul).
al_lado(a, b).

color(a, rojo) /\ forma(a, circulo).
color(b, verde) \/ color(b, azul).
~color(c, rojo).
color(a, rojo) => forma(a, circulo).

exists(X, color(X, rojo)).
exists(X, forma(X, triangulo)).
exists(X, (color(X, verde) /\ forma(X, triangulo))).
forAll(X, exists(Y, color(X, Y))).
```

### ❌ Fórmulas Falsas

```prolog
forma(a, triangulo).
color(c, rojo).
color(c, verde).
forma(c, circulo).

color(a, azul) /\ forma(a, triangulo).
color(b, rojo) \/ forma(b, cuadrado).
~color(a, rojo).
color(a, azul) => forma(a, triangulo).

forall(X, color(X, rojo)).
forall(X, forma(X, circulo)).
exists(X, (forma(X, cuadrado) /\ color(X, rojo))).
forAll(X, exists(Y, al_lado(X, Y))).  % f3 no tiene nadie al lado
```

## Añadir nuevas fórmulas de prueba

Para probar nuevas fórmulas:

- Usa `opción 3` en el menú.
- Introduce fórmulas válidas según la sintaxis lógica.
- Puedes probar combinaciones anidadas, nuevas constantes o relaciones si las defines primero.

## Licencia

Proyecto educativo desarrollado por estudiantes del ET12 - Grupo G2.

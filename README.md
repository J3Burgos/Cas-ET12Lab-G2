# Cas-ET12Lab-G2

- Desarrollar un programa capaz de evaluar fórmulas de la lógica de predicados en el contexto de interpretaciones con dominios finitos

## Evaluador de Fórmulas Lógicas en Prolog

Este proyecto permite definir interpretaciones de estructuras lógicas (constantes, predicados, funciones), evaluar fórmulas usando lógica proposicional y de predicados, y operar sobre dominios personalizados mediante una interfaz interactiva en consola.

## Características

- Definición de interpretaciones personalizadas (constantes, funciones, predicados).
- Evaluación de fórmulas lógicas con conectores (`~`, `/\`, `\/`, `=>`, `<=>`) y cuantificadores (`forAll`, `exists`).
- Soporte para evaluaciones en diferentes contextos semánticos (visuales, letras, etc.).
- Interfaz de menú interactiva.
- Exportación/importación de interpretaciones a/desde archivos `.pl`.

## Estructura del Proyecto

| Archivo                     | Descripción                                                                              |
| --------------------------- | ---------------------------------------------------------------------------------------- |
| `interfaz.pl`               | Punto de entrada. Proporciona menú interactivo y flujo general del programa.             |
| `definir_interpretacion.pl` | Permite al usuario construir una interpretación lógicamente válida.                      |
| `evaluacion.pl`             | Lógica para evaluar fórmulas atómicas, compuestas y cuantificadas.                       |
| `formulas.pl`               | Define los operadores lógicos y su semántica.                                            |
| `valoracion.pl`             | Proporciona mecanismos para valorar términos lógicos (variables, funciones, constantes). |
| `inter_letras.pl`           | Interpretación de prueba basada en letras (`a`, `b`, `c`).                               |
| `interpretacion.pl`         | Interpretación de prueba basada en un mundo visual con figuras geométricas.              |

## Ejecución

1. Abre SWI-Prolog.
2. Carga el archivo principal:

   ```prolog
   ?- [interfaz].
   ```plaintext

3. Aparecerá un menú con las siguientes opciones:

   ```plaintext
   0. Definir interpretación
   1. Cargar interpretación
   2. Ver interpretación cargada
   3. Evaluar una fórmula
   4. Ayuda
   5. Salir
   ```

## Sintaxis de Fórmulas

- **Negación**: `~p`
- **Conjunción**: `p /\ q`
- **Disyunción**: `p \/ q`
- **Condicional**: `p => q`
- **Bicondicional**: `p <=> q`
- **Cuantificadores**:

  - Universal: `forAll(X, Formula)`
  - Existencial: `exists(X, Formula)`

### Ejemplos

```prolog
forma(a, cuadrado).                    % Fórmula atómica
~forma(b, circulo).                    % Negación
color(a, rojo) /\ forma(a, circulo).  % Conjunción
exists(X, forma(X, triangulo)).       % Cuantificador existencial
```

## Requisitos

- SWI-Prolog

:- module(definir_interpretacion, [ iniciar_interpretacion/0 ]).

:- dynamic interpretacion/3.
:- dynamic dominio/1.
:- dynamic usuario_pred/2.

%% Punto de entrada para definir la interpretación
iniciar_interpretacion :-
    write('Introduce el dominio como lista (ej: [0,1,2]): '), nl,
    safe_read(Dominio),
    retractall(dominio(_)),
    assertz(dominio(Dominio)),
    write('Dominio registrado.'), nl,
    definir_simbolos,
    preguntar_guardar.

%% Leer términos con manejo de errores de sintaxis
safe_read(Term) :-
    catch(read(Term), _Error, (
        write('Entrada no válida, inténtalo de nuevo.'), nl,
        safe_read(Term)
    )).

%% Definición interactiva de símbolos
definir_simbolos :-
    write('Deseas introducir un simbolo? (constante/predicado/fin): '), nl,
    safe_read(Opcion),
    ( Opcion == constante -> definir_constante, definir_simbolos
    ; Opcion == predicado  -> definir_predicado,  definir_simbolos
    ; Opcion == fin         -> write('Interpretacion completa.'), nl
    ; write('Opcion no valida.'), nl, definir_simbolos
    ).

%% Definir constantes
definir_constante :-
    write('Nombre de la constante (atomo): '), nl,
    safe_read(Nombre),
    write('Valor asociado en el dominio: '), nl,
    safe_read(Valor),
    assertz(interpretacion(Nombre, 0, Valor)),
    format('Constante ~w interpretada como ~w.~n', [Nombre, Valor]).

%% Definir predicados y registrar usuario_pred/2
definir_predicado :-
    write('Nombre del simbolo (publico): '), nl,
    safe_read(NombrePublico),
    write('Aridad (numero de argumentos en la formula): '), nl,
    safe_read(N),
    AridadInt is N + 1,
    write('Nombre interno del predicado o funcion: '), nl,
    safe_read(NombreInterno),
    assertz(interpretacion(NombrePublico, N, NombreInterno)),
    assertz(usuario_pred(NombreInterno, AridadInt)),
    format('Introduce hechos: cada hecho con aridad ~w (ej: ~w(Arg1,Arg2,...,Valor).)~n',
           [AridadInt, NombreInterno]),
    write('Termina con fin.'), nl,
    leer_hechos(AridadInt, NombreInterno),
    format('Simbolo ~w interpretado como ~w con aridad de formula ~w.~n',
           [NombrePublico, NombreInterno, N]).

leer_hechos(AridadEsperada, NombreEsperado) :-
    repeat,
    safe_read(Hecho),
    ( Hecho == fin -> !
    ; procesar_hecho(Hecho, AridadEsperada, NombreEsperado), fail
    ).

procesar_hecho(Hecho, AridadEsperada, NombreEsperado) :-
    compound(Hecho),
    functor(Hecho, Nombre, Aridad),
    ( Aridad =:= AridadEsperada ->
        ( Nombre == NombreEsperado ->
            assertz(Hecho),
            format('Hecho ~w anadido correctamente.~n', [Hecho])
        ; format('Error: se esperaba el nombre ~w pero recibiste ~w.~n',
                 [NombreEsperado, Nombre]), fail
        )
    ; format('Error: aridad incorrecta. Se esperaba ~w argumentos, pero el hecho tiene ~w.~n',
             [AridadEsperada, Aridad]), fail
    ).

%% Pregunta y guarda si el usuario lo desea
preguntar_guardar :-
    write('Deseas guardar esta interpretacion en un archivo? (si/no): '), nl,
    safe_read(SiNo),
    ( SiNo == si ->
        write('Nombre del archivo (con extension .pl o sin ella): '), nl,
        safe_read(Nombre),
        guardar_interpretacion(Nombre)
    ; true
    ).

%% Normaliza nombre y vuelca dominio, interpretaciones y hechos de usuario
guardar_interpretacion(Nombre) :-
    normalize_filename(Nombre, Archivo),
    open(Archivo, write, Stream),
    dominio(D), format(Stream, 'dominio(~q).~n~n', [D]),
    forAll(interpretacion(N, A, I),
           format(Stream, 'interpretacion(~q, ~w, ~q).~n', [N, A, I])),
    nl(Stream),
    forAll(usuario_pred(F, AInt),
           ( functor(T, F, AInt),
             forAll(clause(T, true),
                    format(Stream, '~q.~n', [T]))
           )),
    close(Stream),
    format('Interpretacion guardada en archivo ~w.~n', [Archivo]).

%% Añade .pl si hace falta
normalize_filename(Input, Archivo) :-
    ( sub_atom(Input, _, 3, 0, '.pl') -> Archivo = Input
    ; atom_concat(Input, '.pl', Archivo)
    ).


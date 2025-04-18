:- module(definir_interpretacion, [iniciar_interpretacion/0]).
:- dynamic interpretacion/3.
:- dynamic dominio/1.

iniciar_interpretacion :-
    write('Introduce el dominio como lista (ej: [0,1,2]): '), nl,
    read(Dominio),
    retractall(dominio(_)),
    assertz(dominio(Dominio)),
    write('Dominio registrado.'), nl,
    definir_simbolos,
    preguntar_guardar.

definir_simbolos :-
    write('Deseas introducir un simbolo? (constante/predicado/fin): '), nl,
    read(Opcion),
    ( Opcion == constante -> definir_constante, definir_simbolos
    ; Opcion == predicado -> definir_predicado, definir_simbolos
    ; Opcion == fin -> write('Interpretacion completa.'), nl
    ; write('Opcion no valida.'), nl, definir_simbolos
    ).

definir_constante :-
    write('Nombre de la constante (atomo): '), nl,
    read(Nombre),
    write('Valor asociado en el dominio: '), nl,
    read(Valor),
    assertz(interpretacion(Nombre, 0, Valor)),
    format('Constante ~w interpretada como ~w.~n', [Nombre, Valor]).

definir_predicado :-
    write('Nombre del simbolo (publico): '), nl,
    read(NombrePublico),
    write('Aridad: '), nl,
    read(Aridad),
    write('Nombre interno del predicado o funcion: '), nl,
    read(NombreInterno),
    write('Introduce los hechos (ej: pred(1,2,v).), termina con fin.'), nl,
    leer_hechos(Aridad, NombreInterno),
    assertz(interpretacion(NombrePublico, Aridad, NombreInterno)),
    format('Simbolo ~w interpretado como ~w.~n', [NombrePublico, NombreInterno]).

leer_hechos(AridadEsperada, NombreEsperado) :-
    repeat,
    read(Hecho),
    ( Hecho == fin ->
        !
    ; procesar_hecho(Hecho, AridadEsperada, NombreEsperado),
      fail
    ).

procesar_hecho(Hecho, AridadEsperada, NombreEsperado) :-
    compound(Hecho),
    functor(Hecho, Nombre, Aridad),
    ( Aridad =:= AridadEsperada ->
        ( Nombre == NombreEsperado ->
            assertz(Hecho),
            format('Hecho ~w anadido correctamente.~n', [Hecho])
        ; format('Error: se esperaba el nombre ~w pero recibiste ~w.~n', [NombreEsperado, Nombre]), fail
        )
    ; format('Error: aridad incorrecta. Se esperaba ~w argumentos, pero el hecho tiene ~w.~n', [AridadEsperada, Aridad]), fail
    ).

preguntar_guardar :-
    write('Deseas guardar esta interpretacion en un archivo? (si/no): '), nl,
    read(SiNo),
    ( SiNo == si ->
        write('Nombre del archivo (con extension .pl o sin ella): '), nl,
        read(Nombre),
        guardar_interpretacion(Nombre)
    ; true
    ).

guardar_interpretacion(Nombre) :-
    ( sub_atom(Nombre, _, 3, 0, '.pl') -> Archivo = Nombre ; atom_concat(Nombre, '.pl', Archivo) ),
    open(Archivo, write, Stream),
    dominio(D), format(Stream, 'dominio(~q).~n~n', [D]),
    forall(interpretacion(N, A, I), format(Stream, 'interpretacion(~q, ~w, ~q).~n', [N, A, I])),
    nl(Stream),
    findall(F/N, (interpretacion(_, _, F), atom(F), current_predicate(F/N)), Pairs),
    guardar_hechos(Pairs, Stream),
    close(Stream),
    format('Interpretacion guardada en archivo ~w.~n', [Archivo]).

guardar_hechos([], _).
guardar_hechos([F/N | Resto], Stream) :-
    functor(Hecho, F, N),
    forall(clause(Hecho, true), format(Stream, '~q.~n', [Hecho])),
    guardar_hechos(Resto, Stream).

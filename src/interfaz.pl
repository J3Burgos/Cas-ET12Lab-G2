:- dynamic fichero_cargado/1.

:- discontiguous interpretacion/3.
:- multifile interpretacion/3. 
:- dynamic interpretacion_operadores/3. 
:- dynamic dominio/1.

:- [valoracion].
:- [evaluacion].
:- [formulas].
:- [definir_interpretacion].
:- initialization(main).

main :-
    banner,
    menu.

banner :-
    write('       _______    _____    __    __  _____    ____  ____  ____  '), nl,
    write('      / ____/ |  / /   |  / /   / / / /   |  / __ \\/ __ \\/ __ \\ '), nl,
    write('     / __/  | | / / /| | / /   / / / / /| | / / / / / / / /_/ / '), nl,
    write('    / /___  | |/ / ___ |/ /___/ /_/ / ___ |/ /_/ / /_/ / _, _/  '), nl,
    write('   /_____/  |___/_/  |_/_____/\\____/_/  |_/_____/\\____/_/ |_|   '), nl,
    write('                         ____  ______                           '), nl,
    write('                        / __ \\/ ____/                           '), nl,
    write('                       / / / / __/                              '), nl,
    write('                      / /_/ / /___                              '), nl,
    write('                     /_____/_____/                              '), nl,
    write('         __________  ____  __  _____  ____    ___   _____       '), nl,
    write('        / ____/ __ \\/ __ \\/  |/  / / / / /   /   | / ___/       '), nl,
    write('       / /_  / / / / /_/ / /|_/ / / / / /   / /| | \\__ \\        '), nl,
    write('      / __/ / /_/ / _, _/ /  / / /_/ / /___/ ___ |___/ /        '), nl,
    write('     /_/    \\____/_/ |_/_/  /_/\\____/_____/_/  |_/____/         '), nl,
    write('           __    ____  ___________________   _____              '), nl,
    write('          / /   / __ \\/ ____/  _/ ____/   | / ___/              '), nl,
    write('         / /   / / / / / __ / // /   / /| | \\__ \\               '), nl,
    write('        / /___/ /_/ / /_/ // // /___/ ___ |___/ /               '), nl,
    write('       /_____/\\____/\\____/___/\\____/_/  |_/____/                '), nl,
    write('                                                                '), nl.
menu :-
    write('\n----------------------------------'), nl,
    write('--------- Menu Principal ---------'), nl,
    write('----------------------------------'), nl,
    write('0. Definir interpretacion'), nl,
    write('1. Cargar interpretacion'), nl,
    write('2. Ver interpretacion cargada'), nl,
    write('3. Evaluar una formula'), nl,
    write('4. Ayuda'), nl,
    write('5. Salir'), nl,
    write('Seleccione una opcion: '),
    read(Opcion),
    procesar_opcion(Opcion).

procesar_opcion(0) :- !,
    write('\n== Definir Interpretacion =='), nl,
    iniciar_interpretacion,
    menu.

procesar_opcion(1) :- !,
    write('\n== Cargar Interpretacion =='), nl,
    write('Nombre del archivo entre comillas ("nombre.pl"): '), nl,
    read(Fichero),
    cargar_fichero(Fichero),
    menu.

procesar_opcion(2) :- !,
    mostrar_interpretacion,
    menu.

procesar_opcion(3) :- !,
    write('\n== Evaluar Formula =='), nl,
    write('Ingrese la formula (o "salir." para volver al menu): '), nl,
    read(Formula),
    ( Formula == salir ->
        write('Saliendo de la evaluacion de frmulas...'), nl
    ; compound(Formula) ->
        ( catch(evaluacion:evaluacion(Formula, Resultado), Error, (
              write('¡Error durante la evaluacion!'), nl,
              mostrar_error(Error),
              fail
          )) ->
            write('Resultado: '), write(Resultado), nl
        ; write('Formula invalida o error durante la evaluacion.'), nl
        ),
        procesar_opcion(3)  % Volver a evaluar otra fórmula
    ; write('Error: ingrese solo la formula (no use "evaluacion(...)" ni punto y coma).'), nl,
      procesar_opcion(3)  % Volver a evaluar otra fórmula
    ),
    menu.

procesar_opcion(4) :- !,
    ayuda,
    menu.

procesar_opcion(5) :- !,
    write('\nSaliendo...'), nl,
    halt.

procesar_opcion(_) :-
    write('\nOpcion no valida.'), nl,
    menu.


% --------------------------------------------------
% FUNCIONES AUXILIARES
% --------------------------------------------------

cargar_fichero(Fichero) :-
    exists_file(Fichero),
    retractall(fichero_cargado(_)),
    consult(Fichero),
    assertz(fichero_cargado(Fichero)),
    write('Interpretacion cargada exitosamente.'), nl.

cargar_fichero(_) :-
    write('El archivo no existe.'), nl.

mostrar_interpretacion :-
    ( fichero_cargado(F) -> write('Archivo cargado: '), write(F), nl ;
      write('No hay archivo cargado.'), nl
    ),
    ( dominio(D) -> true ; D = 'NO DEFINIDO' ),
    write('Dominio: '), write(D), nl,
    findall(Const, (interpretacion(Const, 0, _)), Constantes),
    write('Constantes: '), write(Constantes), nl,
    findall(Func, (interpretacion(Func, N, _), N > 0), Simbolos),
    write('Simbolos (relaciones y funciones): '), write(Simbolos), nl.

mostrar_error(error(Msg, Args)) :-
    format('Error: '), format(Msg, Args), nl.

ayuda :-
    write('\n== Ayuda Rapida =='), nl,
    write('Ingrese formulas en formato Prolog usando:'), nl,
    tab(4), write('~   negacion'), nl,
    tab(4), write('/\\  conjuncion'), nl,
    tab(4), write('\\/  disyuncion'), nl,
    tab(4), write('=>  condicional'), nl,
    tab(4), write('<=> bicondicional'), nl,
    write('Cuantificadores:'), nl,
    tab(4), write('forAll(Var, Formula)'), nl,
    tab(4), write('exists(Var, Formula)'), nl,
    write('\nEjemplos:'), nl,
    tab(4), write('al_lado(a, b).'), nl,
    tab(4), write('forma(c, triangulo).'), nl,
    tab(4), write('exists(X, al_lado(c, X)).'), nl,
    write('\nPresione Enter para volver al menu.').

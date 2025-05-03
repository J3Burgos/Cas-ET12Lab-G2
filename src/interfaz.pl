:- dynamic fichero_cargado/1.

:- [valoracion].
:- [evaluacion].
:- [formulas].
:- [definir_interpretacion].
:- initialization(main).

main :-
    menu,
    read(Opcion),
    procesar_opcion(Opcion).

menu :-
    write('\n============================================='), nl,
    write('== Evaluador de Formulas Logicas - Interfaz =='), nl,
    write('============================================='), nl,
    write('0. Definir interpretacion'), nl,
    write('1. Cargar interpretacion'), nl,
    write('2. Ver interpretacion cargada'), nl,
    write('3. Evaluar una formula'), nl,
    write('4. Ayuda'), nl,
    write('5. Salir'), nl,
    write('Seleccione una opcion: ').

procesar_opcion(0) :- !,
    write('\n== Definir Interpretacion =='), nl,
    iniciar_interpretacion,
    main.

procesar_opcion(1) :- !,
    write('\n== Cargar Interpretacion =='), nl,
    write('Nombre del archivo entre comillas ("nombre.pl"): '), nl,
    read(Fichero),
    cargar_fichero(Fichero),
    main.

procesar_opcion(2) :- !,
    mostrar_interpretacion,
    main.

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
    main.

procesar_opcion(4) :- !,
    ayuda,
    main.

procesar_opcion(5) :- !,
    write('\nSaliendo...'), nl,
    halt.

procesar_opcion(_) :-
    write('\nOpcion no valida.'), nl,
    main.

% =======================
% FUNCIONES AUXILIARES
% =======================

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
    write('\nEjemplos (no usar punto al final):'), nl,
    tab(4), write('al_lado(a, b)'), nl,
    tab(4), write('forma(c, triangulo)'), nl,
    tab(4), write('exists(X, al_lado(c, X))'), nl,
    write('\nPresione Enter para volver al menu.'), nl,
    read(_).

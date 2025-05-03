%% INTERPRETACIÓN DE PRUEBA - NÚMEROS SIMPLES

% Dominio de prueba
dominio([a, b, c]).

% Constantes nombradas
interpretacion(x, 0, a).
interpretacion(y, 0, b).
interpretacion(z, 0, c).

% Función unaria: siguiente letra (rotativo)
interpretacion(siguiente, 1, sig_letra).
sig_letra(a, b).
sig_letra(b, c).
sig_letra(c, a).

% Predicado unario: vocal (solo 'a')
interpretacion(es_vocal, 1, pred_vocal).
pred_vocal(a, v).
pred_vocal(b, f).
pred_vocal(c, f).
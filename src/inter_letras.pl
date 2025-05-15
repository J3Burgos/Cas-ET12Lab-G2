dominio([1, 2, 3]).

interpretacion(a, 0, 1).
interpretacion(b, 0, 2).
interpretacion(c, 0, 3).
interpretacion(siguiente, 1, sig_letra).
interpretacion(es_vocal, 1, pred_vocal).

sig_letra(1, 2).
sig_letra(2, 3).
sig_letra(3, 1).

pred_vocal(1, v).
pred_vocal(2, f).
pred_vocal(3, f).
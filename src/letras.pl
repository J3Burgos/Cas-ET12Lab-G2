dominio([1,2,3]).

interpretacion(a, 0, 1).
interpretacion(b, 0, 2).
interpretacion(c, 0, 3).
interpretacion(siguiente, 1, sig_letra).
interpretacion(es_vocal, 1, vocal).

sig_letra(1,2).
sig_letra(2,3).
vocal(1,v).
vocal(2,f).
vocal(3,f).

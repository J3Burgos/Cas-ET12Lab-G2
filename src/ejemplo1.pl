dominio([1,2,3]).

interpretacion(a, 0, 1).
interpretacion(b, 0, 2).
interpretacion(c, 0, 3).
interpretacion(es_vocal, 1, vo).
interpretacion(siguiente, 1, sig).

vo(1,v).
vo(2,f).
vo(3,f).
sig(1,2).
sig(2,3).
sig(3,f).

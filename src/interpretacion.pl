% ==========================================
% INTERPRETACIÓN DE PRUEBA - MUNDO VISUAL
% ==========================================

% Dominio: elementos geométricos
dominio([f1, f2, f3, o1, o2, o3, c1, c2, c3]).

% ------------------------------------------
% Constantes: cada letra representa una figura
% ------------------------------------------
interpretacion(a, 0, f1).  % figura a
interpretacion(b, 0, f2).  % figura b
interpretacion(c, 0, f3).  % figura c

interpretacion(cuadrado, 0, o1).     % forma d
interpretacion(circulo, 0, o2).      % forma e
interpretacion(triangulo, 0, o3).    % forma f

interpretacion(rojo, 0, c1).     % color g
interpretacion(azul, 0, c2).     % color h
interpretacion(verde, 0, c3).    % color i

interpretacion(forma, 2, forma_inter).
interpretacion(color, 2, color_inter).
interpretacion(al_lado, 2, lado_inter).

% ------------------------------------------
% Predicados unarios: propiedades de figuras
% ------------------------------------------

forma_inter(f1, o2, v).   % a es un circulo
forma_inter(f2, o3, v).   % b es un triangulo
forma_inter(f3, o1, v).   % c es un cuadrado 
forma_inter(f1, o1, f).   % a no es un cuadrado
forma_inter(f2, o2, f).   % b no es un circulo
forma_inter(f3, o3, f).   % c no es un triangulo

color_inter(f1, c1, v).   % a es rojo
color_inter(f2, c3, v).   % b es verde
color_inter(f3, c2, v).   % c es azul
color_inter(f1, c2, f).   % a no es azul
color_inter(f2, c1, f).   % b no es rojo
color_inter(f3, c3, f).   % c no es verde

lado_inter(f1, f2, v).   % a al lado de b
lado_inter(f2, f1, v).   % b al lado de a
lado_inter(f2, f3, v).   % b al lado de c
lado_inter(f3, f2, v).   % c al lado de b
lado_inter(f1, f3, f).   % a no al lado de c
lado_inter(f3, f1, f).   % c no al lado de a



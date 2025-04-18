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

color_inter(f1, c1, v).   % a es rojo
color_inter(f2, c3, v).   % b es verde
color_inter(f3, c2, v).   % c es azul

lado_inter(1, 2, v).   % a al lado de b
lado_inter(2, 3, v).   % b al lado de c



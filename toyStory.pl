% Relaciona al dueño con el nombre del juguete y la cantidad de años que lo ha tenido
duenio(andy, woody, 8).
duenio(sam, jessie, 3).
% Relaciona al juguete con su nombre
% los juguetes son de la forma:
% deTrapo(tematica)
% deAccion(tematica, partes)
% miniFiguras(tematica, cantidadDeFiguras)
% caraDePapa(partes)
juguete(woody, deTrapo(vaquero)).
juguete(jessie, deTrapo(vaquero)).
juguete(buzz, deAccion(espacial, [original(casco)])).
juguete(soldados, miniFiguras(soldado, 60)).
juguete(monitosEnBarril, miniFiguras(mono, 50)).
juguete(seniorCaraDePapa, caraDePapa([ original(pieIzquierdo), original(pieDerecho), repuesto(nariz) ])).
% Dice si un juguete es raro
esRaro(deAccion(stacyMalibu, 1, [sombrero])).
% Dice si una persona es coleccionista
esColeccionista(sam).


% Punto 1

tematica(Juguete, Tematica):-
    juguete(_, Juguete),
    tematicaSegun(Juguete, Tematica).

tematicaSegun(deTrapo(Tematica), Tematica).
tematicaSegun(deAccion(Tematica, _), Tematica).
tematicaSegun(miniFiguras(Tematica, _), Tematica).
tematicaSegun(caraDePapa(_), caraDePapa).

esDePlastico(Juguete):-
    juguete(_, Juguete),
    plastico(Juguete).

plastico(miniFiguras(_, _)).
plastico(caraDePapa(_)).

esDeColeccion(Juguete):-
    juguete(_, Juguete),
    coleccionable(Juguete).

coleccionable(deTrapo(_)).
coleccionable(Juguete):-
    accionOcaraDePapa(Juguete),
    esRaro(Juguete).

accionOcaraDePapa(caraDePapa(_)).
accionOcaraDePapa(deAccion(_, _)).

%% Punto 2

amigoFiel(Duenio, NombreDeJuguete):-
    juguete(NombreDeJuguete, Juguete),
    duenio(Duenio, NombreDeJuguete, Antiguedad),
    not(esDePlastico(Juguete)),
    forall((duenio(Duenio, OtroNombreDeJuguete, Antiguedad2), OtroNombreDeJuguete \= NombreDeJuguete), Antiguedad > Antiguedad2).

%%Punto 3

superValioso(Nombre):-
    juguete(Nombre, Juguete),
    tienePiezasOriginales(Juguete),
    duenio(Duenio, Nombre, _),
    not(esColeccionista(Duenio)).

tienePiezasOriginales(Juguete):-
    piezas(Juguete, Piezas),
    forall(member(Pieza, Piezas), esOriginal(Pieza)).

piezas(deAccion(_, Piezas), Piezas).
piezas(caraDePapa(Piezas), Piezas).

esOriginal(original(_)).































puedeDonar(Duenio, ListaDeJuguetes, Felicidad):-
    
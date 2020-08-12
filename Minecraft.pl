jugador(stuart, [piedra, piedra, piedra, piedra, piedra, piedra, piedra, piedra], 3).
jugador(tim, [madera, madera, madera, madera, madera, pan, carbon, carbon, carbon, pollo, pollo], 8).
jugador(steve, [madera, carbon, carbon, diamante, panceta, panceta, panceta], 2).

lugar(playa, [stuart, tim], 2).
lugar(mina, [steve], 8).
lugar(bosque, [], 6).

comestible(pan).
comestible(panceta).
comestible(pollo).
comestible(pescado).


%%punto 1

tieneItem(Jugador, Item):-
    caracteristicaJugadores(Jugador, tiene(Item)).

sePreocupaPorSuSalud(Jugador):-
    tieneItem(Jugador, Item1),
    tieneItem(Jugador, Item2),
    comestible(Item1),
    comestible(Item2),
    Item1 \= Item2.

cantidadDelItem(Jugador, Item, Cantidad):-
    caracteristicaJugadores(Jugador, cantidad(Item, Cantidad)).
    
item(Item):-
    tieneItem(_, Item).

esJugador(Jugador):-
    jugador(Jugador, _, _).

tieneMasDe(Jugador, Item):-
    cantidadDelItem(Jugador, Item, Cantidad1),
    forall((cantidadDelItem(OtroJugador, Item, Cantidad2), Jugador \= OtroJugador), Cantidad1 > Cantidad2).

%%Punto 2

hayMonstruos(Lugar):-
    caracteristicaLugares(Lugar, monstruos).

correPeligro(Jugador):-
    caracteristicaLugares(Lugar, habita(Jugador)),
    hayMonstruos(Lugar).

correPeligro(Jugador):-
    estaHambriento(Jugador),
    noTieneComida(Jugador).

estaHambriento(Jugador):-
    caracteristicaJugadores(Jugador, hambriento).

noTieneComida(Jugador):-
    caracteristicaJugadores(Jugador, noComida).

nivelDePeligrosidad(Lugar, Peligrosidad):-
    caracteristicaLugares(Lugar, peligrosidad(Peligrosidad)).

%%Punto 3

item(horno, [ itemSimple(piedra, 8) ]).
item(placaDeMadera, [ itemSimple(madera, 1) ]).
item(palo, [ itemCompuesto(placaDeMadera) ]).
item(antorcha, [ itemCompuesto(palo), itemSimple(carbon, 1) ]).

puedeConstruir(Jugador, Item):-
    caracteristicaJugadores(Jugador, puedeConstruir(Item)).

%%________________________________________________________________________________________________________________________________________________________


caracteristicaJugadores(Jugador, Caracteristica):-
    jugador(Jugador, Inventario, Hambre),
    caracteristicaJugadoresSegun(Inventario, Hambre, Caracteristica).

caracteristicaJugadoresSegun(Inventario, _, tiene(Item)):-
    member(Item, Inventario).

caracteristicaJugadoresSegun(Inventario, _, cantidad(Item, Cantidad)):-
    item(Item),
    findall(Item, caracteristicaJugadoresSegun(Inventario, _, tiene(Item)), ListaDeEseItem),
    length(ListaDeEseItem, Cantidad).

caracteristicaJugadoresSegun(_, Hambre, hambriento):-
    Hambre < 4.

caracteristicaJugadoresSegun(Inventario, _, noComida):-
    forall((comestible(Item), item(Item)), caracteristicaJugadoresSegun(Inventario, _, cantidad(Item, 0))).

caracteristicaJugadoresSegun(Inventario, _, puedeConstruir(Item)):-
    item(Item, Materiales),
    forall(member(Material, Materiales), puedeConstruirSegun(Inventario, Material)).

caracteristicaLugares(Lugar, Caracteristica):-
    lugar(Lugar, Habitantes, Oscuridad),
    caracteristicaLugarSegun(Habitantes, Oscuridad, Caracteristica).

caracteristicaLugarSegun(_, Oscuridad, monstruos):-
    Oscuridad > 6.

caracteristicaLugarSegun(Habitantes, _, habita(Jugador)):-
    member(Jugador, Habitantes).

caracteristicaLugarSegun(Habitantes, Oscuridad, peligrosidad(Nivel)):-
    peligrosidadSegun(Habitantes, Oscuridad, Nivel).

peligrosidadSegun(ListaDeJugadores, Oscuridad, Peligrosidad):-
    length(ListaDeJugadores, 0),
    Peligrosidad is Oscuridad*10.

peligrosidadSegun(ListaDeJugadores, Oscuridad, Peligrosidad):-
    length(ListaDeJugadores, Cantidad),
    Cantidad > 0,
    peligrosidadPoblados(Oscuridad, ListaDeJugadores, Peligrosidad).

peligrosidadPoblados(Oscuridad, ListaDeJugadores, Peligrosidad):-
    not(caracteristicaLugarSegun(_, Oscuridad, monstruos)),
    findall(Hambriento, (estaHambriento(Hambriento), caracteristicaLugarSegun(ListaDeJugadores, _, habita(Hambriento))), GenteConHambre),
    length(ListaDeJugadores, CantidadTotal),
    length(GenteConHambre, CantidadHambrientos),
    Peligrosidad is CantidadHambrientos*100/CantidadTotal.

peligrosidadPoblados(Oscuridad, _, 100):-
    caracteristicaLugarSegun(_, Oscuridad, monstruos).

puedeConstruirSegun(Inventario, itemCompuesto(Item)):-
    caracteristicaJugadoresSegun(Inventario, _, puedeConstruir(Item)).

puedeConstruirSegun(Inventario, itemSimple(Item, Cantidad)):-
    caracteristicaJugadoresSegun(Inventario, _, cantidad(Item, CantidadTotal)),
    CantidadTotal >= Cantidad.
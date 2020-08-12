% Base de conocimiento

jugador(juli, 2200, jemeres).
jugador(aleP, 1600, mongoles).
jugador(feli, 500000, persas).
jugador(aleC, 1723, otomanos).
jugador(ger, 1729, ramanujanos).
jugador(juan, 1515, britones).
jugador(marti, 1342, argentinos).
% … y muchos más también
tiene(aleP, unidad(samurai, 199)).
tiene(aleP, unidad(espadachin, 10)).
tiene(aleP, unidad(granjero, 10)).
tiene(aleP, recurso(800, 300, 100)).
tiene(aleP, edificio(casa, 40)).
tiene(aleP, edificio(castillo, 1)).
tiene(juan, unidad(carreta, 10)).
% … y muchos más también

% militar(Tipo, costo(Madera, Alimento, Oro), Categoria).
militar(espadachin, costo(0, 60, 20), infanteria).
militar(arquero, costo(25, 0, 45), arqueria).
militar(mangudai, costo(55, 0, 65), caballeria).
militar(samurai, costo(0, 60, 30), unica).
militar(keshik, costo(0, 80, 50), unica).
militar(tarcanos, costo(0, 60, 60), unica).
militar(alabardero, costo(25, 35, 0), piquero).
% … y muchos más tipos pertenecientes a estas categorías.

% aldeano(Tipo, produce(Madera, Alimento, Oro)).
aldeano(lenador, produce(23, 0, 0)).
aldeano(granjero, produce(0, 32, 0)).
aldeano(minero, produce(0, 0, 23)).
aldeano(cazador, produce(0, 25, 0)).
aldeano(pescador, produce(0, 23, 0)).
aldeano(alquimista, produce(0, 0, 25)).
% … y muchos más también

% edificio(Edificio, costo(Madera, Alimento, Oro)).
edificio(casa, costo(30, 0, 0)).
edificio(granja, costo(0, 60, 0)).
edificio(herreria, costo(175, 0, 0)).
edificio(castillo, costo(650, 0, 300)).
edificio(maravillaMartinez, costo(10000, 10000, 10000)).
% … y muchos más también

lePuedeGanar(caballeria, arqueria).
lePuedeGanar(arqueria, infanteria).
lePuedeGanar(infanteria, piqueros).
lePuedeGanar(piquero, caballeria).

% transporte

esTransporte(carreta).
esTransporte(urnaMercante).

% Recurso

recurso(oro).
recurso(madera).
recurso(alimento).

% Edades y sus condiciones para avanzar

edad(media, []).
edad(feudal, [tieneRecurso(alimento, 500), tieneEdificio(casa, 1)]).
edad(castillos, [tieneRecurso(alimento, 800), tieneRecurso(oro, 200), tieneEdificio(herreria, 1)]).
edad(castillos, [tieneRecurso(alimento, 800), tieneRecurso(oro, 200), tieneEdificio(establo, 1)]).
edad(castillos, [tieneRecurso(alimento, 800), tieneRecurso(oro, 200), tieneEdificio(galeriaDeTiro, 1)]).
edad(imperial, [tieneRecurso(alimento, 1000), tieneRecurso(oro, 800), tieneEdificio(castillo, 1), tieneEdificio(universidad, 1)]).

% Punto 1

esUnAfano(Jugador1, Jugador2):-
    jugador(Jugador1, Puntos1, _),
    jugador(Jugador2, Puntos2, _),
    %diferenciaDePuntosMayorA(Puntos1, Puntos2, 500).
    Puntos1 - Puntos2 > 500.

%diferenciaDePuntosMayorA(PuntosJugador1, PuntosJugador2, DiferenciaMinima):-
%    PuntosJugador1 - PuntosJugador2 > DiferenciaMinima.

% Punto 2

esEfectivo(Unidad1, Unidad2):-
    militar(Unidad1, _, Categoria1),
    militar(Unidad2, _, Categoria2),
    lePuedeGanar(Categoria1, Categoria2).

esEfectivo(samurai, Unidad):-
    militar(Unidad, _, unica).

% Punto 3

soloTieneDe(Jugador, Categoria):-
    tiene(Jugador, _),
    forall(tiene(Jugador, unidad(Unidad, _)), militar(Unidad, _, Categoria)).

alarico(Jugador):-
    soloTieneDe(Jugador, infanteria).

% Punto 4

leonidas(Jugador):-
    soloTieneDe(Jugador, piqueros).

% Punto 5

nomada(Jugador):-
    jugador(Jugador, _, _),
    not(tiene(Jugador, edificio(casa, _))).

% Punto 6

cuantoCuesta(Algo, Costo):-
    militar(Algo, Costo, _).

cuantoCuesta(Algo, costo(0,50,0)):-
    aldeano(Algo, _).

cuantoCuesta(Algo, Costo):-
    edificio(Algo, Costo).

cuantoCuesta(Algo, costo(100,0,50)):-
    esTransporte(Algo).

% Punto 7

produccion(Algo, Produce):-
    aldeano(Algo, Produce).

produccion(Algo, produce(0,0,32)):-
    esTransporte(Algo).

produccion(keshik, produce(0,0,10)).

% Punto 8

produccionTotal(Jugador, Recurso, ProduccionTotal):-
    jugador(Jugador, _, _),
    recurso(Recurso),
    calculoDeProduccion(Jugador, Recurso, ProduccionTotal).
    
calculoDeProduccion(Jugador, Recurso, ProduccionTotal):-
    findall(Produccion, produccionDeUnRecurso(Jugador, Recurso, Produccion), ProduccionesIndividuales),
    sum_list(ProduccionesIndividuales, ProduccionTotal).

produccionDeUnRecurso(Jugador, Recurso, Produccion):-
    tiene(Jugador, unidad(Unidad, CantidadUnidades)),
    produceRecurso(Unidad, Recurso, CantidadRecurso),
    Produccion is CantidadRecurso*CantidadUnidades.

produceRecurso(Unidad, oro, CantidadRecurso):-
    produccion(Unidad, produce(_, _, CantidadRecurso)).
produceRecurso(Unidad, alimento, CantidadRecurso):-
    produccion(Unidad, produce(_, CantidadRecurso, _)).
produceRecurso(Unidad, madera, CantidadRecurso):-
    produccion(Unidad, produce(CantidadRecurso, _, _)).

% Punto 9

estaPeleado(Jugador1, Jugador2):-
    jugador(Jugador1, _, _),
    jugador(Jugador2, _, _),
    Jugador1 \= Jugador2,
    cantidadUnidades(Jugador1, CantidadUnidades),
    cantidadUnidades(Jugador2, CantidadUnidades),
    valorProduccion(Jugador1, Valor1),
    valorProduccion(Jugador2, Valor2),
    abs(Valor1 - Valor2, Resultado),
    Resultado < 100,
    not(esUnAfano(Jugador1, Jugador2)),
    not(esUnAfano(Jugador2, Jugador1)).

cantidadUnidades(Jugador, Cantidad):-
    findall(CantidadUnidad, (tiene(Jugador, unidad(_, CantidadUnidad))), CantidadUnidades),
    sum_list(CantidadUnidades, Cantidad).

valorProduccion(Jugador, Valor):-
    findall(ValorRecurso, (produccionTotal(Jugador, Recurso, ProduccionRecurso), valorRecurso(Recurso, ProduccionRecurso, ValorRecurso)), ValoresDeRecursos),
    sum_list(ValoresDeRecursos, Valor).

valorRecurso(oro, ProduccionRecurso, ValorRecurso):-
    ValorRecurso is ProduccionRecurso*5.

valorRecurso(madera, ProduccionRecurso, ValorRecurso):-
    ValorRecurso is ProduccionRecurso*3.

valorRecurso(alimento, ProduccionRecurso, ValorRecurso):-
    ValorRecurso is ProduccionRecurso*2.

% Punto 10

avanzaA(Jugador, Edad):-
    jugador(Jugador, _, _),
    edad(Edad, Condiciones),
    forall(member(Condicion, Condiciones), cumpleCondicion(Jugador, Condicion)).

cumpleCondicion(Jugador, tieneEdificio(Edificio, Cantidad)):-
    tiene(Jugador, edificio(Edificio, CantidadJugador)),
    CantidadJugador >= Cantidad.

cumpleCondicion(Jugador, tieneRecurso(Recurso, Cantidad)):-
    recurso(Recurso),
    tieneDeEseRecurso(Jugador, Recurso, CantidadJugador),
    CantidadJugador >= Cantidad.

tieneDeEseRecurso(Jugador, oro, Cantidad):-
    tiene(Jugador, recurso(_, _, Cantidad)).
tieneDeEseRecurso(Jugador, alimento, Cantidad):-
    tiene(Jugador, recurso(_, Cantidad, _)).
tieneDeEseRecurso(Jugador, madera, Cantidad):-
    tiene(Jugador, recurso(Cantidad, _, _)).
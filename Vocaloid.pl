% Base de conocimiento

vocaloid(megurineLuka, [cancion(nightFever, 4), cancion(foreverYoung, 5)]).
vocaloid(hatsuneMiku, [cancion(tellYourWorld, 4)]).
vocaloid(gumi, [cancion(foreverYoung(4)), cancion(tellYourWorld, 5)]).
vocaloid(seeU, [cancion(novemberRain, 6), cancion(nightFever, 5)]).
vocaloid(kaito, []).

% Punto 1

vocaloidNovedoso(Vocaloid):-
    vocaloid(Vocaloid, Canciones),
    sabeAlMenosNCanciones(Canciones, 2),
    duracionMenorA(Canciones, 15).

duracionDeCanciones(Canciones, Duracion):-
    findall(TiempoCancion, member(cancion(_, TiempoCancion), Canciones), TiempoCanciones),
    sum_list(TiempoCanciones, Duracion).
    
% Punto 2

acelerado(Vocaloid):-
    vocaloid(Vocaloid, _),
    not(calmado(Vocaloid)).

calmado(Vocaloid):-
    vocaloid(Vocaloid, Canciones),
    algunaDuraMasDe(Canciones, 4).

% Punto 3

concierto(mikuExpo, estadosUnidos, 2000, gigante(2, 6)).
concierto(magicalMirai, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequenio(4)).

% Punto 4

puedeParticipar(Vocaloid, Concierto):-
    vocaloid(Vocaloid, Canciones),
    concierto(Concierto, _, _, Caracteristica),
    condicionCanciones(Canciones, Caracteristica).

puedeParticipar(hatsuneMiku, Concierto):-
    concierto(Concierto, _, _, _).

condicionCanciones(Canciones, gigante(MinimasConocidas, DuracionMinima)):-
    sabeAlMenosNCanciones(Canciones, MinimasConocidas),
    duracionMayorA(Canciones, DuracionMinima).

condicionCanciones(Canciones, mediano(DuracionMaxima)):-
    duracionMenorA(Canciones, DuracionMaxima).

condicionCanciones(Canciones, pequenio(DuracionMinima)):-
    algunaDuraMasDe(Canciones, DuracionMinima).

sabeAlMenosNCanciones(Canciones, CantidadMinima):-
    length(Canciones, Cantidad),
    Cantidad >= CantidadMinima.

duracionMayorA(Canciones, DuracionMinima):-
    duracionDeCanciones(Canciones, Duracion),
    Duracion >= DuracionMinima.

duracionMenorA(Canciones, DuracionMaxima):-
    duracionDeCanciones(Canciones, Duracion),
    Duracion < DuracionMaxima.

algunaDuraMasDe(Canciones, DuracionMinima):-
    member(cancion(_, TiempoCancion), Canciones),
    TiempoCancion > DuracionMinima.

% Punto 5

masFamoso(Vocaloid):-
    vocaloid(Vocaloid, Canciones),
    calculoDeFama(Vocaloid, Canciones, Fama),
    forall((vocaloid(OtroVocaloid, _), OtroVocaloid \= Vocaloid, calculoDeFama(OtroVocaloid, _, Fama2)), Fama > Fama2).

calculoDeFama(Vocaloid, Canciones, Fama):-
    vocaloid(Vocaloid, Canciones),
    findall(FamaPorConcierto, (puedeParticipar(Vocaloid, Concierto), concierto(Concierto, _, FamaPorConcierto, _)), GananciasDeFama),
    sum_list(GananciasDeFama, GananciaTotal),
    length(Canciones, CancionesConocidas),
    Fama is CancionesConocidas*GananciaTotal.

% Punto 6

conoceA(megurineLuka, hatsuneMiku).
conoceA(megurineLuka, gumi).
conoceA(gumi, seeU).
conoceA(seeU, kaito).

conoceIndirectamente(Vocaloid, OtroVocaloid):-
    vocaloid(Vocaloid, _),
    vocaloid(OtroVocaloid, _),
    conoceA(Vocaloid, AnotherVocaloid),
    conoceA(AnotherVocaloid, OtroVocaloid).

unicoEnParticipar(Vocaloid, Concierto):-
    puedeParticipar(Vocaloid, Concierto),
    forall(conoceA(Vocaloid, OtroVocaloid), not(puedeParticipar(OtroVocaloid, Concierto))),
    forall(conoceIndirectamente(Vocaloid, AnotherVocaloid), not(puedeParticipar(AnotherVocaloid, Concierto))).

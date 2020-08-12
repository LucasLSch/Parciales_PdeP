rata(remy, gusteaus).
rata(emile, bar).
rata(django, pizzeria).

cocina(linguini, ratatouille, 3).
cocina(linguini, sopa, 5). 
cocina(colette, salmonAsado, 9).
cocina(horst, ensaladaRusa, 8).

trabajaEn(gusteaus, linguini).
trabajaEn(gusteaus, colette).
trabajaEn(gusteaus, skinner).
trabajaEn(gusteaus, horst).
trabajaEn(cafeDes2Moulins, amelie).

plato(ensaladaRusa, entrada([papa, zanahoria, arvejas, huevo, mayonesa])).
plato(bifeDeChorizo, principal(pure, 25)).
plato(frutillasConCrema, postre(265)).

%1)

inspeccionSatisfactoria(Restaurant) :-
    not(rata(_,Restaurant)).

%2)

chef(Empleado,Restaurant) :-
    trabajaEn(Restaurant,Empleado),
    cocina(Empleado,_,_).

%3)

chefcito(Rata) :-
    trabajaEn(Restaurant,linguini),
    rata(Rata,Restaurant).

%4)

cocinaBien(Persona,Plato) :-
    cocina(Persona,Plato,Experiencia),
    Experiencia > 7.

cocinaBien(remy).

%5)

encargadoDe(Persona,Plato,Restaurant) :-
    trabajaEn(Restaurant,Persona),
    cocina(Persona,Plato,Experiencia1),
    forall((trabajaEn(Restaurant,OtraPersona),cocina(OtraPersona,Plato,Experiencia2),Persona \= OtraPersona),Experiencia1 > Experiencia2).

%6)

saludable(Plato) :-
    plato(Plato,Tipo),
    calculoDeCalorias(Tipo,Calorias),
    Calorias < 75.

calculoDeCalorias(entrada(Ingredientes),Calorias) :-
    length(Ingredientes,Cantidad)
    Calorias is Cantidad*15.

calculoDeCalorias(principal(pure,TiempoDeCoccion),Calorias) :-
    Calorias is 20+TiempoDeCoccion*5.

calculoDeCalorias(principal(papasFritas,TiempoDeCoccion),Calorias) :-
    Calorias is 50+TiempoDeCoccion*5.

calculoDeCalorias(principal(ensalada,TiempoDeCoccion),Calorias) :-
    Calorias is TiempoDeCoccion*5.

calculoDeCalorias(postre(Calorias),Calorias).

saludable(Plato) :-
    plato(Plato,postre(_)),
    grupo(Plato).

%7)

criticaPositiva(Critico,Restaurant) :-
    inspeccionSatisfactoria(Restaurant),
    esPositivaSegun(Critico,Restaurant).

espacialista(Restaurant,Plato) :-
    trabajaEn(Restaurant,_),
    forall(chef(Empleado,Restaurant),cocinaBien(Empleado,Plato)).

esPositivaSegun(antonEgo,Restaurant) :-
    especialista(Restaurant,ratatouille).

esPositivaSegun(christophe,Restaurant) :-
    trabajaEn(Restaurant,_),
    findall(Empleado,trabajaEn(Restaurant,Empleado),Empleados),
    length(Empleados,Cantidad),
    Cantidad > 3.

esPositivaSegun(cormillot,Restaurant) :-
    todosSaludables(Restaurant),
    entradasConZanahoria(Restaurant).

todosSaludables(Restaurant) :-
    forall((platoDeRestaurant(Restaurant,Plato)),saludable(Plato)).

entradasConZanahoria(Restaurant) :-
    forall((platoDeRestaurant(Restaurant,Plato),plato(Plato,entrada(Ingredientes)),member(zanahoria,Ingredientes))).

platoDeRestaurant(Restaurant,Plato) :-
    trabajaEn(Restaurant,Chef),
    cocina(Chef,Plato,_).
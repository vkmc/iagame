:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3    
	  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
% Dada un conjunto de metas, calcula el camino de menor costo
% y retorna el plan de movimientos que el agente debe realizar
% para llegar hasta alli
%
% +Metas - lista con las posiciones de las metas
% -Plan - plan de acciones que el agente debe realizar para llegar
% a la meta mas cercana (e.g., move(12), move(13),...)
% -Destino - Posicion con la meta seleccionada

buscar_plan_desplazamiento(Metas, Plan, Destino) :-

        % Detectar algoritmo de busqueda e inicializar frontera
        inicializar_frontera(Metas, Frontera),
        % write('Se ejecuta la '), detectar_algoritmo, write('con las metas: '), writeln(Metas),

        % Buscar camino
        buscar_camino(Metas, Frontera, [], Camino, Destino),
        % writeln('Camino solucion obtenido: '), writeln(Camino),

        % Generar plan
        generar_plan(Camino, Plan).
        % writeln('Plan a seguir: '), writeln(Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Inicializacion %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_frontera(+Metas, -FronteraInicial)
% Genera la frontera actual y construye una lista que contiene el nodo
% correspondiente a la posicion actual del agente.
%
% +Metas - Lista de posiciones que son meta
% -FronteraInicial - Frontera con el nodo correspondiente 
% al nodo actual del agente
%
% Representacion de nodos y arcos del arbol de busqueda
%
% costo(G,H,F), donde F(N) = G(N) + H(N)
%
% nodo(Pos, Costo, Camino), donde Pos se mapea
% directamente al nodo del entorno, Costo es la suma del costo 
% de llegar hacia ese nodo desde el inicio y su heuristica, y
% Camino es el camino hacia el nodo desde el inicio

inicializar_frontera(Metas, [Nodo]) :-        
        at([agent,me], PosActual),
        calcular_h_min(PosActual, Metas, Heuristica),
        Costo = costo(0, Heuristica, Heuristica),
        Nodo = nodo(PosActual, Costo, []).

% detectar_algoritmo
% Verifica la existencia del hecho UCS y adecua el calculo del costo heuristico
% para realizar este algoritmo.

detectar_algoritmo :-
        ucs,
        write('busqueda de costos uniforme (UCS) ').

detectar_algoritmo :-
        not(ucs),
        write('busqueda A* ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Busqueda %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% buscar_camino(+Metas, +Frontera, +Visitados, -Camino, -Destino)
% Dada una lista de metas y una frontera, retorna el camino
% de menor costo desde el nodo inicial hasta la meta.
% Puede ocurrir que falle si el camino a la meta no existe, sea porque
% no existe en el mapa o porque el camino no fue descubierto aun
%
% +Metas - Lista con las posiciones de las metas
% +Frontera - Conjunto de nodos que conforman la frontera actual
% +Visitados - Conjunto de nodos que ya fueron visitados
% -Camino - Lista de nodos que conforman el camino hacia la meta
% -Destino - Posicion de la meta elegida

% Caso base - Encuentra el nodo meta y retorna una solucion optimal

buscar_camino(Metas, Frontera, Visitados, Solucion, Destino) :-
        seleccionar(Frontera, Visitados, nodo(Destino,_,Camino), _FronteraSinActual, _VisitadosConActual),
        es_meta(Destino, Metas),
        reverse([Destino|Camino], Solucion).

% Caso recursivo - Agrega el nodo al camino actual y sigue buscando

buscar_camino(Metas, Frontera, Visitados, Camino, Destino) :-
        seleccionar(Frontera, Visitados, PosActual, FronteraSinActual, VisitadosConActual),
        vecinos(PosActual, Vecinos),
        agregar_a_frontera(PosActual, Metas, Vecinos, FronteraSinActual, FronteraNueva, VisitadosConActual, VisitadosNuevo),
        buscar_camino(Metas, FronteraNueva, VisitadosNuevo, Camino, Destino).

% seleccionar(+Frontera, +Visitados, -NodoMenorCosto, -FronteraSinMenor, -VisitadosConMenor)
%
% +Frontera - Conjunto de nodos que conforman la frontera actual
% +Visitados - Conjunto de nodos que ya fueron visitados
% -NodoMenorCosto - Nodo cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros nodos.
% -VisitadosConMenor - Conjunto de visitados al que se le agrega el nodo % cuya estimacion del costo total hacia la meta es menor en comparacion
% con otros nodos

seleccionar(Frontera, Visitados, NodoMenorCosto, FronteraSinMenor, [NodoMenorCosto|Visitados]) :-
        elegir_menor_costo(Frontera, NodoMenorCosto),
        delete(Frontera, NodoMenorCosto, FronteraSinMenor),!.

% elegir_menor_costo(+Frontera, -NodoMenorCosto)
% Busca en la frontera el nodo cuya estimacion del costo total hacia
% la meta sea menor en comparacion con otros nodos.
%
% +Frontera - Conjunto de nodos que conforman la frontera actual
% -NodoMenorCosto - Nodo cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros nodos.

elegir_menor_costo([Nodo|Frontera], NodoMenorCosto) :-
        elegir_menor_costo_aux(Frontera, Nodo, NodoMenorCosto).

% elegir_menor_costo_aux(+Frontera, -NodoMenorActual, -NodoMenorCosto)
% Realiza la busqueda recursiva del nodo con menor costo
%
% +Frontera - Conjunto de nodos que conforman la frontera actual
% -NodoMenorActual - Nodo cuya estimacion del costo total hacia
% la meta es, por el momento, la menor.
% -NodoMenorCosto - Nodo cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros nodos.

% Caso base - Frontera vacia

elegir_menor_costo_aux([], NodoMenorCosto, NodoMenorCosto).

% Caso recursivo - Uno o mas nodos en la frontera
% El nodo de la frontera que fue seleccionado 
% tiene menor costo que el nodo que era menor hasta el momento

elegir_menor_costo_aux([Nodo|Frontera], NodoMenorActual, NodoMenorCosto) :-
        Nodo = nodo(_,costo(_,_,F2),_),
        NodoMenorActual = nodo(_,costo(_,_,F1),_),
        F2 < F1,
        NodoMenorNuevo = Nodo,
        elegir_menor_costo_aux(Frontera, NodoMenorNuevo, NodoMenorCosto).

% Caso recursivo - Uno o mas nodos en la frontera
% El nodo menor sigue siendo el menor en comparacion 
% con otros nodos

elegir_menor_costo_aux([_Nodo|Frontera], NodoMenorActual, NodoMenorCosto) :-
        elegir_menor_costo_aux(Frontera, NodoMenorActual, NodoMenorCosto).

% es_meta(+Destino, +Metas)
% Tiene exito si Destino pertenece al conjunto de Metas
% y falla en caso contrario

es_meta(Destino, Metas) :- member(Destino, Metas).

% agregar_a_frontera(+NodoActual, +Metas, +Vecinos, +Frontera, -NuevaFrontera, +Visitados, -NuevoVisitados)
% Agrega un nodo a la frontera
%
% +NodoActual - Nodo en el que se encuentra actualmente 
% en el recorrido del arbol
% +Metas - Lista con las posiciones de las metas
% +Vecinos - Conjunto de nodos adyacentes al nodo actual
% +Frontera - Conjunto de nodos que conforman la frontera actual
% -NuevaFrontera - Conjunto de nodos de la frontera, incluyendo a los vecinos del nodo actual
% +Visitados - Conjunto de nodos que ya fueron visitados
% -NuevoVisitados - Conjunto de nodos visitados incluyendo al nodo actual

% Caso 'Nuevo nodo encontrado' - El nodo no esta ni en la frontera,
% ni en el conjunto de visitados

agregar_a_frontera(NodoActual, Metas, [Vecino|Vecinos], Frontera, [NuevoNodo|NuevaFrontera], Visitados, NuevoVisitados) :-
        NodoActual = nodo(PosActual, _CostoActual, CaminoActual),
        no_visitado(Vecino, Frontera, Visitados),
        calcular_costo(NodoActual, Vecino, Metas, Costo),
        NuevoNodo = nodo(Vecino, Costo, [PosActual|CaminoActual]),
        agregar_a_frontera(NodoActual, Metas, Vecinos, Frontera, NuevaFrontera, Visitados, NuevoVisitados),!.

% Caso 'En frontera' - El nodo ya esta en el conjunto frontera,
% pero el nuevo costo es mejor

agregar_a_frontera(NodoActual, Metas, [Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        NodoActual = nodo(PosActual, _CostoActual, CaminoActual),
        calcular_costo(NodoActual, Vecino, Metas, Costo),
        NuevoNodo = nodo(Vecino, Costo, [PosActual|CaminoActual]),

        reemplazar_si_es_menor(NuevoNodo, Frontera, NuevaFronteraConMenor),
        agregar_a_frontera(NodoActual, Metas, Vecinos, NuevaFronteraConMenor, NuevaFrontera, Visitados, NuevoVisitados),!.

% Caso 'En visitados' - El nodo ya esta en el conjunto visitados,
% pero el nuevo costo es mejor

agregar_a_frontera(NodoActual, Metas, [Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        NodoActual = nodo(PosActual, _CostoActual, CaminoActual),
        calcular_costo(NodoActual, Vecino, Metas, Costo),
        NuevoNodo = nodo(Vecino, Costo, [PosActual|CaminoActual]),
        reemplazar_si_es_menor(NuevoNodo, Visitados, NuevoVisitadosConMenor),
        agregar_a_frontera(NodoActual, Metas, Vecinos, Frontera, NuevaFrontera, NuevoVisitadosConMenor, NuevoVisitados),!.

% Caso base - El conjunto de vecinos es vacio
agregar_a_frontera(_NodoActual, _Metas, [], Frontera, Frontera, Visitados, Visitados).

% Cualquier otro caso en el que no se modifica ni la frontera
% ni los visitados

agregar_a_frontera(NodoActual, Metas, [_Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        agregar_a_frontera(NodoActual, Metas, Vecinos, Frontera, NuevaFrontera, Visitados, NuevoVisitados).

% reemplazar_si_es_menor(+NodoNuevo, +ConjuntoNodos, +ConjuntoNodosNuevo)
% Busca un nodo determinado dentro de un conjunto de nodos  
% y, de encontrarlo, compara los costos totales asociados.
% Si el nuevo nodo tiene menor costo, entonces lo reemplaza.
%
% +NodoNuevo - Nodo a buscar dentro de ConjuntoNodos
% +ConjuntoNodos - Conjunto de nodos actuales
% (en la frontera o ya visitados)
% -ConjuntoNodosNuevo - Conjunto de nodos actuales
% (en la frontera o ya visitados) posiblemente modificada

reemplazar_si_es_menor(NodoNuevo, ConjuntoNodos, ConjuntoNodosNuevo) :-
        NodoNuevo = nodo(Pos, costo(_G, _H, F2), _Camino),
        member(nodo(Pos,_,_), ConjuntoNodos),
        selectchk(nodo(Pos, costo(_,_,F1),_),ConjuntoNodos, NodoNuevo,ConjuntoNodosNuevo),
        F2 < F1.
        
% calcular_costo(NodoAnterior, NodoActual, Metas, Costo)
% Dado un nodo, el nodo anterior a el y un conjunto de metas,
% calcula el menor costo a alguna de las metas del conjunto de metas
% Representacion del Costo - costo(G, H, F) , donde G es el costo
% del camino realizado para alcanzar NodoActual, H es el costo estimado
% para alcanzar a la meta a partir del NodoActual y F es la suma de G y H
%
% +NodoAnterior - Vecino anterior directo que posee informacion
% de costo relevante para NodoActual
% +NodoActual - Nodo al cual se quiere calcular el costo
% +Metas - Lista con las posiciones de las metas
% -Costo - Costos asociados a NodoActual

calcular_costo(NodoAnterior, NodoActual, Metas, Costo) :- 
	calcular_g(NodoAnterior, NodoActual, G),
	calcular_h_min(NodoActual, Metas, H),
	F is G + H,
	Costo = costo(G, H, F).

% calcular_g(NodoAnterior, NodoActual, Costo)
% Determina el costo del camino realizado para alcanzar NodoActual
% desde el NodoAnterior y se suma al costo del camino realizado desde
% los nodos previos hasta el NodoAnterior
%
% +NodoAnterior - Nodo anterior directo al nodo actual
% +NodoActual - Nodo actual al cual se quiere calcular el costo
% -Costo - Entero con el costo del camino desde la posicion
% desde donde se comenzo la busuqeda hasta el nodo actual

calcular_g(NodoAnterior, NodoActual, Costo) :-
        NodoAnterior = nodo(IdAnterior, costo(G1,_,_),_),
        ady(IdAnterior, NodoActual, G2),
        Costo is G1 + G2.    

% calcular_h_min(+Nodo, +Metas, -MenorHeuristica)
% Dado un nodo y una lista de metas, 
% devuelve la heuristica calculada de menor valor
% hacia alguna de las metas.
%
% +Nodo - Nodo del cual se quiere obtener la estimacion heuristica
% +Metas - Lista con las posiciones de las metas
% -MenorHeuristica - Menor valor heuristico obtenido

% Caso busqueda de costos uniformes (UCS)

calcular_h_min(_Nodo, _Metas, 0) :- ucs.

% Caso busqueda A*

calcular_h_min(Nodo, Metas, MenorHeuristica) :-
        
        findall(
                        Heuristica,
                (
                        member(NodoMeta, Metas),
                        calcular_h(Nodo, NodoMeta, Heuristica)
                ),
                        ListaHeuristicas
               ),

        min_list(ListaHeuristicas, MenorHeuristica).

% calcular_h(+Nodo, +NodoMeta, -Heuristica)
% Implementacion de la formula de distancia (o Manhattan) para 3 variables
%
% +Nodo - Nodo del cual se quiere obtener la estimacion heuristica
% +NodoMeta - Nodo Meta seleccionada para obtener la estimacion 
% heuristica
% -Heuristica - Estimacion del costo del camino desde el nodo a la meta

calcular_h(Nodo, NodoMeta, Heuristica) :-
        node(Nodo, VectorNodo, _),
        node(NodoMeta, VectorNodoMeta, _),
        distance(VectorNodo, VectorNodoMeta, Heuristica).

% no_visitado(+Id, +Frontera, +Visitados)
% Determina si el nodo identificado con Id esta o no presente en la 
% Frontera o en Visitados
% +Id - Id del nodo a controlar
% +Frontera - Conjunto de nodos que conforman la frontera actual
% +Visitados - Conjunto de nodos que ya fueron visitados

no_visitado(Id, Frontera, Visitados) :-
	not(member(nodo(Id,_,_), Visitados)), 
	not(member(nodo(Id,_,_), Frontera)).

% vecinos(+Nodo, -Vecinos)
% Dado un nodo se obtiene la lista de nodos adyacentes
% proximos transitables
%
% +Nodo - Nodo del cual se quieren obtener los vecinos
% -Vecinos - Conjunto de nodos adyacentes al nodo actual

vecinos(nodo(Nodo,_Costo,_Camino), Vecinos):-
	findall(
                        Adyacente,
                (
                        node(Nodo, _PosNodo, Connections),
                        member([Adyacente,_CostoAdyacente], Connections)
                ),
                        Vecinos
               ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Plan de acciones %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generar_plan(+Camino, -Plan)
% Usa el predicado generar_plan_aux/3 para construir 
% una lista de acciones a partir del camino hallado
% por el algoritmo de busqueda. 
%
% +Camino - Lista de nodos que guian al agente de su posicion actual
% hacia la meta
% -Plan - Conjunto de acciones que debe realizar el agente para desplazarse
% desde su posicion actual hacia la meta

generar_plan([NodoActual|Camino], Plan) :-
        generar_plan_aux(NodoActual, Camino, Plan).

% generar_plan_aux(+NodoActual, +Camino -Plan)
%
% +NodoActual - Nodo a traducir
% +Camino - Lista de nodos que guian al agente de su posicion actual
% hacia la meta
% -Plan - Conjunto de acciones que debe realizar el agente para desplazarse
% desde su posicion actual hacia la meta
% Caso base - Conjunto de nodos vacio

generar_plan_aux(_NodoActual, [NodoSiguiente], [move(NodoSiguiente)]) :- !.

% Caso recursivo - Al menos un nodo para visitar

generar_plan_aux(_NodoActual, [NodoSiguiente|Camino], [move(NodoSiguiente)|Plan]) :-
        generar_plan_aux(NodoSiguiente, Camino, Plan),!.

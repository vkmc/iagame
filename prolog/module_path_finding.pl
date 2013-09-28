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

        % Inicializar frontera
        inicializar_frontera(Metas, Frontera),
        writeln('Se ejecuta A* con las metas: '), writeln(Metas),

        buscar_camino(Metas, Frontera, [], Camino, Destino),
        writeln('Camino solucion obtenido: '), writeln(Camino),

        generar_plan(Camino, Plan),
        writeln('Plan a seguir: '), writeln(Plan).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Inicializacion %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_frontera(+Metas, -FronteraInicial)
% Genera la frontera actual y construye una lista que contiene el estado
% correspondiente a la posicion actual del agente.
%
% +Metas - Lista de posiciones que son meta
% - FronteraInicial - Frontera con el estado correspondiente 
% al estado actual del agente
%
% Representacion de nodos y arcos del arbol de busqueda
%
% costo(G,H,F), donde F(N) = G(N) + H(N)
%
% estado(Pos, Costo, Camino), donde Pos se mapea
% directamente al nodo del entorno, Costo es la suma del costo 
% de llegar hacia ese nodo desde el inicio y su heuristica, y
% Camino es el camino hacia el estado desde el inicio

inicializar_frontera(Metas, [Nodo]) :-        
        at([agent,me], PosActual),
        calcular_h_min(PosActual, Metas, H),
        Costo = costo(0, H, H),
        Nodo = estado(PosActual, Costo, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Algoritmo A* %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% buscar_camino(+Metas, +Frontera, +Visitados, -Camino, -Destino)
% Dada una lista de metas y una frontera, retorna el camino
% de menor costo desde el estado inicial hasta la meta.
% Puede ocurrir que falle si el camino a la meta no existe, sea porque
% no existe en el mapa o porque el camino no fue descubierto aun
%
% +Metas - Lista con las posiciones de las metas
% +Frontera - Conjunto de estados que conforman la frontera actual
% +Visitados - Conjunto de estados que ya fueron visitados
% -Camino - Lista de estados que conforman el camino hacia la meta
% -Destino - Posicion de la meta elegida

% Caso base - Encuentra el estado meta y retorna una solucion optimal

buscar_camino(Metas, Frontera, Visitados, Solucion, Destino) :-
        seleccionar(Frontera, Visitados, estado(Destino,_,Camino), _FronteraSinActual, _VisitadosConActual),
        es_meta(Destino, Metas),
        reverse([Destino|Camino], Solucion).

% Caso recursivo - Agrega el estado al camino actual y sigue buscando
buscar_camino(Metas, Frontera, Visitados, Camino, Destino) :-
        seleccionar(Frontera, Visitados, PosActual, FronteraSinActual, VisitadosConActual),
        vecinos(PosActual, Vecinos),
        agregar_a_frontera(PosActual, Metas, Vecinos, FronteraSinActual, FronteraNueva, VisitadosConActual, VisitadosNuevo),
        buscar_camino(Metas, FronteraNueva, VisitadosNuevo, Camino, Destino).

% seleccionar(+Frontera, +Visitados, -EstadoMenorCosto, -FronteraSinMenor, -VisitadosConMenor)
%
% +Frontera - Conjunto de estados que conforman la frontera actual
% +Visitados - Conjunto de estados que ya fueron visitados
% -EstadoMenorCosto - Estado cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros estados.
% -VisitadosConMenor - Conjunto de visitados al que se le agrega el estado % cuya estimacion del costo total hacia la meta es menor en comparacion
% con otros estados

seleccionar(Frontera, Visitados, EstadoMenorCosto, FronteraSinMenor, [EstadoMenorCosto|Visitados]) :-
        elegir_menor_costo(Frontera, EstadoMenorCosto),
        delete(Frontera, EstadoMenorCosto, FronteraSinMenor),!.

% elegir_menor_costo(+Frontera, -EstadoMenorCosto)
% Busca en la frontera el estado cuya estimacion del costo total hacia
% la meta sea menor en comparacion con otros estados.
%
% +Frontera - Conjunto de estados que conforman la frontera actual
% -EstadoMenorCosto - Estado cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros estados.

elegir_menor_costo([Estado|Frontera], EstadoMenorCosto) :-
        elegir_menor_costo_aux(Frontera, Estado, EstadoMenorCosto).

% elegir_menor_costo_aux(+Frontera, -EstadoMenorActual, -EstadoMenorCosto)
% Realiza la busqueda recursiva del estado con menor costo
%
% +Frontera - Conjunto de estados que conforman la frontera actual
% -EstadoMenorActual - Estado cuya estimacion del costo total hacia
% la meta es, por el momento, la menor.
% -EstadoMenorCosto - Estado cuya estimacion del costo total 
% hacia la meta es menor en comparacion con otros estados.

% Caso base - Frontera vacia

elegir_menor_costo_aux([], EstadoMenorCosto, EstadoMenorCosto).

% Caso recursivo - Uno o mas estados en la frontera
% El estado de la frontera que fue seleccionado 
% tiene menor costo que el estado que era menor hasta el momento

elegir_menor_costo_aux([Estado|Frontera], EstadoMenorActual, EstadoMenorCosto) :-
        Estado = estado(_,costo(_,_,F2),_),
        EstadoMenorActual = estado(_,costo(_,_,F1),_),
        F2 < F1,
        EstadoMenorNuevo = Estado,
        elegir_menor_costo(Frontera, EstadoMenorNuevo, EstadoMenorCosto).

% Caso recursivo - Uno o mas estados en la frontera
% El estado menor sigue siendo el menor en comparacion 
% con otros estados

elegir_menor_costo_aux([_Estado|Frontera], EstadoMenorActual, EstadoMenorCosto) :-
        elegir_menor_costo(Frontera, EstadoMenorActual, EstadoMenorCosto).

% es_meta(+Destino, +Metas)
% Tiene exito si Destino pertenece al conjunto de Metas
% y falla en caso contrario
es_meta(Destino, Metas) :- member(Destino, Metas).

% agregar_a_frontera(+EstadoActual, +Metas, +Vecinos, +Frontera, -NuevaFrontera, +Visitados, -NuevoVisitados)


% +EstadoActual - Estado en el que se encuentra actualmente 
% en el recorrido del arbol
% +Metas - Lista con las posiciones de las metas
% +Vecinos - Conjunto de estados adyacentes al estado actual
% +Frontera - Conjunto de estados que conforman la frontera actual
% -NuevaFrontera - Conjunto de estados de la frontera, incluyendo a los vecinos del estado actual
% +Visitados - Conjunto de estados que ya fueron visitados
% -NuevoVisitados - Conjunto de estados visitados incluyendo al estado actual

% Caso 'Nuevo estado encontrado' - El estado no esta ni en la frontera,
% ni en el conjunto de visitados

agregar_a_frontera(EstadoActual, Metas, [Vecino|Vecinos], Frontera, [EstadoNuevo|NuevaFrontera], Visitados, NuevoVisitados) :-
        EstadoActual = estado(PosActual, _CostoActual, CaminoActual),
        no_visitado(Vecino, Frontera, Visitados),
        calcular_costo(EstadoActual, Vecino, Metas, Costo),
        EstadoNuevo = estado(Vecino, Costo, [PosActual|CaminoActual]),
        agregar_a_frontera(EstadoActual, Metas, Vecinos, Frontera, NuevaFrontera, Visitados, NuevoVisitados),!.

% Caso 'En frontera' - El estado ya esta en el conjunto frontera,
% pero el nuevo costo es mejor

agregar_a_frontera(EstadoActual, Metas, [Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        EstadoActual = estado(PosActual, _CostoActual, CaminoActual),
        calcular_costo(EstadoActual, Vecino, Metas, Costo),
        EstadoNuevo = estado(Vecino, Costo, [PosActual|CaminoActual]),
        reemplazar_si_es_menor(EstadoNuevo, Frontera, NuevaFronteraConMenor),
        agregar_a_frontera(EstadoActual, Metas, Vecinos, NuevaFronteraConMenor, NuevaFrontera, Visitados, NuevoVisitados),!.

% Caso 'En visitados' - El estado ya esta en el conjunto visitados,
% pero el nuevo costo es mejor

agregar_a_frontera(EstadoActual, Metas, [Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        EstadoActual = estado(PosActual,_CostoActual,CaminoActual),
        calcular_costo(EstadoActual, Vecino, Metas, Costo),
        EstadoNuevo = estado(Vecino, Costo, [PosActual|CaminoActual]),
        reemplazar_si_es_menor(EstadoNuevo, Visitados, NuevoVisitadosConMenor),
        agregar_a_frontera(EstadoActual, Metas, Vecinos, Frontera, NuevaFrontera, NuevoVisitadosConMenor, NuevoVisitados),!.

% Caso base - El conjunto de vecinos es vacio
agregar_a_frontera(_EstadoActual, _Metas, [], Frontera, Frontera, Visitados, Visitados).

% Cualquier otro caso en el que no se modifica ni la frontera
% ni los visitados
agregar_a_frontera(EstadoActual, Metas, [_Vecino|Vecinos], Frontera, NuevaFrontera, Visitados, NuevoVisitados) :-
        agregar_a_frontera(EstadoActual, Metas, Vecinos, Frontera, NuevaFrontera, Visitados, NuevoVisitados).

% reemplazar_si_es_menor(+EstadoNuevo, +ConjuntoEstados, +ConjuntoEstadosNuevo)
% Busca un estado determinado dentro de un conjunto de estados  
% y, de encontrarlo, compara los costos totales asociados.
% Si el nuevo estado tiene menor costo, entonces lo reemplaza.
%
% +EstadoNuevo - Estado a buscar dentro de ConjuntoEstado
% +ConjuntoEstados - Conjunto de estados actuales
% (en la frontera o ya visitados)
% -ConjuntoEstadosNuevo - Conjunto de estados actuales
% (en la frontera o ya visitados) posiblemente modificada

reemplazar_si_es_menor(EstadoNuevo, ConjuntoEstados, ConjuntoEstadosNuevo) :-
        EstadoNuevo = estado(Pos, costo(_G, _H, F2), _Camino),
        member(estado(Pos,_,_), ConjuntoEstados),
        selectchk(estado(Pos, costo(_,_,F1),_),ConjuntoEstados, EstadoNuevo,ConjuntoEstadosNuevo),
        F2 < F1.
        
% calcular_costo(EstadoAnterior, EstadoActual, Metas, Costo)
% Dado un estado, el estado anterior a el y un conjunto de metas,
% calcula el menor costo a alguna de las metas del conjunto de metas
% Representacion del Costo - costo(G, H, F) , donde G es el costo
% del camino realizado para alcanzar EstadoActual, H es el costo estimado
% para alcanzar a la meta a partir del EstadoActual y F es la suma de G y H
%
% +EstadoAnterior - Vecino anterior directo que posee informacion
% de costo relevante para EstadoActual
% +EstadoActual - Estado al cual se quiere calcular el costo
% +Metas - Lista con las posiciones de las metas
% -Costo - Costos asociados a EstadoActual
calcular_costo(EstadoAnterior, EstadoActual, Metas, Costo) :- 
	calcular_g(EstadoAnterior, EstadoActual, G),
	calcular_h_min(EstadoActual, Metas, H),
	F is G + H,
	Costo = costo(G, H, F).

% calcular_g(EstadoAnterior, EstadoActual, Costo)
% Determina el costo del camino realizado para alcanzar EstadoActual
% desde el EstadoAnterior y se suma al costo del camino realizado desde
% los estados previos hasta el EstadoAnterior
%
% +EstadoAnterior - Estado anterior directo al estado actual
% +EstadoActual - Estado actual al cual se quiere calcular el costo
% -Costo - Entero con el costo del camino desde la posicion
% desde donde se comenzo la busuqeda hasta el estado actual

calcular_g(EstadoAnterior, EstadoActual, Costo) :-
        EstadoAnterior = estado(node(_,_,Adyacentes),costo(G1,_,_),_),
        intersection(Adyacentes, [[EstadoActual,G2]], [[EstadoActual,G2]]),
        Costo is G1 + G2.        

% calcular_h_min(+Estado, +Metas, -MenorH)
% Dado un estado y una lista de metas, 
% devuelve la heuristica calculada de menor valor
% hacia alguna de las metas.
%
% +Estado - Estado del cual se quiere obtener la estimacion heuristica
% +Metas - Lista con las posiciones de las metas
% -MenorH - Menor valor heuristico obtenido

calcular_h_min(Estado, Metas, MenorH) :-
        node(Estado,EstadoV,_),
        findall(
                        H,
                (
                        member(EstadoMeta, Metas),
                        node(EstadoMeta, EstadoMetaV, _),
                        calcular_h(EstadoV, EstadoMetaV, H)
                ),
                        ListaHeuristicas
               ),
        min_list(ListaHeuristicas, MenorH).

% calcular_h(+EstadoVector, +EstadoMetaVector, -H)
% Implementacion de la formula de distancia (o Manhattan) para 3 variables
%
% +EstadoVector - Terna de puntos que representan un nodo del mapa 
% asociado al estado actual
% +EstadoMetaVector - Terna de puntos que representan un nodo del mapa 
% asociado al estado meta
% -H - Estimacion del costo del camino desde el estado a la meta

calcular_h(vector(X1,Y1,Z1), vector(X2,Y2,Z2), H) :-
        X is X2 - X1, pow(X,2,PX),
        Y is Y2 - Y1, pow(Y,2,PY),
        Z is Z2 - Z1, pow(Z,2,PZ),
        S is PX + PY + PZ,
        sqrt(S,H).

% no_visitado(+Estado, +Frontera, +Visitados)
% Determina si el Estado esta o no presente en la Frontera o en Visitados
% +Estado - Estado a controlar
% +Frontera - Conjunto de estados que conforman la frontera actual
% +Visitados - Conjunto de estados que ya fueron visitados

no_visitado(Estado, Frontera, Visitados) :-
	not(member(estado(Estado,_,_), Visitados)), 
	not(member(estado(Estado,_,_), Frontera)).

% vecinos(+Estado, -Vecinos)
% Dado un estado se obtiene la lista de estados adyacentes
% proximos transitables
%
% +Estado - Estado del cual se quieren obtener los vecinos
% -Vecinos - Conjunto de estados adyacentes al estado actual
vecinos(Estado, Vecinos) :-
        Estado = estado(node(_,_,Adyacentes),_,_),
        findall(
                Vecino,
                member([Vecino,_],Adyacentes),
                Vecinos
               ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Plan de acciones %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generar_plan(+Camino, -Plan)
% Construye una lista de acciones a partir del camino hallado
% por el algoritmo A*.
%
% +Camino - Lista de estados que guian al agente de su posicion actual
% hacia la meta
% -Plan - Conjunto de acciones que debe realizar el agente para desplazarse
% desde su posicion actual hacia la meta

% Caso base - Conjunto de estados vacio
generar_plan([], []).

% Caso recursivo - Al menos un estado para visitar
generar_plan([Pos|Camino], [move(Pos)|Plan]) :-
        generar_plan(Camino, Plan).
        










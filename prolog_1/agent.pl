:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1, buscar_metas/1, ucs/0.

plan([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      update_beliefs(Percept), !,

      % display_ag, nl,

      % writeln('Decidiendo accion...'),

      decide_action(Action), !,

      do_action(Action),

      run.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cambiar_decision(+Metas)
% Si encuentra metas, se prioriza ir hacia ellas que seguir explorando
%
% +Metas - Conjunto de metas

cambiar_decision([]).

cambiar_decision(_Metas) :-
        write('En el camino encontre metas... '),
        retract(plan(_)),
        assert(plan([])).

% decide_action(-Action)
% Determina la proxima accion que realizara el agente
%
% -Action - Proxima accion

% Energia baja
decide_action(Action) :-
	at([agent, me],Pos),
	at([inn, IName],Pos),
        write('Llegue a la posada '), write(IName),
        write(' en '), writeln(Pos),
	property([agent,me],life,Energia),
	property([agent,me],lifeTotal,EnergiaMax),
	EnergiaCritica is EnergiaMax - 70,
	Energia < EnergiaMax,
	Action = noop,
	writeln('Descansando en posada... ').

% Encuentra oro
decide_action(Action) :-
	at([agent,me],Pos),
	at([gold,GName],Pos),
	writeln('Encontre oro'),
        writeln('Voy a intentar agarrarlo... '),
        Action = pickup([gold, GName]),
        writeln('Juntando oro... ').

% Pelea con otros agentes (no es parte de la Etapa 1)
decide_action(Action) :-
	atPos([agent,me],MyPos),
	atPos([agent,Target],TPos),
	Target \= me,
	property([agent,Target],life,TLife),
	TLife > 0,
	pos_in_attack_range(MyPos,TPos),
	Action = attack([agent,Target]),
        write('Atacando a '), write(Target), writeln('!').

% Plan de acciones: acciones a seguir
decide_action(Action) :-
	plan(Plan),
	Plan \= [],
	Plan = [Action|Acciones],
	retractall(plan(_)),
	assert(plan(Acciones)),
	% write('Ejecutando plan de acciones. Proxima accion: '),
	% writeln(Action),
        % write('Quedan por ejecutar '),
        % writeln(Acciones),
        buscar_metas(Metas),
        cambiar_decision(Metas).

% No hay plan de acciones pero hay metas, se ejecuta algoritmo A*  
decide_action(Action) :-
	plan([]),
	buscar_metas(Metas), % Se buscan metas
	Metas \= [],
        % Se efectua una busqueda A* con las metas obtenidas
        retractall(ucs),
        assert(ucs :- false),
        % writeln('Buscando un camino... '),
	buscar_plan_desplazamiento(Metas, [Action|Acciones], _Destino),	
	% write('Plan de acciones para ir a '), write(Destino),
	% write(': '), writeln([Action|Acciones]),
	retractall(plan(_)),
	assert(plan(Acciones)).

% No existen metas, el agente explora en busca de metas
decide_action(Action) :-
        writeln('No encontraron metas. El agente va a explorar.'),
	explorar(Action).

decide_action(Action) :-
        not(at([gold,_],_)),    % Si no unifica con ninguna de las anteriores y no hay mas oro que juntar, termina la exploracion
        writeln('Ya se exploro todo el mapa. El agente no tiene nada mas por hacer.'),
	Action = noop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Auxiliares %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% explorar(-Action)
% Si el agente no tiene ninguna meta, opta por explorar
%
% -Action - Proxima accion a ejecutar por el agente

% En lo posible, intentara explorar aquellas posiciones que
% no conozca en absoluto y con algo de suerte encontrara una meta
% mas promisoria

explorar(Action) :-
        % write('Buscar posiciones no exploradas: '),
        % Se obtiene el conjunto de posiciones conocidas por el agente que aun no han sido exploradas
	findall(        Pos,
                (
                        node(Pos,_,Connections),                        
                        not(visitados(Pos)),
                        not(interior(Connections))
                ),
                        SinExplorar
               
               ),
	SinExplorar \= [],
	% writeln(SinExplorar),
        retractall(ucs),
        assert(ucs :- true),
        % Se efectua una busqueda UCS con las metas obtenidas
        buscar_plan_desplazamiento(SinExplorar, [Action|Acciones], _Destino),
	% write('Explorando - Posicion destino: '),
        % writeln(Destino),
        % write('Explorando - Acciones a realizar: '),
	% writeln(Acciones),
        % Eliminamos planes de accion anteriores
	retract(plan(_)),
	% Nuevo plan de acciones a seguir
	assert(plan(Acciones)).

% Si ya vio todo el mapa, seguira explorando por caminos conocidos

explorar(Action) :-
        % write('Buscar posiciones no exploradas: '),
        % Se obtiene el conjunto de posiciones conocidas por el agente que aun no han sido exploradas
	findall(        Pos,
                (
                        node(Pos,_,_),                        
                        not(visitados(Pos))
                ),
                        SinExplorar
               
               ),
	SinExplorar \= [],
	writeln(SinExplorar),
        retractall(ucs),
        assert(ucs :- true),
        % Se efectua una busqueda UCS con las metas obtenidas
        buscar_plan_desplazamiento(SinExplorar, [Action|Acciones], _Destino),
	% write('Explorando - Posicion destino: '),
        % writeln(Destino),
        % write('Explorando - Acciones a realizar: '),
	% writeln(Acciones),
        % Eliminamos planes de accion anteriores
	retract(plan(_)),
	% Nuevo plan de acciones a seguir
	assert(plan(Acciones)).
      

% interior(+Connections)
% Determina si los vecinos de una posicion dada se encuentran en el radio
% de vision del agente. No se exploran posiciones que se sean visibles para el agente,
% de este modo se mejora la eficiencia de la exploracion.
%
% +Connections - Lista de posiciones adyacentes a una posicion dada.

interior([]).

interior([[Ady,_]|Connections]) :- node(Ady,_,_), interior(Connections).

% buscar_metas(-Metas)
% Selecciona proxima meta segun los intereses y requerimientos
% del agente
%
% -Metas - Conjunto de metas del estado actual

% El agente tiene bajo nivel de vida

buscar_metas(Metas):- 
        property([agent,me],life,Energia),
        Energia < 70,
	findall(InnPos,
                (
                        at([inn,IName], InnPos),
                        node(InnPos, InnVector, _),
                        at([agent, me], MyPos),
                        node(MyPos, MyVector, _),
                        entity_descr([inn, IName], Propiedades),
                        member([forbidden_entry, EntradaProhibida], Propiedades),
                        distance(MyVector, InnVector, InnDist),
                        entrada_habilitada(EntradaProhibida, InnDist)
                ),
                        Metas),
        at([inn,IName], InnPos),
        entity_descr([inn, IName], Propiedades),
        write('Metas (posada): '),write(IName),write(' '),write(Propiedades),write(' en '),writeln(InnPos).

% El agente busca oro

buscar_metas(Metas):-
        findall(GPos,
                at([gold,_],GPos),
                Metas),
        at([gold,GName],GPos),
        entity_descr([gold,GName],Propiedades),
        write('Metas (oro): '),write(GName),write(' '),write(Propiedades),write(' en '),writeln(GPos).

% entrada_habilitada(+ListaNegra, +Distancia)
% Determina si la entrada del agente a una posada esta prohibida
%
% +ListaNegra - Lista de agentes que tienen prohibida la entrada
% a la posada
% +Distancia - Distancia del el agente a la posada

% La entrada esta habilitada - Lista negra de agentes vacia

entrada_habilitada([],_) :- !.

% La entrada esta habilitada - No formo parte de la lista negra

entrada_habilitada(EntradaProhibida,_Distancia) :-
        not(member([me,_ForbiddenUntil],EntradaProhibida)), !.

% La entrada esta inhabilitada - Formo parte de la lista negra
% Se calcula si pasado el tiempo tomado para llegar hacia alli
% tendre nuevamente la entrada habilitada

entrada_habilitada(EntradaProhibida,Distancia) :-
        member([me,ForbiddenUntil],EntradaProhibida),
        time(T),
        T >= ForbiddenUntil,
        TiempoRestante is ForbiddenUntil - T,
        Distancia >= TiempoRestante.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic ag_name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = dumbtrooper,

           agent_init(AgName),
           assert(ag_name(AgName)),
	   agent_reset,
           connect,
           run,

           disconnect.

s:- start_ag.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.

start_ag_instance(InstanceID):-
                    AgClassName = dumbtrooper,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),



		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).

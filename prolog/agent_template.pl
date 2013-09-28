%% Player-Agent Template
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1, buscar_metas/1, visitados/1.

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

      display_ag, nl,

      writeln('Decidiendo accion...'),
      decide_action(Action),

      do_action(Action),

      run.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Energia baja
decide_action(Action) :-
	at([agent, me],Pos),
	at([inn,_],Pos),
	property([agent,me],life,AgLife),
	property([agent,me],lifeTotal,AgLifeTotal),
	AgLifeWarning is AgLifeTotal - 40,
	AgLife < AgLifeWarning,
	Action = noop,
	writeln('Descansando en posada...'),!.

decide_action(Action):-
	at([agent, me], Pos),
	at([gold, GName], Pos),
	write('Encontré un tesoro: '), write(GName), write('!'),nl,
        write('voy a intentar tomarlo...'),nl,
        Action = pickup([gold, GName]).

%decide_action(Action):-
%	atPos([agent, me], MyPos),
%	atPos([agent, Target], TPos),
%	Target \= me,
%	property([agent, Target], life, TLife),
%	TLife > 0,
%	pos_in_attack_range(MyPos, TPos),
%	Action = attack([agent, Target]).

% Plan de acciones: acciones a seguir
decide_action(Action):-
	plan(Plan),
	Plan \= [],
	Plan = [Action|Acciones],
	retract(plan(_)),
	assert(plan(Acciones)),
	write('Aun ejecutando plan de acciones. Proxima accion:\n\t'),
	writeln(Action),
	write(Action),!.

% No hay plan de acciones pero hay metas, se ejecuta algoritmo A*  
decide_action(Action) :-
	plan([]),
	buscar_metas(Metas), % Se buscan metas
	Metas \= [],
	write(Metas),
        % Se ejecuta el predicado que contiene el A* con las metas obtenidas anteriormente
	buscar_plan_desplazamiento(Metas,[Action|Acciones],Destino),
	writeln('Se buscará un camino con A*'),
	write('Se obtuvo el siguiente plan de acciones para ir a '),writeln(Destino),
	writeln('[Action|Acciones]'),
	retract(plan(_)),
	assert(plan(Acciones)),!.

% No existen metas, el agente explora en busca de metas
decide_action(Action):- 
	explorar(Action),
	writeln('SIN METAS EXPLORAR'),!.

%decide_action(Action) :-
%	Action = null_action,
%	writeln('YA EXPLORE TODO EL MAPA Y JUNTE TODOS LOS OBJETOS. MISION CUMPLIDA!!!!!!').

%decide_action(Action):-
%	at([agent, me], MyNode),
%	findall(Node, ady(MyNode, Node), PossibleDestNodes),
%	random_member(DestNode, PossibleDestNodes), % Selecciona aleatoriamente una posición destino.
 %       write("Nodo destino: "), writeln(DestNode),
%	Action = move(DestNode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Auxiliares %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% explorar(-Action)
% Action: Proxima accion a ejecutar por el agente

explorar(Action):-
        writeln('Explorar nodos no explorados'),
        % Se obtiene una lista de las posiciones conocidas por el agente que aun no han sido exploradas
	findall(Pos,(node(Pos,_,_), not(visitados(Pos))), NoExplorados),
	NoExplorados \= [],
        writeln('Nodos no explorados: '),
	writeln(NoExplorados),
        buscar_plan_desplazamiento(NoExplorados, [Action|Acciones], Destino),
	write('Explorando, se irá a:\n\t'),
        writeln(Destino),
	writeln(Acciones),
	%retract(plan(_)),
	% Se asigna el nuevo plan de acciones a seguir
	assert(plan(Acciones)).			

% buscar_metas(-Metas): Selecciona proxima meta

% El agente tiene bajo nivel de vida
buscar_metas(Metas):- 
        property([agent,me],life,AgLife),
        AgLife < 75,
	findall(Pos,at([inn,_],Pos),Metas).

% El agente busca oro
buscar_metas(Metas):-
        findall(Pos,at([gold,_],Pos),Metas).


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


start_ag:- AgName = agent,
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
                    AgClassName = template,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).

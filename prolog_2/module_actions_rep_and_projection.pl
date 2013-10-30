:- module(actions_rep_and_projection,
	  [
	    dynamic_state_rels/1,
	    project/3,
	    action_descr/1,
	    holds/2
	  ]).

:- [extras_for_agents].
%:- consult(extras_for_agents).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% action(+ActionName, -Preconditions, -AddList, -DelList).
%
% Representaci�n STRIPS de las acciones del agente.
% Preconditions, AddList y DelList son listas de relaciones.
%
% Adem�s de las acciones primitivas, debe especificarse m�nimamente la
% acci�n goto(PosDest).
%
% Aclaraci�n: en las precondiciones de las acciones puede hacerse uso de
% los predicados =/2, \=/2 y not/1, adem�s de las relaciones din�micas
% primitivas consideradas en el predicado dyn_state_rel/1 y las
% est�ticas especificadas en el predicado static_relation.
%


action(move(To), [at([agent, me], From), ady(From, To)], [at([agent, me], To)], [at([agent, me], From)]).


action(goto(PosDest), [at([agent, me], PosOrig)], [at([agent, me], PosDest)], [at([agent, me], PosOrig)]).


action(pickup(Obj), [at(Obj, PosX), at([agent, me], PosX)], [has([agent, me], Obj)], [at(Obj, PosX)]).


action(stay, [at([agent, me], Pos), at([inn, _Inn], Pos)], [], []). %'stay' should be 'stay_at_inn'.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% action_descr(-ActionDescr).
%
% Retorna la descripci�n completa de una acci�n. Concretamente,
% ActionDescr es una lista
%
%          [ActionName, Preconditions, AddList, DelList],
%
% tal que vale action(ActionName, Preconditions, AddList, DelList).
%

action_descr([ActionName, Preconditions, AddList, DelList]):-
	action(ActionName, Preconditions, AddList, DelList).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dynamic_state_rels(-InitState).
%
% Extrae la lista InitState de todas las relaciones din�micas primitivas
% validas de acuerdo a las creencias actuales del agente
%
% Para poder proyectar (o simular) la ejecuci�n de una secuencia de acciones a partir del estado actual,
% sin destruir la representaci�n interna que se tiene actualmente del mundo, resulta necesario
% replicar (parcialmente) dicha representaci�n para poder realizar la simulaci�n o proyecci�n sobre la
% r�plica, que se ir� modificando de acuerdo a los efectos de las acciones proyectadas.
% Concretamente, para realizar una proyecci�n, solo resulta necesario replicar las relaciones din�micas primitivas.
%
% Luego, InitState es una r�plica de las creencias din�micas del agente,
% a partir de la cual se pueda proyectar la ejecuci�n de una secuencia
% de acciones.

dynamic_state_rels(InitState):-
	findall(Rel, dyn_state_rel(Rel), InitState).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dyn_state_rel(-Relation)
%
% Retorna una instancia, v�lida actualmente, de una relaci�n din�mica
% primitiva. Es empleado por el predicado dynamic_state_rels/1 para
% obtener todas las instancias de relaciones din�micas primitivas que
% valen actualmente.
%
% <<<PUEDE AGREGAR OTRAS REGLAS SI DESEA CONSIDERAR CREENCIAS DIN�MICAS
% PRIMITIVAS ADICIONALES>>>


dyn_state_rel(at(ThingID, Pos)):-
	at(ThingID, Pos).

dyn_state_rel(has(ThingID, Thing1ID)):-
	has(ThingID, Thing1ID).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% static_relation(+Relation, +Arity)
%
% Establece que ciertas creencias del agente son constantes, es decir,
% que no cambian a lo largo del tiempo. Esto permite excluir estas
% relaciones de la representaci�n del estado actual del mundo que se
% lleva al realizar una proyecci�n, por ejemplo, en el contexto de
% una planificaci�n.
% Luego, cuando se desee saber si una cierta relaci�n est�tica se
% satisface en el estado actual alcanzado en una proyecci�n, se
% consultar� directamente a la base de conocimiento del agente.
%
% Tambien puede emplearse para permitir el uso de ciertos predicados
% predefinidos (como en =/2, \=2 o el is/2) o definidos por el usuario,
% (como is_a/2 y ady_at_cardinal/3). Estos �ltimos pueden pensarse como
% relaciones est�ticas predefinidas.


static_relation(node, 3).
static_relation(last_seen_at, 2).

static_relation(=, 2).    % Predicados predefinidos
static_relation(\=, 2).
static_relation(not, 1).

static_relation(ady, 2). % Predicados definido en extras_for_agents.pl
static_relation(is_a, 2).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% holds(+Relation, +State)
%
% Establece si Relation vale en el estado denotado por State
% Dado que State solo lista relaciones din�micas primitivas,
% si Relation es est�tica entonces se consultar� directamente a la base
% de conocimiento del agete (regla 2), como fue explicado anteriormente.


holds(Relation, State):-
	member(Relation, State),
	!.

holds(Relation, _State):-
	functor(Relation, Func, Arity),
	static_relation(Func, Arity),   % relaci�n derivada y constante
	call(Relation).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% holds_all(+Relations, +State)
%
% Versi�n de holds/2 para una lista de relaciones.


holds_all([], _State).

holds_all([Rel | Rels], State):-
	holds(Rel, State),
	holds_all(Rels, State).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% execute(+Action, +SBeforeAction, -SAfterAction)
%
% Se satisface cuando la acci�n Action puede ejecutarse con �xito a
% partir del estado actual (ie, se satisfacen sus precondiciones).
% SAfterAction representa el estado resultante de ejecutar la acci�n a
% partir del estado previo SBeforeAction.
%
% Tambi�n se satisface cuando la acci�n no se encuentra especificada,
% retornando como estado resultante al estado original (es decir, se
% asume que nada cambia al ejecutar la acci�n).

execute(ActName, SBeforeAction, SAfterAction):-
	action_descr([ActName, Pre, Add, Del]),
	!,
	holds_all(Pre, SBeforeAction),
	union(SBeforeAction, Add, Aux),
	subtract(Aux, Del, SAfterAction).

execute(_HLAction, SBeforeAction, SBeforeAction).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% project(+Plan, +Init, -Finish)
%
% Se satisface cuando el plan Plan puede ejecutarse con
% �xito a partir del estado actual (ie, se satisfacen las precondiciones
% de cada acci�n del plan en el momento que les toca ejecutarse). Adem�s
% retorna el estado (proyectado) resultado de ejecutar el plan.

project([], CurrState, CurrState).


project([Action | RestOfPlan], CurrState, StateAfterPlan):-
	execute(Action, CurrState, StateAfterAction),
	!,
	project(RestOfPlan, StateAfterAction, StateAfterPlan).

project(Action, _EActual, _Finish):-
	write('Plan no factible. Fallar�n las PRE de '), writeln(Action),nl,
	fail.




:-      writes_disabled,
	redefine_system_predicate(write(_)),
	assert(write(_)),
	redefine_system_predicate(writeln(_)),
	assert(writeln(_)),
	redefine_system_predicate(nl),
	assert(nl).



















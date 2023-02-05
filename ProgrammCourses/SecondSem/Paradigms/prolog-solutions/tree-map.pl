node(Key, Value, Left, Right, node(Key, Value, Left, Right)).

map_build([], 0) :- !.
map_build(ListMap, TreeMap) :- length(ListMap, Size), map_build_impl(ListMap, Size, TreeMap).

map_build_impl([(Key, Value) | _], 1, Parent) :- node(Key, Value, 0, 0, Parent), !.

map_build_impl([(Key1, Value1), (Key2, Value2) | _], 2, Parent) :-
	node(Key2, Value2, 0, 0, Son), 
	node(Key1, Value1, 0, Son, Parent), !.	
		

map_build_impl(ListMap, Size, Parent) :- 
	Left_Size is (Size // 2), Right_Size is ((Size - 1) // 2),
	find_medium(ListMap, Left_Size, (M_Key, M_Value), Right_List),
	map_build_impl(ListMap, Left_Size, Left_Son), 
	map_build_impl(Right_List, Right_Size, Right_Son),
	node(M_Key, M_Value, Left_Son, Right_Son, Parent).

find_medium([H | T], 0, R, R_List) :- R = H, R_List = T, !. 
find_medium([H | T], N, R, R_List) :- N1 is N - 1, find_medium(T, N1, R, R_List), !.

map_get(0, _, _) :- !, fail.
map_get(node(Key, ValueN, _, _), Key, Value) :- Value = ValueN, !.
map_get(node(KeyN, _, _, Right), Key, Value) :- Key > KeyN, \+ (Right = 0), map_get(Right, Key, Value), !.
map_get(node(KeyN, _, Left, _), Key, Value) :- KeyN > Key, \+ (Left = 0), map_get(Left, Key, Value), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%map_put(0, Key, Value, Result) :- map_build([(Key, Value)], Result).

map_minKey(node(KeyN, _, 0, _), Key) :- Key = KeyN, !.
map_minKey(node(KeyN, _, Left, _), Key) :- map_minKey(Left, Key), !.

map_maxKey(node(KeyN, _, _, 0), Key) :- Key = KeyN, !.
map_maxKey(node(KeyN, _, _, Right), Key) :- map_maxKey(Right, Key), !.
  



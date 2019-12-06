(* TODO: Check possible need of epsilon *)
let eps = 0.000000001
let fabs (x : float) = if x < 0. then -. x else x

(* Punkt na płaszczyźnie *)
type point = float * float
type kartka = point -> int

(* Pierwsza albo druga składowa punktu *)
let first  (x, _) = x
and second (_, y) = y

(* Dodowanaie, odejmowanie punktów oraz mnożenie punktu przez skalar *)
let add_points (ax, ay) (bx, by) = (ax +. bx, ay +. by)
and sub_points (ax, ay) (bx, by) = (ax -. bx, ay -. by)
and mul_point (s : float) (x, y) = (s *. x, s *. y)

(* Iloczyn skalarny wektorów [a] i [b] *)
let dot (a : point) (b : point) =
	(first a) *. (first b) +. (second a) *. (second b)

(* Odbija punkt [x] względem prostej przechodzącej przez [l_orig] oraz [l_end] *)
let reflect (v : point) (l_orig, l_end) =
	assert (l_orig <> l_end);
	let l = sub_points l_end l_orig
	and p = sub_points v l_orig in
	let factor = 2. *. (dot p l) /. (dot l l)
	in add_points l_orig (sub_points (mul_point factor l) p)

(* Sprawdza, po której stronie prostej [l] znajduje się wektor [v] *)
(* Zwraca  1, jeśli [v] jest po lewej [l] 						   *)
(* Zwraca  0, jeśli [v] leży na [l] 							   *)
(* Zwraca -1, jeśli [v] leży na prawo [l]						   *)
let side (v : point) (l_orig, l_end) =
	assert (l_orig <> l_end);
	let p = sub_points v l_orig
	and l = sub_points l_end l_orig in
	let cross = (first p) *. (second l) -. (second p) *. (first l)
	in
		if fabs cross < eps then 0
		else if cross < 0. then 1
		else -1

let prostokat (ax, ay) (bx, by) : kartka = function (px, py) ->
		if px >= ax -. eps && px <= bx +. eps && py >= ay -. eps && py <= by +. eps then 1 else 0	

let kolko (s : point) (r : float) =
	let mag (k : point) = sqrt (dot k k) in
	function (p : point) -> if (mag (sub_points p s) <= r +. eps) then 1 else 0

let zloz (a : point) (b : point) (k : kartka) : kartka =
	assert (a <> b);
	let fold = (a, b) in
	function (p : point) ->
		let sid = side p fold in
		if sid > 0 then k p + k (reflect p fold)
		else if sid = 0 then k p
		else 0 

let skladaj (lst : (point * point) list) (k : kartka) =
	List.fold_left (fun x -> function (a, b) -> zloz a b x) k lst
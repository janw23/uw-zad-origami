(* TODO: Check possible need of epsilon *)

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
		if cross < 0. then 1
		else if cross > 0. then -1
		else 0

(* Zwraca funkcję sprawdzającą, czy punkt [p] jest wewnątrz prostokąta [ra, rb] *)
let inside_rect (ra : point) (rb : point) =
	function (p : point) -> begin
		(first p) >= (first ra) && (first p) <= (first rb) &&
		(second p) >= (second ra) && (second p) <= (second rb)
	end

(* Zwraca funkcję sprawdzającą, czy punkt [p] jest wewnątrz okręgu [s, r] *)
let inside_circle (s : point) (r : float) =
	let sqrmag (k : point) = dot k k in
	function (p : point) -> (sqrmag (sub_points p s) <= r *. r)

(* Zmienna pomocnicza do debugowania     *)
(* Autor: Piotr Prabucki & Jan Wawszczak *)
let make_shit = 69;;

(* Krotka symbolizująca brak jakiegokolwiek zgięcia kartki *)
let zero_fold = ((0, 0), (0, 0))

(* Tworzy kartkę, która do sprawdzania przynależności punktu do siebie samej 			*)
(* używa funkcji [inside_shape] oraz zakłada, że wykonane zostały złożenia z listy [folds] *)
let make_sheet (inside_shape : point -> bool) (fold : (point * point) * (fold * fold)) : (kartka -> int) =
	function (p : point) -> begin
		(* Jeśli nie wykonane zostało żadne zgięcie *)
		if fold = zero_fold then (if inside_shape p then 1 else 0)
		else begin
			
		end
	end

let prostokat (a : point) (b : point) =
	make_sheet (inside_rect a b) zero_fold

let kolko (p : point) (r : float) =
	make_sheet (inside_circle p r) zero_fold

let zloz (a : point) (b : point) (k : kartka) = k

let skladaj (lst : (point * point) list) (k : kartka) = k
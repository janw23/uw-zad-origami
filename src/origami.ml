let eps = 0.000000001
let fabs (x : float) = if x < 0. then -. x else x

(* Punkt na płaszczyźnie *)
type point = float * float

(* Zwraca, ile razy szpilka wbita w kartkę w danym punkcie przebije ją *)
type kartka = point -> int

(* Pierwsza albo druga składowa punktu *)
let first  (x, _) = x
and second (_, y) = y

(* Dodowanaie, odejmowanie punktów oraz mnożenie punktu przez skalar *)
let add_points (ax, ay) (bx, by) = (ax +. bx, ay +. by)
and sub_points (ax, ay) (bx, by) = (ax -. bx, ay -. by)
and mul_point (s : float) (x, y) = (s *. x, s *. y)

(* Iloczyn skalarny i wektorowy wektorów [a] i [b] *)
let dot   (a, b) (c, d) = a *. c +. b *. d
and cross (a, b) (c, d) = a *. d -. b *. c

(* Odbija punkt [x] względem prostej przechodzącej przez [l_orig] oraz [l_end] *)
let reflect (v : point) (l_orig, l_end) =
	let l = sub_points l_end l_orig
	and p = sub_points v l_orig in
	let factor = 2. *. (dot p l) /. (dot l l)
	in add_points l_orig (sub_points (mul_point factor l) p)

(* Sprawdza, po której stronie prostej [l] znajduje się wektor [v] *)
(* Zwraca  1, jeśli [v] jest po lewej [l] 						   *)
(* Zwraca  0, jeśli [v] leży na [l] 							   *)
(* Zwraca -1, jeśli [v] leży na prawo [l]						   *)
let side (v : point) (l_orig, l_end) =
	let p = sub_points v l_orig
	and l = sub_points l_end l_orig in
	let cros = cross p l in
		if fabs cros < eps then 0
		else if cros < 0. then 1
		else -1

(* Tworzy prostokątną niezgiętą kartkę *)
let prostokat (ax, ay) (bx, by) : kartka = function (px, py) ->
	if px >= ax -. eps && px <= bx +. eps &&
	   py >= ay -. eps && py <= by +. eps
	   then 1 else 0	

(* Tworzy okrągłą niezgiętą kartkę *)
let kolko (s : point) (r : float) =
	let sqrmag (kx, ky) = (kx *. kx +. ky *. ky) in
	function (p : point) ->
		if sqrmag (sub_points p s) <= (r +. eps) *. (r +. eps)
		then 1 else 0

(* Składa kartkę [k] wzdłuż wektora z [a] do [b] *)
let zloz (a : point) (b : point) (k : kartka) : kartka =
	let fold = (a, b) in
	function (p : point) ->
		let sid = side p fold in
		if sid > 0 then k p + k (reflect p fold)
		else if sid = 0 then k p
		else 0 

(* Wykonuje serię złożeń [lst] na kartce [k] *)
let skladaj (lst : (point * point) list) (k : kartka) =
	List.fold_left (fun x -> function (a, b) -> zloz a b x) k lst

(* TESTY *)
   
let a = prostokat (0., 0.) (10., 10.);;
let a = zloz (5., 0.) (5., 377.) a;;

assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let a = zloz (5., 0.) (5., 1.) a;;

assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;
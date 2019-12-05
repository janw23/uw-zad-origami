type point = float * float

(* Pierwsza albo druga składowa punktu *)
let first  (x, _) = x
and second (_, y) = y

(* Dodowanaie, odejmowanie punktów oraz mnożenie punktu przez skalar *)
let add_points (ax, ay) (bx, by) = (ax +. bx, ay +. by)
and sub_points (ax, ay) (bx, by) = (ax -. bx, ay -. by)
and mul_point (s : float) (x, y) = (s *. x, s *. y)

type kartka = point -> int

(* Iloczyn skalarny wektorów [a] i [b] *)
let dot (a : point) (b : point) =
	(first a) *. (first b) +. (second a) *. (second b)

(* Odbija punkt [x] względem prostej przechodzącej przez [l_orig] oraz [-_end] *)
let reflect (v : point) (l_orig, l_end) =
	assert (l_orig <> l_end);
	let l = sub_points l_end l_orig
	and p = sub_points v l_orig in
	let factor = 2. *. (dot p l) /. (dot l l)
	in add_points l_orig (sub_points (mul_point factor l) p)

let prostokat (a : point) (b : point) =
	function k -> 0

let kolko (p : point) (r : float) =
	function k -> 0

let zloz (a : point) (b : point) (k : kartka) = k

let skladaj (lst : (point * point) list) (k : kartka) = k
type point = float * float

(* Pierwsza albo druga składowa punktu *)
let first  (x, _) = x
and second (_, y) = y

type kartka = point -> int

(* Iloczyn skalarny wektorów [a] i [b] *)
let dot (a : point) (b : point) =
	(first a) *. (first b) +. (second a) *. (second b) 

(* Odbija punkt [x] względem prostej [l] (rozumianą jako wektor z (0, 0) do l) *)
let reflect (p : point) (l : point) =
	let factor = 2. *. (dot p l) /. (dot l l)
	in (factor *. (first l) -. (first p), factor *. (second l) -. (second p)) 

let prostokat (a : point) (b : point) =
	function k -> 0

let kolko (p : point) (r : float) =
	function k -> 0

let zloz (a : point) (b : point) (k : kartka) = k

let skladaj (lst : (point * point) list) (k : kartka) = k
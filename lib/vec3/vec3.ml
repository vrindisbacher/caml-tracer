
(* unified triples *)
type triple = { x: float; y: float; z: float}

(* polymorphic methods on triples *)
let stream_triple t acc = acc ^ string_of_float t.x ^ " " ^ string_of_float t.y ^ " " ^ string_of_float t.z ^ "\n"

(* for adding, subtracting and multiplying triples *)
let inv_triple t  = {x = -. t.x; y = -. t.y; z = -. t.z}

let manip_triple_inline t1 t2 f = { x = f t1.x t2.x; y = f t1.y t2.y; z = f t1.z t2.z}

let mul_triple_by_const t c = {x = c *. t.x; y = c *. t.y; z = c *. t.z}

(* notation *)
let (--.) v = inv_triple v 

let (!+.) el1 el2 = manip_triple_inline el1 el2 (+.)
let (!*.) el1 el2 = manip_triple_inline el1 el2 (fun a b -> a *. b)
let (!-.) el1 el2 = manip_triple_inline el1 el2 (-.)
let ($*.) el1 c = mul_triple_by_const el1 c 

(* division *)
let div_triple_by_const t c = t $*. (1. /. c) 

(* notation *)
let ($/.) el1 c = div_triple_by_const el1 c

let dot_product t1 t2  = (t1.x *. t2.x) +. (t1.y *. t2.y) +. (t1.z *. t2.z)

(* notation *)
let (~*.) el1 el2 = dot_product el1 el2

let cross_product t1 t2 = {x = (t1.y -. t2.z) *. (t1.z -. t2.y); y = (t1.z -. t2.x) *. (t1.x -. t2.z); z = (t1.x -. t2.y) *. (t1.y -. t2.x);}

(* notation *)
let (%*.) el1 el2 = cross_product el1 el2

let triple_length_sq t = t.x**2. +. t.y**2. +. t.z**2.

let triple_length t = sqrt (triple_length_sq t)

let unit_vector t = t $/. (triple_length t) 


(* useful notation for int cast *)
let (^*) (a : float) (b : float) = string_of_int (int_of_float (a *. b))

let write_color pixel_color = (255.999 ^* pixel_color.x) ^ " " ^ (255.999 ^* pixel_color.y) ^ " " ^ (255.99 ^* pixel_color.z) ^ "\n"
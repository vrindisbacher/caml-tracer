
(* unified triples *)
type 'a triple = { x: 'a; y: 'a; z: 'a}

(* polymorphic methods on triples *)
let stream_triple (t : 'a triple) (acc : string) (f : 'a -> string) : string = acc ^ f t.x ^ " " ^ f t.y ^ " " ^ f t.z ^ "\n"

(* for adding, subtracting and multiplying triples *)
let manip_triple_inline (t1 : 'a triple) (t2 : 'a triple) (f : 'a -> 'a -> 'a) : 'a triple = { x = f t1.x t2.x; y = f t1.y t2.y; z = f t1.z t2.z}

let mul_triple_by_const (t : 'a triple) (c : 'a) (f: 'a -> 'a -> 'a) : 'a triple = {x = f c t.x; y = f c t.y; z = f c t.z}

let div_triple_by_const (t : 'a triple) (c : 'a) (f: 'a -> 'a -> 'a) : 'a triple = mul_triple_by_const t c (fun a b -> f a b)

let dot_product (t1 : 'a triple) (t2 : 'a triple) (f1 : 'a -> 'a -> 'a) (f2 : 'a -> 'a -> 'a) : 'a  = f2 (f1 t1.x t2.x) (f2 (f1 t1.y t2.y) (f1 t1.z t2.z)) 

let cross_product (t1 : 'a triple) (t2 : 'a triple) f1 f2 = {x = f2 (f1 t1.y t2.z) (f1 t1.z t2.y); y = f2 (f1 t1.z t2.x) (f1 t1.x t2.z); z =f2 (f1 t1.x t2.y) (f1 t1.y t2.x);}

let triple_length (t : 'a triple) (c : 'a) f = div_triple_by_const t c f 


(* custom ops on triples *)

(* add mul and sub triples *)
let (!+) el1 el2 = manip_triple_inline el1 el2 (+)
let (!*) el1 el2 = manip_triple_inline el1 el2 (fun a b -> a * b)
let (!-) el1 el2 = manip_triple_inline el1 el2 (-)

let (!+.) el1 el2 = manip_triple_inline el1 el2 (+.)
let (!*.) el1 el2 = manip_triple_inline el1 el2 (fun a b -> a *. b)
let (!-.) el1 el2 = manip_triple_inline el1 el2 (-.)

(* mul by a constant *)
let ($*) c el1 = mul_triple_by_const el1 c (fun a b -> a * b)
let ($*$) el1 c = mul_triple_by_const el1 c (fun a b -> a * b)
let ($*.) c el1 = mul_triple_by_const el1 c (fun a b -> a *. b)
let ($*.$) el1 c = mul_triple_by_const el1 c (fun a b -> a *. b)

(* div by a constant *)
let ($/) el1 c = div_triple_by_const el1 (1 / c) (/)
let ($/.) el1 c = div_triple_by_const el1 (1. /. c) (/.)

(* dot product *)
let (~*) el1 el2 = dot_product el1 el2 (fun a b -> a * b) (+)
let (~*.) el1 el2 = dot_product el1 el2 (fun a b -> a *. b) (+.)

(* cross product *)
let (%*) el1 el2 = cross_product el1 el2 (-) (fun a b -> a * b)
let (%*.) el1 el2 = cross_product el1 el2 (-.) (fun a b -> a *. b)

(* useful notation for int cast *)
let (^*) (a : float) (b : float) = string_of_int (int_of_float (a *. b))

let write_color (pixel_color : float triple) = (255.999 ^* pixel_color.x) ^ " " ^ (255.999 ^* pixel_color.y) ^ " " ^ (255.99 ^* pixel_color.z) ^ "\n"
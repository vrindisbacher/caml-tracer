open Vec3

type ray = {origin: triple; direction: triple}

let at (r : ray) (t : float) = r.origin |+. (r.direction $*. t)

let hit_sphere (center: triple) (radius : float) (r : ray) = 
    let oc = r.origin |-. center in 
    let a = triple_length_sq r.direction in 
    let half_b = oc %~*. r.direction in 
    let c = triple_length_sq oc -. (radius *. radius) in 
    let discriminant = (half_b *. half_b) -. (a *. c) in  
    if discriminant < 0. then -1. else ((-. half_b) -. sqrt discriminant) /. a

let ray_color (r : ray) = 
    let t = hit_sphere {x=0.; y=0.; z=(-. 1.)} 0.5 r in 
    if t > 0. then
        let n = unit_vector r.direction in ({x = n.x +. 1.; y = n.y +. 1.;z = n.z +. 1.} $*. 0.5)
    else 
        let unit_direction = unit_vector r.direction in 
        let t = 0.5 *. (unit_direction.y +. 1.)
        in (({x=1.;y=1.;z=1.} $*. (1. -. t)) |+. ({x=0.5;y=0.7;z=1.} $*. t))
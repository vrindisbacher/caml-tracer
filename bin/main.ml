open Core
open Vec3 
open Ray 


let () = 
    let img_width = 400 in 
    let aspect_ratio = 16. /. 9. in 
    let img_height = int_of_float (float_of_int img_width /. aspect_ratio) in 
    let viewport_height = 2. in 
    let viewport_width = aspect_ratio *. viewport_height in 
    let focal_length = 1. in 
    let origin = {x=0.; y=0.; z=0.} in 
    let horizontal = {x=viewport_width; y=0.; z=0.} in 
    let vertical = {x=0.; y=viewport_height; z=0.} in 
    let lower_left_corner = origin |-. (horizontal $/. 2.) |-. (vertical $/. 2.) |-. {x=0.; y=0.; z=focal_length} in 
    let build_row j img_string = 
        let rec aux i acc = 
            if i < img_width then 
            let u = float_of_int i /. float_of_int (img_width - 1) in 
            let v = float_of_int j /. float_of_int (img_height - 1) in 
            let ray = {origin=origin; direction=lower_left_corner |+. (horizontal $*. u) |+. (vertical $*. v) |-. origin} in 
            let pixel_color = ray_color ray in 
            aux (i + 1) (acc ^ write_color pixel_color) 
            else acc 
        in aux 0 img_string
    in 
    let build_img_string = 
        let rec aux i acc = 
            if i >= 0 then 
            (* debug statement *) 
            let _ = print_string ("lines remaining: " ^ string_of_int i ^ "\n") in 
            aux (i - 1) (build_row i acc) 
            else acc 
        in aux (img_height - 1) ("P3\n" ^ (string_of_int img_width) ^ " " ^ (string_of_int img_height) ^ "\n255\n") 
    in 
    let img_string = build_img_string 
    in Out_channel.write_all "./images/rayimg.ppm" ~data:img_string 
    


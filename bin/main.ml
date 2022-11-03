open Core

let build_row height width j img_string = 
    let rec aux i j acc = 
        if i < width then 
        aux (i + 1) j (acc ^ Vec3.write_color {x = float_of_int i /. float_of_int (width - 1); y = float_of_int j /. float_of_int (height - 1); z = 0.25}) 
        else acc 
    in aux 0 j img_string

let build_img_string height width = 
    let rec aux i acc = 
        if i >= 0 then 
        (* debug statement *) 
        let _ = print_string ("lines remaining: " ^ string_of_int i ^ "\n") in 
        aux (i - 1) (build_row height width i acc) 
        else acc 
    in aux (height - 1) ("P3\n" ^ (string_of_int width) ^ " " ^ (string_of_int height) ^ "\n255\n")

let () = 
    let img_height = 256 in 
    let img_width = 256 in 
    let img_string = build_img_string img_height img_width
    in Out_channel.write_all "./images/rayimg.ppm" ~data:img_string 
    


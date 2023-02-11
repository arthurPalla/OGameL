let w = 1500
let h = 1000
(*
type player = {
  mutable x: int;
  mutable y: int;
}*)
type batiments = 
{
  width:int;
  height: int;
  x:int;
  y:int;
  texture :Raylib.Texture.t
}
let texture_from_image image xsize ysize = 
  Raylib.(image_resize (addr image) xsize ysize);
  Raylib.load_texture_from_image(image)


let texture_from_int n = 

  match n with 
  |0 -> texture_from_image (Raylib.load_image("./housesetx1/house1x1.gif")) 70 50
  |1 -> texture_from_image (Raylib.load_image("./housesetx1/house2x1.gif")) 70 70
  |2 -> texture_from_image (Raylib.load_image("./housesetx1/house3x1.gif")) 100 100
  |_ -> failwith "error while loading house texture"

let rec generate_coo l = 
  let x,y = Random.int (w - 100), Random.int (h-100) in 
  let rec aux list = 
    match list with
    |t::q -> if x < t.x + t.width && t.x > t.x - t.width && y<t.y + t.height && y>t.y -t.width then generate_coo l else aux q
    |[] -> (x,y) in 
    aux l

let  generate_map n = 
  let rec aux number acc = 
    if number = 0 then acc
    else match Random.int 3 with 
    |0 -> let x1,y1 = generate_coo acc  in aux (number-1) ({width = 70; height = 50; x = x1; y =y1; texture = texture_from_int 0}::acc)
    |1 -> let x1,y1 = generate_coo acc  in aux (number-1) ({width = 70; height = 70; x = x1; y =y1; texture = texture_from_int 1}::acc)
    |2 -> let x1,y1 = generate_coo acc in aux (number-1) ({width = 100; height = 100; x = x1; y =y1; texture = texture_from_int 2}::acc)
    |_ -> failwith("C'est super bizarre si ça bug ici") 
  in aux n []

let rec draw_map l = 
  match l with 
  |t::q -> Raylib.draw_texture t.texture t.x t.y Raylib.Color.white; draw_map q
  |[] -> ()

let rec loop map= 
  if Raylib.window_should_close () then Raylib.close_window ()
  else 
    let open Raylib in 
    begin_drawing ();
    clear_background Color.skyblue;
    draw_map map;
    end_drawing ();
    loop map

let setup ()= 
      Random.self_init ();
      Raylib.init_window w h "OGamel";
      Raylib.set_target_fps 60;
      generate_map 10

let () = 
    loop (setup ());

           
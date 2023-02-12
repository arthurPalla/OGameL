let w = 1000
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
type map = 
{
  batiment: batiments list;
  floor: Raylib.Texture.t;
  roads: batiments list;
}
let texture_from_image image xsize ysize =
  Raylib.(image_resize (addr image) xsize ysize);
  Raylib.load_texture_from_image(image)


let texture_crop_and_resize s cox coy xsize ysize w h = 
  let open Raylib in 
  let img = load_image(s) in
  let rect = Rectangle.create cox coy xsize ysize in 
  image_crop (addr img) rect;
  texture_from_image img w h


let texture_from_int n =
  match n with
  |0 -> texture_from_image (Raylib.load_image("./housesetx1/house1x1.gif")) 100 90
  |1 -> texture_from_image (Raylib.load_image("./housesetx1/house2x1.gif")) 110 110
  |2 -> texture_from_image (Raylib.load_image("./housesetx1/house3x1.gif")) 120 120
  |_ -> failwith "error while loading house texture"

let rec generate_coo l =
  let x,y = Random.int (w - 300) + 100, Random.int (h-300) + 100 in
  let rec aux list =
    match list with
    |t::q -> if x < t.x + t.width + 70 && t.x > t.x - t.width - 70 && y<t.y + t.height + 70 && y>t.y -t.width-70 then generate_coo l else aux q
    |[] -> (x,y) in
    aux l

let generate_map n =
  let rec aux number acc =
    if number = 0 then acc
    else match Random.int 3 with
    |0 -> let x1,y1 = generate_coo acc  in aux (number-1) ({width = 70; height = 50; x = x1; y =y1; texture = texture_from_int 0}::acc)
    |1 -> let x1,y1 = generate_coo acc  in aux (number-1) ({width = 70; height = 70; x = x1; y =y1; texture = texture_from_int 1}::acc)
    |2 -> let x1,y1 = generate_coo acc in aux (number-1) ({width = 100; height = 100; x = x1; y =y1; texture = texture_from_int 2}::acc)
    |_ -> failwith("C'est super bizarre si ça bug ici")

  in aux n []

let generate_floor ()= 
  texture_crop_and_resize "./images/tileset.png" 440. 120. 50. 50. 50 50

let generate_road ()= 
  let text_road = texture_crop_and_resize "./images/tileset.png" 0. 60. 28.8 32. 50 50 in 
  let rec aux x1 y1 acc n =
  if x1 < 0 || y1<0 || x1 > w || y1 > h then acc
  else if n <= 0 then acc
  else 
    let bat = {width=50; height=50; x=x1 ;y=y1 ; texture = text_road} in 
    match Random.int 4 with
    |1 -> aux (x1+50) (y1+42) (bat::acc) (n-1) @ aux (x1) (y1+42) (bat::acc) (n-1) @ aux (x1+50) (y1) (bat::acc) (n-1)
    |2 -> aux (x1+42) (y1) (bat::acc) (n-1)
    |3 -> aux (x1) (y1+42) (bat::acc) (n-1)
    |_ -> acc
in 
(aux ((Random.int (w/50))*50) ((Random.int (h/50))*50) [] 20 ) @ (aux ((Random.int (w/50))*50) ((Random.int (h/50))*50) [] 20 ) 

let rec draw_batiment l =
  match l with
  |t::q -> Raylib.draw_texture t.texture t.x t.y Raylib.Color.white; draw_batiment q
  |[] -> ()

let draw_floor floor = 
  for i = 0 to w/50 +1 do 
    for j = 0 to h/50 + 1 do 
      Raylib.draw_texture floor (i*50)  (j*50) Raylib.Color.white
    done;
  done;
  () 

let setup ()=
  Random.self_init ();
  Raylib.init_window w h "OGamel";
  Raylib.set_target_fps 60;
  {batiment = generate_map ((Random.int 5) + 1); floor = generate_floor (); roads = generate_road ()}
 

let rec loop map=
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    draw_floor map.floor;
    draw_batiment map.roads ;
    draw_batiment map.batiment;
    end_drawing ();
    loop map

let () =
    loop (setup ());

let w = 1000
let h = 1000

type 'a cyclic_list = {
  elements: 'a array;
  mutable i: int;
  length:int;
}

type player = { 
  mutable health: int;
  mutable feed: int;
  mutable x: int;
  mutable y:int;
  texture: (Raylib.Texture.t cyclic_list) array;  
  mutable direction: int;
} 

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
  mutable batiment: batiments list;
  floor: Raylib.Texture.t;
  mutable roads: batiments list;
  mutable generated: (int*int) list
}
let rec is_inside_list x l =
  match l with 
  |t::q -> if  t=x then true else is_inside_list x q
  |[] -> false

let texture_from_image image xsize ysize =
  Raylib.(image_resize (addr image) xsize ysize);
  Raylib.load_texture_from_image(image)

let cyclic_next c_list =
  if(c_list.i >= c_list.length -2) then 
    c_list.i <- 0
  else c_list.i <- c_list.i + 1;
  ()

let cyclic_top c_list = 
  c_list.elements.(c_list.i)  



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

let rec generate_coo l x1 y1 =
  let x,y = Random.int (4*w) -2*w + 4*x1*w, Random.int (4*h) - 2*h + 4*y1*w in
  let rec aux list =
    match list with
    |t::q -> if x < t.x + t.width + 70 && t.x > t.x - t.width - 70 && y<t.y + t.height + 70 && y>t.y -t.width-70 then generate_coo l x1 y1 else aux q
    |[] -> (x,y) in
    aux l

let generate_map n x y=
  let rec aux number acc =
    if number = 0 then acc
    else match Random.int 3 with
    |0 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 70; height = 50; x = x1; y =y1; texture = texture_from_int 0}::acc)
    |1 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 70; height = 70; x = x1; y =y1; texture = texture_from_int 1}::acc)
    |2 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 100; height = 100; x = x1; y =y1; texture = texture_from_int 2}::acc)
    |_ -> failwith("C'est super bizarre si ça bug ici")

  in aux n []

let player_init () = 
  let test = Array.init 4 (fun i ->
    let a2 = Array.init 9 (fun j -> (texture_crop_and_resize "./images/player_sheet.png" (float_of_int(j*32)) (float_of_int(291 + i*73)) 32. 73. 50 100))
  in
  {elements = a2; i=0; length = 10}) in 
  {health =20 ; feed = 20; x= 500; y = 500; texture = test; direction = 0}


let generate_floor ()= 
  texture_crop_and_resize "./images/tileset.png" 440. 120. 50. 50. 50 50


let generate_road xchunk ychunk = 
  let text_road = texture_crop_and_resize "./images/tileset.png" 0. 64. 28.8 32. 50 50 in 
  let rec aux x1 y1 acc n =
  if n <= 0 then acc
  else 
    let bat = {width=50; height=50; x=x1 ;y=y1 ; texture = text_road} in 
    match Random.int 4 with
    |1 -> aux (x1+50) (y1+50) (bat::acc) (n-1) @ aux (x1) (y1+50) (bat::acc) (n-1) @ aux (x1+50) (y1) (bat::acc) (n-1)
    |2 -> aux (x1+50) (y1) (bat::acc) (n-1)
    |3 -> aux (x1) (y1+50) (bat::acc) (n-1)
    |_ -> acc
in 
(aux (Random.int (4*w) -2*w + 4*xchunk*w) (Random.int (4*h) - 2*h + 4*ychunk*w) [] 15 ) @ (aux (Random.int (4*w) -2*w + 4*xchunk*w) (Random.int (4*h) - 2*h + 4*ychunk*w) [] 15 ) 


let rec draw_batiment l (joueur:player)=
  match l with
  |t::q ->if t.x <=  joueur.x + 600 && t.x >= joueur.x - 600 && t.y <= joueur.y + 600 && t.y >= joueur.y - 600 then   Raylib.draw_texture t.texture (t.x - joueur.x + 500) (t.y - joueur.y + 500) Raylib.Color.white; draw_batiment q joueur
  |[] -> ()



let update_map (map:map) (joueur:player) = 
  let x1 = joueur.x / 2000 in 
  let y1 = joueur.y / 2000 in 
  for i = -1 to 1 do 
    for j = -1 to 1 do 
       if (i != 0 || j!= 0) && not (is_inside_list ((x1 + i),(y1+j) ) map.generated) then(
        Raylib.draw_text (String.cat (string_of_int (x1+i) )  (string_of_int (y1+j)) ) (300+50 * i) (300+50*j) 30 Raylib.Color.red;
        map.generated <- ((x1 + i),(y1+j))::map.generated;
        map.roads <- generate_road (x1 + i) (y1 + j) @ map.roads;
        map.batiment <- (generate_map 30 (x1 + i) (y1 + j)) @ map.batiment;)
  done;
done; 
()


let draw_floor floor (joueur:player)= 
  let xt = (joueur.x mod 50) in 
  let yt = (joueur.y mod 50) in 
  for i = -1 to (w+1)/50 +1 do 
    for j = -1 to (h+1)/50 + 1 do 
      Raylib.draw_texture floor (i*50 - xt)  (j*50 - yt) Raylib.Color.white
    done;
  done;
  () 

  
let update_player (joueur:player) = 
  let open Raylib in
  if(is_key_down Key.W) then ((joueur.y <- joueur.y -5; joueur.direction <- 0);   cyclic_next joueur.texture.(0))
  else if(is_key_down Key.S) then ((joueur.y <- joueur.y +5; joueur.direction <- 1);   cyclic_next joueur.texture.(1))
  else if(is_key_down Key.D) then ((joueur.x <- joueur.x +5; joueur.direction <- 3);  cyclic_next joueur.texture.(3))
  else if(is_key_down Key.A) then ((joueur.x <- joueur.x -5; joueur.direction <- 2);   cyclic_next joueur.texture.(2));
  ()


let draw_player (joueur:player) = 
  Raylib.draw_texture (cyclic_top ((joueur.texture).(joueur.direction))) (500) (500) Raylib.Color.white

let setup ()=
  Random.self_init ();
  Raylib.init_window w h "OGamel";
  Raylib.set_target_fps 60;
  {batiment = generate_map (60) 0 0; floor = generate_floor (); roads = generate_road 0 0; generated = [(0,0)]}
 

let rec loop map joueur=
  if Raylib.window_should_close () then Raylib.close_window ()
  
  else
    update_player joueur;
    update_map map joueur;
    let open Raylib in
    begin_drawing ();
    draw_floor map.floor joueur;
    draw_batiment (map.roads) joueur;
    draw_batiment (map.batiment) joueur;
    draw_player joueur;
    draw_text (string_of_int joueur.x) 50 50 30 Color.red;
    draw_text (string_of_int joueur.y) 140 50 30 Color.red;

    end_drawing ();
    loop map joueur

let () =
    let map = setup () in 
    let joueur = player_init () in 
    loop map joueur;

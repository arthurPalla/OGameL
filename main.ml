open Generation
open Graphic
open Types

let rec is_inside_list x l =
  match l with 
  |t::q -> if  t=x then true else is_inside_list x q
  |[] -> false

let player_init () = 
  let test = Array.init 4 (fun i ->
    let a2 = Array.init 9 (fun j -> (texture_crop_and_resize "./images/player_sheet.png" (float_of_int(j*32)) (float_of_int(291 + i*73)) 32. 73. 50 100))
  in
  {elements = a2; i=0; length = 10}) in 
  {health =20 ; feed = 20; x= 500; y = 500; texture = test; direction = 0}


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
  
let update_player (joueur:player) = 
  let open Raylib in
  if(is_key_down Key.W) then ((joueur.y <- joueur.y -5; joueur.direction <- 0);   cyclic_next joueur.texture.(0))
  else if(is_key_down Key.S) then ((joueur.y <- joueur.y +5; joueur.direction <- 1);   cyclic_next joueur.texture.(1))
  else if(is_key_down Key.D) then ((joueur.x <- joueur.x +5; joueur.direction <- 3);  cyclic_next joueur.texture.(3))
  else if(is_key_down Key.A) then ((joueur.x <- joueur.x -5; joueur.direction <- 2);   cyclic_next joueur.texture.(2));
  ()

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

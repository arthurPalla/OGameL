open Generation
open Graphic
open Types
open Player

let rec is_inside_list x l =
  match l with 
  |t::q -> if  t=x then true else is_inside_list x q
  |[] -> false


let update_map (map:map) (joueur:player) = 
  let x1 = joueur.x / 10000 in 
  let y1 = joueur.y / 10000 in 
  for i = -1 to 1 do 
    for j = -1 to 1 do 
       if (i != 0 || j!= 0) && not (is_inside_list ((x1 + i),(y1+j) ) map.generated) then(
        Raylib.draw_text (String.cat (string_of_int (x1+i) )  (string_of_int (y1+j)) ) (300+50 * i) (300+50*j) 30 Raylib.Color.red;
        map.generated <- ((x1 + i),(y1+j))::map.generated;
        map.roads <- generate_road (x1 + i) (y1 + j) @ map.roads;
        map.batiment <- (generate_map 40 1000 (x1 + i) (y1 + j)) @ map.batiment;)
  done;
done; 
()

let setup ()=
  Random.self_init ();
  Raylib.init_window w h "OGamel";
  Raylib.set_target_fps 60;
  {batiment = generate_map 40 1000 0 0; floor = generate_floor (); roads = generate_road 0 0; generated = [(0,0)]}
 

let rec loop map joueur=
  if Raylib.window_should_close () then Raylib.close_window ()
  
  else
    update_player joueur map;
    update_map map joueur;
    let open Raylib in
    begin_drawing ();
    draw_floor map.floor joueur;
    draw_road (map.roads) joueur;
    draw_batiment_first_plan (map.batiment) joueur;
    draw_player joueur;
    draw_fps 300 300;
    draw_batiment_second_plan (map.batiment) joueur;
    draw_text (string_of_int joueur.x) 50 50 30 Color.red;
    draw_text (string_of_int joueur.y) 140 50 30 Color.red;
    if joueur.is_inventory_open then draw_inventory joueur;

    end_drawing ();
    loop map joueur

let () =
    let map = setup () in 
    let joueur = player_init () in 
    joueur.inventory.(17) <- Some (8, {id = 8; name = "Wood log"; image = (texture_from_image_name "./images/items/1.png" 54 54)});
    loop map joueur;

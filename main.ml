open Generation
open Graphic
open Types
open Player
open Enemy
open Items

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
        map.batiment <- (generate_map 40 1000 (x1 + i) (y1 + j)) @ map.batiment;
        map.enemies <- generate_enemy 200 (x1 + i) (y1 + j) map.batiment @ map.enemies)
  done;
done; 
()

let setup ()=
  Random.self_init ();
  Raylib.init_window w h "OGamel";
  Raylib.set_target_fps 60;
  fill_item_table ();
  let bat = generate_map 40 1000 0 0 in 
  {batiment = bat; floor = generate_floor (); roads = generate_road 0 0; generated = [(0,0)]; enemies = generate_enemy 200 0 0 bat}
 

let rec loop map joueur=
  if Raylib.window_should_close () then Raylib.close_window ()
  
  else
    update_player joueur map;
    update_map map joueur;
    update_enemies map.enemies joueur map;
    let open Raylib in
    begin_drawing ();
    draw_map map joueur;
    draw_fps 300 300;
    draw_text (string_of_int joueur.x) 50 50 30 Color.red;
    draw_text (string_of_int joueur.y) 140 50 30 Color.red;
    draw_hearth joueur;
    draw_food joueur;
    draw_current_item joueur;
    if joueur.is_inventory_open then draw_inventory joueur;
    end_drawing ();
    loop map joueur

let () =
    let map = setup () in 
    let joueur = player_init () in 
    get_item joueur (item_from_id 1) 237 0;
    get_item joueur (item_from_id 1) 27 44;
    get_item joueur (item_from_id 2) 214 9;
    get_item joueur (item_from_id 3) 24 18;
    get_item joueur (item_from_id 4) 1 36;
    get_item joueur (item_from_id 5) 1 37;
    get_item joueur (item_from_id 6) 1 38;
    get_item joueur (item_from_id 7) 1 39;
    get_item joueur (item_from_id 8) 1 40;
    get_item joueur (item_from_id 9) 1 41;
    use_item_durability joueur 40 80;
    loop map joueur
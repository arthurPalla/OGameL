open Generation
open Graphic
open Types
open Player
open Enemy
open Items

let draw_init (joueur:Types.player) = 
  let x,y = Raylib.get_mouse_x (), Raylib.get_mouse_y () in 

  if Raylib.is_mouse_button_pressed Raylib.MouseButton.Left && x > 400  && y > 820 && x < 400 + 200  && y < 820 + 100 then
    joueur.health <- 20;
  let open Raylib in 
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "OGameL" 400 30 60 Color.red;
    draw_rectangle 100 100 800 700 Color.black;
    draw_rectangle (100 + 6) (100+6) (800 - 12) (700 - 12) Raylib.Color.gray;
    draw_text "Bienvenue" 400 120 50 Color.white;
    draw_text "Vous êtes un mage sans pouvoir qui est apparu " 150 200 30 Color.white;
    draw_text "sans raison apparente dans un monde truffé " 150 250 30 Color.white;
    draw_text "de petites créatures vertes" 150 300 30 Color.white;
    draw_text "Pourrez vous survivre ? " 150 350 30 Color.white;
    draw_text "Touches :" 400 450 40 Color.white;
    draw_text "déplacement: z / q / s /d" 300 500 30 Color.white;
    draw_text "inventaire: i" 300 550 30 Color.white;
    draw_text "Attaquer / récolter bois: a" 300 600 30 Color.white;
    draw_text "Entrez / Sortir des maison: e" 300 650 30 Color.white;



    draw_rectangle 400 820 200 100 Color.black;
    draw_rectangle (400 + 6) (820+6) (200 - 12) (100 - 12) Raylib.Color.maroon;
    draw_text "Jouer" (400 + 200 / 2 -23) (820 + 100 /2 - 10) 20 Raylib.Color.black;
    end_drawing ()

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
  
  else begin
    if joueur.health <= 0 then (draw_death () ;loop map joueur)
    else if joueur.health > 20 then (draw_init joueur; loop map joueur)
    else (
    update_map map joueur;
    update_player joueur map;
    update_enemies map.enemies joueur map;
    let open Raylib in
    begin_drawing ();
    draw_map map joueur;
    draw_text (string_of_int joueur.x) 50 50 30 Color.red;
    draw_text (string_of_int joueur.y) 140 50 30 Color.red;
    draw_hearth joueur;
    draw_food joueur;
    draw_current_item joueur;
    if joueur.is_inventory_open then draw_inventory joueur;
    end_drawing ();
    loop map joueur)
    end 

let () =
    let map = setup () in 
    let joueur = player_init () in 
    get_item joueur (item_from_id 1) 27 0;
    get_item joueur (item_from_id 2) 24 1;
    get_item joueur (item_from_id 3) 24 18;
    get_item joueur (item_from_id 4) 1 36;
    get_item joueur (item_from_id 4) 1 16;
    get_item joueur (item_from_id 5) 1 37;
    get_item joueur (item_from_id 6) 1 38;
    get_item joueur (item_from_id 7) 1 39;
    get_item joueur (item_from_id 8) 1 40;
    get_item joueur (item_from_id 9) 1 41;
    loop map joueur
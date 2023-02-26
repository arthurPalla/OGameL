open Generation
open Graphic
open Types
open Player
open Enemy
open Items

let draw_init (joueur:Types.player) = 
  let x,y = Raylib.get_mouse_x (), Raylib.get_mouse_y () in 

  if Raylib.is_mouse_button_pressed Raylib.MouseButton.Left && x > 400  && y > 870 && x < 400 + 200  && y < 870 + 100 then
    joueur.health <- 20;
  let open Raylib in 
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "OGameL" 400 30 60 Color.red;
    draw_rectangle 100 100 800 750 Color.black;
    draw_rectangle (100 + 6) (100+6) (800 - 12) (750 - 12) Raylib.Color.gray;
    draw_text "Bienvenue" 400 120 50 Color.white;
    draw_text "Vous êtes un mage sans pouvoir qui est apparu " 150 200 30 Color.white;
    draw_text "sans raison apparente dans un monde truffé " 150 250 30 Color.white;
    draw_text "de petites créatures vertes" 150 300 30 Color.white;
    draw_text "Pourrez vous survivre ? " 150 350 30 Color.white;
    draw_text "Touches :" 400 450 40 Color.white;
    draw_text "Déplacement: z / q / s /d" 300 500 30 Color.white;
    draw_text "Ouvrir l'inventaire: i" 300 550 30 Color.white;
    draw_text "Attaquer / Récolter bois: a" 300 600 30 Color.white;
    draw_text "Entrez / Sortir des maison: e" 300 650 30 Color.white;
    draw_text "changer d'item dans la main : 1/2/3 ..." 300 700 30 Color.white;
    draw_text "Ouvrir les coffres : o" 300 750 30 Color.white;



    draw_rectangle 400 870 200 100 Color.black;
    draw_rectangle (400 + 6) (870+6) (200 - 12) (100 - 12) Raylib.Color.maroon;
    draw_text "Jouer" (400 + 200 / 2 -23) (870 + 100 /2 - 10) 20 Raylib.Color.black;
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
  Raylib.init_audio_device ();
  fill_item_table ();
  let bat = generate_map 40 1000 0 0 in 
  {batiment = bat; floor = generate_floor (); roads = generate_road 0 0; generated = [(0,0)]; enemies = generate_enemy 200 0 0 bat}
 

let rec loop map joueur music=
  if Raylib.window_should_close () then Raylib.close_window ()
  
  else begin 
    if joueur.health <= 0 then (draw_death () ;loop map joueur music)
    else if joueur.health > 20 then (draw_init joueur; loop map joueur music)
    else (
      Raylib.update_music_stream music;
    Raylib.play_music_stream music;
    update_map map joueur;
    update_player joueur map;
    update_enemies map.enemies joueur map;
    let open Raylib in
    begin_drawing ();
    draw_map map joueur;
    draw_text (string_of_int joueur.x) 50 50 30 Color.red;
    draw_text (string_of_int joueur.y) 140 50 30 Color.red;
    let kill_indicator_text = "Kills : " ^ (string_of_int !kill_counter) in draw_text kill_indicator_text (950 - measure_text kill_indicator_text 30) 50 30 Color.black;
    draw_hearth joueur;
    draw_food joueur;
    draw_current_item joueur;
    if joueur.is_inventory_open then draw_inventory joueur;
    end_drawing ();
    loop map joueur music)
    end 

let () =
    let map = setup () in 
    let joueur = player_init () in 
    let music = Raylib.load_music_stream "./music/lavanville.mp3" in
    get_item joueur (item_from_id 9) 1 36;
    loop map joueur music
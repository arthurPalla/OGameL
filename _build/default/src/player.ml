let player_init () : (Types.player) = 
  let test = Array.init 4 (fun i ->
    let a2 = Array.init 9 (fun j -> (Graphic.texture_crop_and_resize "./images/player_sheet.png" (float_of_int(j*32)) (float_of_int(291 + i*73)) 32. 73. 50 100))
  in
  {Types.elements = a2; i=0; length = 10}) in 
  {health =11 ; feed = 11; x= 500; y = 500; texture = test; direction = 0; inventory = Array.make 45 None; is_inventory_open = false}

let go_forward (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x) (player.y - 5) map.batiment) then 
    player.y <- player.y -5; 
  player.direction <- 0;   
  Types.cyclic_next player.texture.(0)

let go_backward (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x) (player.y + 5) map.batiment) then 
    player.y <- player.y +5; 
  player.direction <- 1;   
  Types.cyclic_next player.texture.(1)

let go_right (player:Types.player) (map:Types.map) = 
  if not (Physic.collision (player.x + 5) (player.y) map.batiment) then 
    player.x <- player.x +5; 
  player.direction <- 3;  
  Types.cyclic_next player.texture.(3)

let go_left (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x - 5) (player.y) map.batiment) then 
    player.x <- player.x -5;
  player.direction <- 2;   
  Types.cyclic_next player.texture.(2)

let show_hide_inventory (player:Types.player) =
  if player.is_inventory_open then player.is_inventory_open<-false
  else player.is_inventory_open<-true

let update_player (joueur:Types.player) map = 
  let open Raylib in
  if(is_key_down Key.W) && (not joueur.is_inventory_open) then go_forward joueur map
  else if(is_key_down Key.S) && (not joueur.is_inventory_open) then go_backward joueur map
  else if(is_key_down Key.D) && (not joueur.is_inventory_open) then go_right joueur map
  else if(is_key_down Key.A) && (not joueur.is_inventory_open) then go_left joueur map
  else if (is_key_pressed Key.I) then show_hide_inventory joueur
  ; ()
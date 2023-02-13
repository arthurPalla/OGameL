open Types
open Graphic
open Physic

let player_init () : player = 
  let test = Array.init 4 (fun i ->
    let a2 = Array.init 9 (fun j -> (texture_crop_and_resize "./images/player_sheet.png" (float_of_int(j*32)) (float_of_int(291 + i*73)) 32. 73. 50 100))
  in
  {elements = a2; i=0; length = 10}) in 
  {health =20 ; feed = 20; x= 500; y = 500; texture = test; direction = 0; inventory = Array.make 45 None; is_inventory_open = false}

let go_forward (player:player) (map:map) =
  if not (collision (player.x) (player.y - 5) map.batiment) then 
    player.y <- player.y -5; 
  player.direction <- 0;   
  cyclic_next player.texture.(0)

let go_backward (player:player) (map:map) =
  if not (collision (player.x) (player.y + 5) map.batiment) then 
    player.y <- player.y +5; 
  player.direction <- 1;   
  cyclic_next player.texture.(1)

let go_right (player:player) (map:map) = 
  if not (collision (player.x + 5) (player.y) map.batiment) then 
    player.x <- player.x +5; 
  player.direction <- 3;  
  cyclic_next player.texture.(3)

let go_left (player:player) (map:map) =
  if not (collision (player.x - 5) (player.y) map.batiment) then 
    player.x <- player.x -5;
  player.direction <- 2;   
  cyclic_next player.texture.(2)

let show_hide_inventory (player:player) =
  if player.is_inventory_open then player.is_inventory_open<-false
  else player.is_inventory_open<-true

let update_player (joueur:player) map = 
  let open Raylib in
  if(is_key_down Key.W) && (not joueur.is_inventory_open) then go_forward joueur map
  else if(is_key_down Key.S) && (not joueur.is_inventory_open) then go_backward joueur map
  else if(is_key_down Key.D) && (not joueur.is_inventory_open) then go_right joueur map
  else if(is_key_down Key.A) && (not joueur.is_inventory_open) then go_left joueur map
  else if (is_key_pressed Key.I) then show_hide_inventory joueur
  ; ()
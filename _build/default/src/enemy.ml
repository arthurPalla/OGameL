let goblin_init x1 y1  : (Types.enemy)= 
  let text = Array.init 4 (fun i ->
    let a2 = Array.init 7 (fun j -> (Graphic.texture_crop_and_resize "./images/goblinsword.png" (float_of_int(j*64)) (float_of_int(i*64)) 64. 64. 100 100))
  in

  {Types.elements = a2; i=0; length = 8}) in 
  let texture_attack =  Array.init 4 (fun i ->
    let a2 = Array.init 3 (fun j -> (Graphic.texture_crop_and_resize "./images/goblinsword.png" (float_of_int( 518+ j*64)) (float_of_int(i*64)) 64. 64. 100 100))
  in
  {Types.elements = a2; i=2; length = 3}) in
  let death_texture = let a2 = Array.init 5 (fun j -> (Graphic.texture_crop_and_resize "./images/goblinsword.png" (float_of_int(j*64)) (float_of_int(256)) 64. 64. 100 100))
  in
  {Types.elements = a2; i=0; length = 5} in
  {health =20 ; x= x1; y = y1; texture = text; direction = 0; is_attacking = false; attack_texture = texture_attack; sleep_time = 0; moove_time = 0; death_texture = death_texture}

let go_forward (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x) (enemy.y - 5) None map) then 
    enemy.y <- enemy.y -5; 
  enemy.direction <- 2;   
  Types.cyclic_next enemy.texture.(2)

let go_backward (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x) (enemy.y + 5) None map) then 
    enemy.y <- enemy.y +5; 
  enemy.direction <- 0;   
  Types.cyclic_next enemy.texture.(0)

let go_right (enemy:Types.enemy) (map:Types.map) = 
  if not (Physic.collision (enemy.x + 5) (enemy.y) None map) then 
    enemy.x <- enemy.x +5; 
  enemy.direction <- 1;  
  Types.cyclic_next enemy.texture.(1)

let go_left (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x - 5) (enemy.y) None map) then 
    enemy.x <- enemy.x -5;
  enemy.direction <- 3;   
  Types.cyclic_next enemy.texture.(3)

let attack (enemy:Types.enemy) (joueur:Types.player) = 
  enemy.is_attacking <- true;
  if enemy.attack_texture.(enemy.direction).i = 0 && joueur.inside_batiment = None then 
    match enemy.direction with
    |0 -> if abs (enemy.x - joueur.x + 25) <= 50 && joueur.y - enemy.y <= 70 then joueur.health <- joueur.health - 1
    |1 -> if abs (enemy.y - joueur.y) <= 50 && joueur.x  - enemy.x <= 70 then joueur.health <- joueur.health -1
    |2 -> if abs (enemy.x - joueur.x + 25) <= 50 && enemy.y - joueur.y <= 70 then joueur.health <- joueur.health - 1
    |3 -> if abs (enemy.y - joueur.y) <= 50 && enemy.x - joueur.x - 50 <= 70 then joueur.health <- joueur.health -1
    |_ -> failwith "impossible case"

let should_moove (enemy:Types.enemy) = 
  if enemy.sleep_time >= 2 then(
    enemy.sleep_time <- 0; true)
  else(
    enemy.sleep_time <- enemy.sleep_time + 1;
    false)
let should_change_direction (enemy:Types.enemy) = 
  if enemy.moove_time >=8 then (
    enemy.moove_time <- 0; true)  
  else(
    enemy.moove_time <- enemy.moove_time +1; 
    false) 
(*
let face_player (enemy:Types.enemy) (joueur:Types.player) = 
  if abs(enemy.x - joueur.x) <= abs(enemy.y - joueur.y) then (
    if enemy.x <= joueur.x then 
      enemy.direction <- 3
    else enemy.direction <- 1
  )
  else (
    if enemy.y <= joueur.y then 
      enemy.direction <- 0  
  else enemy.direction <- 2
  )*)
let continue_direction (enemy:Types.enemy) (map:Types.map)= 
  match enemy.direction with 
  |0 -> go_backward enemy map
  |1 -> go_right enemy map
  |2 -> go_forward enemy map
  |3 -> go_left enemy map
  |_ -> failwith "Bizzarre"

let stop_attack (enemy:Types.enemy) = 
  for i = 0 to 3 do 
    Types.reset_indice_max enemy.attack_texture.(i)
  done;
  enemy.is_attacking <-false
let update_enemy (enemy:Types.enemy) (joueur:Types.player) (map:Types.map)= 
  if enemy.health <= 0 then Types.cyclic_next_to_max enemy.death_texture
  else if abs (enemy.x -joueur.x) <= 500 && abs (enemy.y -joueur.y) <= 500  then begin 
    if not (should_change_direction enemy) && (abs (enemy.x -joueur.x + 25) >= 20 && abs (enemy.y -joueur.y) >= 20)then(
      stop_attack enemy;
      continue_direction enemy map)
    else if abs (enemy.x -joueur.x) >= abs (enemy.y -joueur.y) && abs (enemy.x -joueur.x+ 25 ) >=50 then (
      stop_attack enemy;
      if enemy.x <= joueur.x then go_right enemy map
      else go_left enemy map
    )
    else if abs (enemy.x -joueur.x) <= abs (enemy.y -joueur.y) && abs (enemy.y -joueur.y) >= 50 then (
      stop_attack enemy;
      if enemy.y <= joueur.y then go_backward enemy map
      else go_forward enemy map;
    )
    else if abs (enemy.x -joueur.x + 25 ) <=50 || abs (enemy.y -joueur.y) <=50 then(
      if abs (enemy.x -joueur.x) <= abs (enemy.y -joueur.y) then (
        if enemy.y - joueur.y >= 20 then (go_forward enemy map ; stop_attack enemy)
        else if joueur.y - enemy.y >= 20 then (go_backward enemy map; stop_attack enemy)
        else (Types.cyclic_former enemy.attack_texture.(enemy.direction);attack enemy joueur) 
        )
      else (
        if enemy.x - joueur.x + 25>=70 then( go_left enemy map; stop_attack enemy)
        else if joueur.x - enemy.x + 25 >= 70 then( go_right enemy map;       stop_attack enemy)
        else (Types.cyclic_former enemy.attack_texture.(enemy.direction);attack enemy joueur) 

      )
    )
      else (Types.cyclic_former enemy.attack_texture.(enemy.direction);attack enemy joueur)
  end 

let  update_enemies (enemies:Types.enemy list) (joueur:Types.player) (map:Types.map) =
  let rec aux l acc =
    match l with 
    |t::q -> if should_moove t then update_enemy t joueur map; aux q (t::acc)
    |[] -> acc 
  in map.enemies <- aux enemies []

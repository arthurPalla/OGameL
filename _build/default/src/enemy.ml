let goblin_init x1 y1  : (Types.enemy)= 
  let text = Array.init 4 (fun i ->
    let a2 = Array.init 7 (fun j -> (Graphic.texture_crop_and_resize "./images/goblinsword.png" (float_of_int(j*64)) (float_of_int(i*64)) 64. 64. 100 100))
  in

  {Types.elements = a2; i=0; length = 8}) in 
  let texture_attack =  Array.init 4 (fun i ->
    let a2 = Array.init 2 (fun j -> (Graphic.texture_crop_and_resize "./images/goblinsword.png" (float_of_int(512 + j*64)) (float_of_int(i*64)) 32. 64. 100 100))
  in
  {Types.elements = a2; i=0; length = 3}) in

  {health =11 ; x= x1; y = y1; texture = text; direction = 0; is_attacking = false; attack_texture = texture_attack; sleep_time = 0}

let go_forward (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x) (enemy.y - 5) map.batiment) then 
    enemy.y <- enemy.y -5; 
  enemy.direction <- 2;   
  Types.cyclic_next enemy.texture.(2)

let go_backward (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x) (enemy.y + 5) map.batiment) then 
    enemy.y <- enemy.y +5; 
  enemy.direction <- 0;   
  Types.cyclic_next enemy.texture.(0)

let go_right (enemy:Types.enemy) (map:Types.map) = 
  if not (Physic.collision (enemy.x + 5) (enemy.y) map.batiment) then 
    enemy.x <- enemy.x +5; 
  enemy.direction <- 1;  
  Types.cyclic_next enemy.texture.(1)

let go_left (enemy:Types.enemy) (map:Types.map) =
  if not (Physic.collision (enemy.x - 5) (enemy.y) map.batiment) then 
    enemy.x <- enemy.x -5;
  enemy.direction <- 3;   
  Types.cyclic_next enemy.texture.(3)

let should_moove (enemy:Types.enemy) = 
  if enemy.sleep_time >= 5 then(
    enemy.sleep_time <- 0; true)
  else(
    enemy.sleep_time <- enemy.sleep_time + 1;
    false)
let update_enemy (enemy:Types.enemy) (joueur:Types.player) (map:Types.map)= 
  if abs (enemy.x -joueur.x) <= 500 && abs (enemy.y -joueur.y) <= 500 then begin 
    if abs (enemy.x -joueur.x) >= abs (enemy.y -joueur.y) then  (
      if enemy.x <= joueur.x then go_right enemy map
      else go_left enemy map
    )
    else (
      if enemy.y <= joueur.y then go_backward enemy map
      else go_forward enemy map;
    )
  end

let  update_enemies (enemies:Types.enemy list) (joueur:Types.player) (map:Types.map) =
  let rec aux l acc =
    match l with 
    |t::q -> if should_moove t then update_enemy t joueur map; aux q (t::acc)
    |[] -> acc 
  in map.enemies <- aux enemies []

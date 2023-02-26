let selected_slot = ref (-1)
let kill_counter = ref 0

let player_init () : (Types.player) = 
  let test = Array.init 4 (fun i ->
    let a2 = Array.init 9 (fun j -> (Graphic.texture_crop_and_resize "./images/player_sheet.png" (float_of_int(j*32)) (float_of_int(291 + i*73)) 32. 73. 50 100))
  in
  {Types.elements = a2; i=0; length = 10}) in 
  {health =21 ;inside_batiment= None; feed = 20; x= 500; y = 500; texture = test; direction = 0; inventory = Array.make 45 None; is_inventory_open = false; hand = 0; is_hitting = false; hit_step = 0}

let go_forward (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x) (player.y - 5) player.inside_batiment map) then 
    player.y <- player.y -5; 
  player.direction <- 0;   
  Types.cyclic_next player.texture.(0)

let go_backward (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x) (player.y + 5) player.inside_batiment map) then 
    player.y <- player.y +5; 
  player.direction <- 1;   
  Types.cyclic_next player.texture.(1)

let go_right (player:Types.player) (map:Types.map) = 
  if not (Physic.collision (player.x + 5) (player.y) player.inside_batiment map) then 
    player.x <- player.x +5; 
  player.direction <- 3;  
  Types.cyclic_next player.texture.(3)

let go_left (player:Types.player) (map:Types.map) =
  if not (Physic.collision (player.x - 5) (player.y) player.inside_batiment map) then 
    player.x <- player.x -5;
  player.direction <- 2;   
  Types.cyclic_next player.texture.(2)

let show_hide_inventory (player:Types.player) =
  if player.is_inventory_open then player.is_inventory_open<-false
  else player.is_inventory_open<-true

let rec get_item (player:Types.player) (item:Types.item) (nb:int) (c:int) =
  if c < 45 && item.stackable then
    match player.inventory.(c) with
    | None -> if nb <= 99 then (player.inventory.(c) <- Some (nb, item))
              else (player.inventory.(c) <- Some (99, item); get_item player item (nb - 99) (c + 1))
    | Some (n, i) when (i  = item) -> if n = 99 then (get_item player item nb (c+ 1)) 
                                    else if (n + nb) > 99 then (player.inventory.(c) <- Some (99, i); get_item player item (n + nb - 99) (c + 1))
                                    else if (n + nb) <= 99 then (player.inventory.(c) <- Some (n + nb, i))
    | Some _ -> get_item player item nb (c + 1)
  else if c < 45 && (not item.stackable) then
    match player.inventory.(c) with
    | None -> player.inventory.(c) <- Some (1, item)
    | _ -> get_item player item nb (c + 1)
  else ()

let rec drop_item (player:Types.player) (item:Types.item) (nb:int) (c:int) =
  if c < 45 then
    match player.inventory.(c) with
    | Some (n,i) when (i = item) -> if (nb = n) then (player.inventory.(c) <- None; nb)
                                    else if (nb < n) then (player.inventory.(c) <- Some ((n - nb), i); nb)
                                    else (player.inventory.(c) <- None; drop_item player item (nb - n) (c + 1) + nb) 
    | None | Some _ -> drop_item player item nb (c + 1) + 0
  else 0

let move_item (player:Types.player) (c1:int) (c2:int) = 
  match (player.inventory.(c1), player.inventory.(c2)) with
  | _, None -> player.inventory.(c2) <- player.inventory.(c1); player.inventory.(c1) <- None
  | None, _ -> ()
  | Some (n1, i1), Some (n2, i2) -> if not (i1 = i2) then ()
                                    else
                                      if (n1 + n2) < 100 then (
                                        player.inventory.(c1) <- None;
                                        player.inventory.(c2) <- Some ((n1 + n2), i1)
                                      ) else (
                                        player.inventory.(c2) <- Some (99, i1);
                                        player.inventory.(c1) <- Some ((n1 + n2 - 99), i1)
                                      )

let rec enter_house (joueur:Types.player) (bat:Types.batiments list) = 
  match bat with
  |t::q -> if t.inside <> None &&  abs(t.x - joueur.x) <= 200 && abs(t.y - joueur.y) <=200 then Some t else enter_house joueur q
  |[] ->if joueur.inside_batiment <> None then begin joueur.x <- (Option.get (joueur.inside_batiment)).x + 30; joueur.y <- (Option.get (joueur.inside_batiment)).y + 30 end;None 

let hand_select (player:Types.player) (hand:int) = 
  player.hand <- hand

let interaction (joueur:Types.player) (map:Types.map) = 
  if joueur.inside_batiment <> None then begin joueur.x <- (Option.get (joueur.inside_batiment)).x + 0; joueur.y <- (Option.get (joueur.inside_batiment)).y + 50; joueur.inside_batiment <- None end
  else joueur.inside_batiment <- enter_house joueur map.batiment

let rec empty_chest (joueur:Types.player) (l:Types.item list) =
  match l with 
  | t::q -> get_item joueur t 1 0; empty_chest joueur q
  | [] -> ()

let rec open_chest (joueur:Types.player) (bat:Types.batiments list) =
  match bat with 
  | t::q -> if joueur.inside_batiment <> None && (Option.get joueur.inside_batiment) = t then (empty_chest joueur t.chest_content; t.chest_content <- [])
            else open_chest joueur q
  | [] -> ()
            

let mouse_detect_slot () =
  let mouse_x = Raylib.get_mouse_x () in
  let mouse_y = Raylib.get_mouse_y () in
  let col = (mouse_x - 221) / 63 in
  let row = (mouse_y - 360) / 63 in
  let c = col + 9 * row in
  if col >= 0 && col < 9 && mouse_y >= 360 && row < 5 then c
  else -1

let drag_n_drop (player:Types.player) =
  let slot = mouse_detect_slot () in
    if (slot < 0) then (
      match player.inventory.(!selected_slot) with
      | None -> ()
      | Some (n,i) -> ignore (drop_item player i n !selected_slot);
      selected_slot := -1
    ) else (
      move_item player !selected_slot slot; 
      selected_slot := -1
    )

let can_cut_tree (joueur: Types.player) (batiment:Types.batiments) = 
  if abs (batiment.x - joueur.x + 25) <= 70 && abs (batiment.y - joueur.y) <= 70 then
    match joueur.direction with
    |1 -> batiment.y - joueur.y <= 70 
    |3 -> batiment.x  - joueur.x  <= 70
    |0 -> joueur.y - batiment.y <= 70
    |2 -> joueur.x - batiment.x - 50 <= 70
    |_ -> failwith "impossible case";
  else false

let cut_tree (player:Types.player) (map:Types.map) =
  let rec aux (bat:Types.batiments list) =
    match bat with
    | t::q -> if t.bat_type = "tree" && can_cut_tree player t then
                begin
                  get_item player (Items.item_from_id 1) (Random.int 10) 0;
                  get_item player (Items.item_from_id 3) (Random.int 10) 0;
                  q
                end
              else t::aux q
    | [] -> []
  in map.batiment <- aux map.batiment

let attack_enemy (joueur: Types.player) (enemy:Types.enemy) damage = 
  let is_enemy_alive = (enemy.health) > 0 in
  if joueur.hit_step >= 0 && abs (enemy.x - joueur.x + 25) <= 70 && abs (enemy.y - joueur.y) <= 70 then  (
    match joueur.direction with
    |1 -> if  enemy.y - joueur.y <= 70 then (enemy.health <- enemy.health - damage; if is_enemy_alive && enemy.health <= 0 then incr kill_counter)
    |3 -> if enemy.x  - joueur.x <= 70 then (enemy.health <- enemy.health - damage; if is_enemy_alive && enemy.health <= 0 then incr kill_counter)
    |0 -> if  joueur.y - enemy.y <= 70 then (enemy.health <- enemy.health - damage; if is_enemy_alive && enemy.health <= 0 then incr kill_counter)
    |2 -> if joueur.x - enemy.x - 50 <= 70 then (enemy.health <- enemy.health - damage; if is_enemy_alive && enemy.health <= 0 then incr kill_counter)
    |_ -> failwith "impossible case");
  enemy
  
let attack_enemies (player:Types.player) (map:Types.map) (damage:int) = 
  let rec aux list = 
    match list with 
    |t::q -> (attack_enemy player t damage) :: aux q
    |[] -> []
  in 
  map.enemies <- aux map.enemies
   
let player_attack (player:Types.player) (map:Types.map) =
  match player.inventory.(36 + player.hand) with
  | Some (_,i) -> if i.name = "Axe" then (attack_enemies player map 5; cut_tree player map; i.durability <- Some ((Option.get i.durability) - 5); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))
                  else if i.name = "Common Sword" then (attack_enemies player map 3; i.durability <- Some ((Option.get i.durability) - 4); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))
                  else if i.name = "Rare Sword" then (attack_enemies player map 5; i.durability <- Some ((Option.get i.durability) - 4); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))
                  else if i.name = "Master Sword" then (attack_enemies player map 10; i.durability <- Some ((Option.get i.durability) - 4); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))
                  else if i.name = "Legendary Sword" then (attack_enemies player map 20; i.durability <- Some ((Option.get i.durability) - 4); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))
                  else if i.name = "Pickaxe" then (attack_enemies player map 2; i.durability <- Some ((Option.get i.durability) - 4); if (Option.get i.durability) < 0 then ignore (drop_item player i 1 (player.hand + 36)))

  | None -> ()

let update_hit (player:Types.player) (map:Types.map) =
  if player.is_hitting && player.hit_step < 30 then
    player.hit_step <- player.hit_step + 2
  else if player.is_hitting && player.hit_step = 30 then
    (player.is_hitting <- false; player_attack player map)
  else if (not player.is_hitting) && player.hit_step > 0 then
    player.hit_step <- player.hit_step - 5
  else ()


let update_player (joueur:Types.player) map = 
  update_hit joueur map;
  let open Raylib in
  if (joueur.is_inventory_open) && (!selected_slot != -1) && (is_mouse_button_released MouseButton.Left) then drag_n_drop joueur;
  if (joueur.is_inventory_open) && (is_mouse_button_down MouseButton.Left) && (!selected_slot = -1) then selected_slot := mouse_detect_slot ();
  if(is_key_down Key.W) && (not joueur.is_inventory_open) then go_forward joueur map
  else if(is_key_down Key.S) && (not joueur.is_inventory_open) then go_backward joueur map
  else if(is_key_down Key.D) && (not joueur.is_inventory_open) then go_right joueur map
  else if(is_key_down Key.A) && (not joueur.is_inventory_open) then go_left joueur map
  else if (is_key_down Key.Q) && (not joueur.is_hitting) && (joueur.hit_step = 0) then joueur.is_hitting <- true
  else 
    let key = get_key_pressed () in
    match key with
    | Key.I -> show_hide_inventory joueur
    | Key.One -> hand_select joueur 0
    | Key.Two -> hand_select joueur 1
    | Key.Three -> hand_select joueur 2
    | Key.Four -> hand_select joueur 3
    | Key.Five -> hand_select joueur 4
    | Key.Six -> hand_select joueur 5
    | Key.Seven -> hand_select joueur 6
    | Key.Eight -> hand_select joueur 7
    | Key.Nine -> hand_select joueur 8
    | Key.E -> interaction joueur map
    | Key.O -> open_chest joueur map.batiment
    | _ -> ()
  ; ()
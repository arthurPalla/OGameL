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

let rec get_item (player:Types.player) (item:Types.item) (nb:int) (c:int) =
  if c < 45 && item.stackable then
    match player.inventory.(c) with
    | None -> if nb <= 99 then (player.inventory.(c) <- Some (nb, item))
              else (player.inventory.(c) <- Some (99, item); get_item player item (nb - 99) (c + 1))
    | Some (n, i) when (i = item) -> if n = 99 then (get_item player item nb (c+ 1)) 
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

let update_player (joueur:Types.player) map = 
  let open Raylib in
  if(is_key_down Key.W) && (not joueur.is_inventory_open) then go_forward joueur map
  else if(is_key_down Key.S) && (not joueur.is_inventory_open) then go_backward joueur map
  else if(is_key_down Key.D) && (not joueur.is_inventory_open) then go_right joueur map
  else if(is_key_down Key.A) && (not joueur.is_inventory_open) then go_left joueur map
  else if (is_key_pressed Key.I) then show_hide_inventory joueur
  ; ()
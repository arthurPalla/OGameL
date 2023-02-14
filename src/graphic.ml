let w = 1000
let h = 1000
let texture_from_image image xsize ysize =
  let open Raylib in
  image_resize (addr image) xsize ysize;
  let tex = load_texture_from_image image in
  unload_image image; (* Je pense que c'est safe ici mais c'est bizarre *)
  tex

let texture_from_image_name =
    let cache = Hashtbl.create 42 in
    let aux image_name xsize ysize =
        if not (Hashtbl.mem cache (image_name, xsize, ysize))
        then Hashtbl.add cache (image_name, xsize, ysize)
            (texture_from_image (Raylib.load_image image_name) xsize ysize);
        Hashtbl.find cache (image_name, xsize, ysize)
    in aux

let texture_batiment_from_int n =
  match n with
  |0 -> texture_from_image_name "./housesetx1/house1x1.gif" 100 90
  |1 -> texture_from_image_name "./housesetx1/house2x1.gif" 110 110
  |2 -> texture_from_image_name "./housesetx1/house3x1.gif" 120 120
  |_ -> failwith "error while loading house texture"

let texture_crop_and_resize =
  let cache = Hashtbl.create 42 in
  let aux s cox coy xsize ysize w h =
      let key = (s, cox, coy, xsize, w, h) in
      if not (Hashtbl.mem cache key)
      then begin
          let open Raylib in
          let img = load_image(s) in
          let rect = Rectangle.create cox coy xsize ysize in
          image_crop (addr img) rect;
          let tex = texture_from_image img w h in
          Hashtbl.add cache key tex
      end;
      Hashtbl.find cache key
  in aux
  
let texture_tree_from_int n = 
  match n with 
  |0 -> texture_crop_and_resize "./images/autumn_2.png" 0. 0. 112. 128. 100 100
  |1 -> texture_crop_and_resize "./images/autumn_2.png" 114. 0. 112. 128. 100 100
  |2 -> texture_crop_and_resize "./images/autumn_3.png" 0. 0. 112. 128. 100 100
  |3 -> texture_crop_and_resize "./images/autumn_3.png" 114. 0. 112. 128. 100 100
  |4 -> texture_crop_and_resize "./images/autumn.png" 0. 0. 112. 128. 100 100
  |5 -> texture_crop_and_resize "./images/autumn.png" 114. 0. 112. 128. 100 100
  |6 -> texture_crop_and_resize "./images/blue_trees.png" 0. 0. 112. 128. 100 100
  |7 -> texture_crop_and_resize "./images/blue_trees.png" 114. 0. 112. 128. 100 100
  |8 -> texture_crop_and_resize "./images/brown_trees_2.png" 0. 0. 112. 128. 100 100
  |9 -> texture_crop_and_resize "./images/brown_trees_2.png" 114. 0. 112. 128. 100 100
  |10-> texture_crop_and_resize "./images/brown_trees.png" 0. 0. 112. 128. 100 100
  |11 -> texture_crop_and_resize "./images/brown_trees.png" 114. 0. 112. 128. 100 100
  |12 -> texture_crop_and_resize "./images/green_trees.png" 0. 0. 112. 128. 100 100
  |13 -> texture_crop_and_resize "./images/green_trees.png" 114. 0. 112. 128. 100 100
  |14 -> texture_crop_and_resize "./images/yellow_trees.png" 0. 0. 112. 128. 100 100
  |15 -> texture_crop_and_resize "./images/yellow_trees.png" 114. 0. 112. 128. 100 100
  |_ -> failwith "error while loading trees texture"

let rec draw_road (l:Types.batiments list) (joueur:Types.player) =
match l with
|t::q ->if t.x <=  joueur.x + 600 && t.x >= joueur.x - 600 && t.y <= joueur.y + 600 && t.y >= joueur.y - 600 then   Raylib.draw_texture t.texture (t.x - joueur.x + 500) (t.y - joueur.y + 500) Raylib.Color.white;draw_road q joueur
|[] -> ()

let rec draw_batiment_first_plan (l:Types.batiments list ) (joueur:Types.player) = 
  match l with
  |t::q ->if t.x <=  joueur.x + 600 && t.x >= joueur.x - 600 && t.y <= joueur.y + 600 && t.y >= joueur.y - 600 && t.y < joueur.y then   Raylib.draw_texture t.texture (t.x - joueur.x + 500) (t.y - joueur.y + 500) Raylib.Color.white;draw_batiment_first_plan q joueur
  |[] -> ()

let rec draw_batiment_second_plan (l:Types.batiments list ) (joueur:Types.player) = 
    match l with
    |t::q ->if t.x <=  joueur.x + 600 && t.x >= joueur.x - 600 && t.y <= joueur.y + 600 && t.y >= joueur.y - 600 && t.y >= joueur.y then   Raylib.draw_texture t.texture (t.x - joueur.x + 500) (t.y - joueur.y + 500) Raylib.Color.white;draw_batiment_second_plan q joueur
    |[] -> ()
let draw_floor floor (joueur:Types.player)= 
  let xt = (joueur.x mod 50) in 
  let yt = (joueur.y mod 50) in 
  for i = -1 to (w+1)/50 +1 do 
    for j = -1 to (h+1)/50 + 1 do 
      Raylib.draw_texture floor (i*50 - xt)  (j*50 - yt) Raylib.Color.white
    done;
  done;
  () 

let draw_player (joueur:Types.player) = 
  Raylib.draw_texture (Types.cyclic_top ((joueur.texture).(joueur.direction))) (500) (500) Raylib.Color.white

let draw_inventory (player:Types.player) =
  Raylib.draw_texture (texture_crop_and_resize "./images/inventory.png" 0. 0. 198. 130. 594 390) 203 305 Raylib.Color.white;
  for i = 0 to Array.length player.inventory - 1 do
    match player.inventory.(i) with
    | None -> ()
    | Some(a, b) -> if i >= 36 then (
                      Raylib.draw_texture b.image (221 + 63 * (i mod 9)) 624 Raylib.Color.white;
                      if b.stackable then 
                        if a > 9 then
                          Raylib.draw_text (string_of_int a) (252 + 63 * (i mod 9)) 660 20 Raylib.Color.gray
                        else
                          Raylib.draw_text (string_of_int a) (261 + 63 * (i mod 9)) 660 20 Raylib.Color.gray)
                    else (
                      Raylib.draw_texture b.image (220 + 63 * (i mod 9)) (360 + 63 * (i / 9)) Raylib.Color.white;
                      if b.stackable then
                        if a > 9 then
                          Raylib.draw_text (string_of_int a) (253 + 63 * (i mod 9)) (395 + 63 * (i / 9)) 20 Raylib.Color.gray
                        else 
                          Raylib.draw_text (string_of_int a) (262 + 63 * (i mod 9)) (395 + 63 * (i / 9)) 20 Raylib.Color.gray)
  done

let draw_hearth (joueur:Types.player) =
  let hearth = ref joueur.health in
  for i = 1 to 10 do 
    begin 
      match !hearth with
      |1 -> Raylib.draw_texture (texture_crop_and_resize "./images/half-heart.png" 0. 0. 60. 60. 25 25) (250 + i*45 ) 900 Raylib.Color.white
      |a -> if a <= 0 then Raylib.draw_texture (texture_crop_and_resize "./images/empty-heart.png" 0. 0. 60. 60. 25 25) (250 + i*45) 900 Raylib.Color.white
                    else Raylib.draw_texture (texture_crop_and_resize "./images/heart.png" 0. 0. 60. 60. 25 25) (250 + i*45 ) 900 Raylib.Color.white
    end;
    hearth := !hearth - 2
  done;
()

let draw_food (joueur:Types.player) =
  let food = ref joueur.feed in
  for i = 1 to 10 do 
    begin 
      match !food with
      |1 -> Raylib.draw_texture (texture_crop_and_resize "./images/mc_icons.png" 80. 27. 9. 9. 40 40) (250 + i*45 ) 950 Raylib.Color.gray
      |a -> if a <= 0 then Raylib.draw_texture (texture_crop_and_resize "./images/mc_icons.png" 43. 27. 9. 9. 35 35) (250 + i*45) 950 Raylib.Color.white
                    else Raylib.draw_texture (texture_crop_and_resize "./images/mc_icons.png" 70. 27. 9. 9. 40 40) (250 + i*45 ) 950 Raylib.Color.white
    end;
    food := !food - 2
  done;
  
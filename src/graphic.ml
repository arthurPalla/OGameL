let w = 1000
let h = 1000
let texture_from_image image xsize ysize =
  Raylib.(image_resize (addr image) xsize ysize);
  Raylib.load_texture_from_image(image)


let texture_batiment_from_int n =
  match n with
  |0 -> texture_from_image (Raylib.load_image("./housesetx1/house1x1.gif")) 100 90
  |1 -> texture_from_image (Raylib.load_image("./housesetx1/house2x1.gif")) 110 110
  |2 -> texture_from_image (Raylib.load_image("./housesetx1/house3x1.gif")) 120 120
  |_ -> failwith "error while loading house texture"
  
let texture_crop_and_resize s cox coy xsize ysize w h = 
  let open Raylib in 
  let img = load_image(s) in
  let rect = Rectangle.create cox coy xsize ysize in 
  image_crop (addr img) rect;
  texture_from_image img w h

(*224 128*)
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
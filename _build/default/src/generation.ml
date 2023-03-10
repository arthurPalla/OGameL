let rec generate_coo (l:Types.batiments list) x1 y1 =
  let x,y = Random.int (10*Graphic.w) -5*Graphic.w + 10*x1*Graphic.w, Random.int (10*Graphic.h) - 5*Graphic.h + 10*y1*Graphic.w in
  let rec aux (list:Types.batiments list) =
    match list with
    |t::q -> if x < t.x + t.width + 50 && x > t.x - t.width - 50 && y<t.y + t.height + 50 && y>t.y -t.width-50 then generate_coo l x1 y1 else aux q
    |[] -> (x,y) in
    aux l

let generate_chest_content () = 
  let content = ref [] in
  for _ = 0 to (Random.int 20) do
    let nb_random = Random.int 56 in
    let item_random = match nb_random with
                      | n when n < 15 -> 1
                      | n when n < 30 -> 2
                      | n when n < 40 -> 3
                      | n when n < 43 -> 4
                      | n when n < 45 -> 5
                      | n when n < 50 -> 9
                      | n when n < 53 -> 8
                      | n when n < 55 -> 6
                      | n when n = 55 -> 7
                      | _ -> 1
    in
    content := !content @ [Items.item_from_id item_random]
  done;
  !content

let generate_tree map n x y = 
  let rec aux number acc =
    if number = 0 then acc
    else let x1,y1 = generate_coo acc x y in 
      aux (number-1) ({inside = None; width = 5; height = 5; x = x1; y =y1; texture = Graphic.texture_tree_from_int (Random.int 15); bat_type = "tree"; chest_content = []}::acc)
  in aux n map

let generate_batiments n x y=
  let rec aux number acc =
    if number = 0 then acc
    else match Random.int 3 with
    |0 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 100; height = 80; x = x1; y =y1; texture = Graphic.texture_batiment_from_int 0; inside = Some (Graphic.texture_crop_and_resize "./images/indoor.png" 0. 0. 224. 200. 1500 1000); bat_type = "house"; chest_content = generate_chest_content ()}::acc)
    |1 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 107; height = 65; x = x1; y =y1; texture = Graphic.texture_batiment_from_int 1;  inside = Some (Graphic.texture_crop_and_resize "./images/indoor.png" 0. 0. 224. 200. 1500 1000); bat_type = "house"; chest_content = generate_chest_content ()}::acc)
    |2 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 107; height = 65; x = x1; y =y1; texture = Graphic.texture_batiment_from_int 2;  inside = Some (Graphic.texture_crop_and_resize "./images/indoor.png" 0. 0. 224. 200. 1500 1000); bat_type = "house"; chest_content = generate_chest_content ()}::acc)
    |_ -> failwith("C'est super bizarre si ??a bug ici")

in aux n []
let generate_map n_batiments n_trees x y = 
  let batiment_list = generate_batiments n_batiments x y in 
  generate_tree batiment_list n_trees x y 

let generate_floor ()= 
  Graphic.texture_crop_and_resize "./images/tileset.png" 440. 120. 50. 50. 50 50

let generate_road xchunk ychunk = 
    let text_road = Graphic.texture_crop_and_resize "./images/tileset.png" 0. 64. 28.8 32. 50 50 in 
    let rec aux x1 y1 acc n =
    if n <= 0 then acc
    else 
      let bat : Types.batiments = {width=50; height=50; x=x1 ;y=y1 ; texture = text_road; inside = None; bat_type = "floor"; chest_content = []} in 
      match Random.int 4 with
      |1 -> aux (x1+50) (y1+50) (bat::acc) (n-1) @ aux (x1) (y1+50) (bat::acc) (n-1) @ aux (x1+50) (y1) (bat::acc) (n-1)
      |2 -> aux (x1+50) (y1) (bat::acc) (n-1)
      |3 -> aux (x1) (y1+50) (bat::acc) (n-1)
      |_ -> acc
  in 
  (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 ) @ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 )@ (aux (Random.int (10*Graphic.w) -5*Graphic.w + 10*xchunk*Graphic.w) (Random.int (10*Graphic.h) - 5*Graphic.h + 10*ychunk*Graphic.w) [] 15 ) 
  
let generate_enemy n x y map = 
  let rec aux number acc =
    if number <= 0 then acc
    else 
      let x1,y1 = generate_coo map x y in aux (number-1) ((Enemy.goblin_init x1 y1)::acc)
  in aux n []
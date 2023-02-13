let rec generate_coo (l:Types.batiments list) x1 y1 =
  let x,y = Random.int (4*Graphic.w) -2*Graphic.w + 4*x1*Graphic.w, Random.int (4*Graphic.h) - 2*Graphic.h + 4*y1*Graphic.w in
  let rec aux (list:Types.batiments list) =
    match list with
    |t::q -> if x < t.x + t.width + 50 && x > t.x - t.width - 50 && y<t.y + t.height + 50 && y>t.y -t.width-50 then generate_coo l x1 y1 else aux q
    |[] -> (x,y) in
    aux l

let generate_map n x y=
let rec aux number acc =
  if number = 0 then acc
  else match Random.int 3 with
  |0 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 100; height = 80; x = x1; y =y1; texture = Graphic.texture_from_int 0}::acc)
  |1 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 107; height = 65; x = x1; y =y1; texture = Graphic.texture_from_int 1}::acc)
  |2 -> let x1,y1 = generate_coo acc x y in aux (number-1) ({width = 107; height = 65; x = x1; y =y1; texture = Graphic.texture_from_int 2}::acc)
  |_ -> failwith("C'est super bizarre si ça bug ici")

in aux n []

let generate_floor ()= 
  Graphic.texture_crop_and_resize "./images/tileset.png" 440. 120. 50. 50. 50 50

  let generate_road xchunk ychunk = 
    let text_road = Graphic.texture_crop_and_resize "./images/tileset.png" 0. 64. 28.8 32. 50 50 in 
    let rec aux x1 y1 acc n =
    if n <= 0 then acc
    else 
      let bat : Types.batiments = {width=50; height=50; x=x1 ;y=y1 ; texture = text_road} in 
      match Random.int 4 with
      |1 -> aux (x1+50) (y1+50) (bat::acc) (n-1) @ aux (x1) (y1+50) (bat::acc) (n-1) @ aux (x1+50) (y1) (bat::acc) (n-1)
      |2 -> aux (x1+50) (y1) (bat::acc) (n-1)
      |3 -> aux (x1) (y1+50) (bat::acc) (n-1)
      |_ -> acc
  in 
  (aux (Random.int (4*Graphic.w) -2*Graphic.w + 4*xchunk*Graphic.w) (Random.int (4*Graphic.h) - 2*Graphic.h + 4*ychunk*Graphic.w) [] 15 ) @ (aux (Random.int (4*Graphic.w) -2*Graphic.w + 4*xchunk*Graphic.w) (Random.int (4*Graphic.h) - 2*Graphic.h + 4*ychunk*Graphic.w) [] 15 ) 
  
<<<<<<< HEAD
=======

>>>>>>> 4b5efc3219ae308e152417ef8a7055a0fca5b081
let items = Hashtbl.create 200

let item_from_id (id:int) = Hashtbl.find items id

let use_item_durability (player:Types.player) (c:int) (d:int) =
  match player.inventory.(c) with
  | None -> ()
  | Some (_, x) ->
    match x.durability with
    | None -> ()
    | Some a -> if d > a then ignore (Player.drop_item player x 1 c)
                else x.durability <- Some (a - d) 


let fill_item_table () =
<<<<<<< HEAD
  Hashtbl.add items 1 Types.{name = "Wood log"; image = (Graphic.texture_from_image_name "./images/items/wood_log.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 2 Types.{name = "Pebbles"; image = (Graphic.texture_from_image_name "./images/items/pebbles.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 3 Types.{name = "Stick"; image = (Graphic.texture_from_image_name "./images/items/stick.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 4 Types.{name = "Axe"; image = (Graphic.texture_from_image_name "./images/items/axe.png" 54 54); stackable = false; durability = Some 200; max_durability = Some 200};
  Hashtbl.add items 5 Types.{name = "Pickaxe"; image = (Graphic.texture_from_image_name "./images/items/pickaxe.png" 54 54); stackable = false; durability = Some 300; max_durability = Some 300};
  Hashtbl.add items 6 Types.{name = "Master Sword"; image = (Graphic.texture_from_image_name "./images/items/master_sword.png" 54 54); stackable = false; durability = Some 400; max_durability = Some 400};
  Hashtbl.add items 7 Types.{name = "Legendary Sword"; image = (Graphic.texture_from_image_name "./images/items/legendary_sword.png" 54 54); stackable = false; durability = Some 900; max_durability = Some 900};
  Hashtbl.add items 8 Types.{name = "Rare Sword"; image = (Graphic.texture_from_image_name "./images/items/rare_sword.png" 54 54); stackable = false; durability = Some 200; max_durability = Some 200};
  Hashtbl.add items 9 Types.{name = "Common Sword"; image = (Graphic.texture_from_image_name "./images/items/common_sword.png" 54 54); stackable = false; durability = Some 80; max_durability = Some 80}
=======
  Hashtbl.add items 1 {Types.name = "Wood log"; image = (Graphic.texture_from_image_name "./images/items/wood_log.png" 54 54); stackable = true};
  Hashtbl.add items 2 {name = "Pebbles"; image = (Graphic.texture_from_image_name "./images/items/pebbles.png" 54 54); stackable = true};
  Hashtbl.add items 3 {name = "Stick"; image = (Graphic.texture_from_image_name "./images/items/stick.png" 54 54); stackable = true};
  Hashtbl.add items 4 {name = "Axe"; image = (Graphic.texture_from_image_name "./images/items/axe.png" 54 54); stackable = false};
  Hashtbl.add items 5 {name = "Pickaxe"; image = (Graphic.texture_from_image_name "./images/items/pickaxe.png" 54 54); stackable = false};
  Hashtbl.add items 6 {name = "Master Sword"; image = (Graphic.texture_from_image_name "./images/items/master_sword.png" 54 54); stackable = false};
  Hashtbl.add items 7 {name = "Legendary Sword"; image = (Graphic.texture_from_image_name "./images/items/legendary_sword.png" 54 54); stackable = false};
  Hashtbl.add items 8 {name = "Rare Sword"; image = (Graphic.texture_from_image_name "./images/items/rare_sword.png" 54 54); stackable = false};
  Hashtbl.add items 9 {name = "Common Sword"; image = (Graphic.texture_from_image_name "./images/items/common_sword.png" 54 54); stackable = false};
  Hashtbl.add items 10 {name = "Water Gun"; image = (Graphic.texture_from_image_name "./images/items/water_gun.png" 54 54); stackable = false};
  Hashtbl.add items 11 {name = "Gun 9mm"; image = (Graphic.texture_from_image_name "./images/items/9mm.png" 54 54); stackable = false};
  Hashtbl.add items 12 {name = "AK-47"; image = (Graphic.texture_from_image_name "./images/items/ak47.png" 54 54); stackable = false};
>>>>>>> 4b5efc3219ae308e152417ef8a7055a0fca5b081

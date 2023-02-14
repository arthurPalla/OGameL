open Types

let items = Hashtbl.create 200

let item_from_id (id:int) = Hashtbl.find items id

let fill_item_table () =
  Hashtbl.add items 1 {name = "Wood log"; image = (Graphic.texture_from_image_name "./images/items/wood_log.png" 54 54); stackable = true};
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
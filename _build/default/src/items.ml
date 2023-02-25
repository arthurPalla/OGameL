let items = Hashtbl.create 200

let item_from_id (id:int) =
  let initial = Hashtbl.find items id in
  Types.{name = initial.name; image = initial.image; stackable = initial.stackable; durability = initial.durability; max_durability = initial.max_durability}

let fill_item_table () =
  Hashtbl.add items 1 Types.{name = "Wood log"; image = (Graphic.texture_from_image_name "./images/items/wood_log.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 2 Types.{name = "Pebbles"; image = (Graphic.texture_from_image_name "./images/items/pebbles.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 3 Types.{name = "Stick"; image = (Graphic.texture_from_image_name "./images/items/stick.png" 54 54); stackable = true; durability = None; max_durability = None};
  Hashtbl.add items 4 Types.{name = "Axe"; image = (Graphic.texture_from_image_name "./images/items/axe.png" 54 54); stackable = false; durability = Some 200; max_durability = Some 200};
  Hashtbl.add items 5 Types.{name = "Pickaxe"; image = (Graphic.texture_from_image_name "./images/items/pickaxe.png" 54 54); stackable = false; durability = Some 300; max_durability = Some 300};
  Hashtbl.add items 6 Types.{name = "Master Sword"; image = (Graphic.texture_from_image_name "./images/items/master_sword.png" 54 54); stackable = false; durability = Some 400; max_durability = Some 400};
  Hashtbl.add items 7 Types.{name = "Legendary Sword"; image = (Graphic.texture_from_image_name "./images/items/legendary_sword.png" 54 54); stackable = false; durability = Some 900; max_durability = Some 900};
  Hashtbl.add items 8 Types.{name = "Rare Sword"; image = (Graphic.texture_from_image_name "./images/items/rare_sword.png" 54 54); stackable = false; durability = Some 200; max_durability = Some 200};
  Hashtbl.add items 9 Types.{name = "Common Sword"; image = (Graphic.texture_from_image_name "./images/items/common_sword.png" 54 54); stackable = false; durability = Some 80; max_durability = Some 80}
type 'a cyclic_list = {
  elements: 'a array;
  mutable i: int;
  length:int;
}

type item = {
  name : string;
  image : Raylib.Texture.t;
  stackable : bool;
  mutable durability : int option;
  max_durability : int option
}
type enemy = {
  mutable health: int;
  mutable x: int;
  mutable y:int;
  texture: (Raylib.Texture.t cyclic_list) array; 
  attack_texture :  (Raylib.Texture.t cyclic_list) array;
  mutable direction: int;
  mutable is_attacking: bool;
  mutable sleep_time: int;
  mutable moove_time: int;
}
type batiments =
{
  width:int;
  height: int;
  x:int;
  inside : Raylib.Texture.t option; 
  y:int;
  texture :Raylib.Texture.t;
  bat_type : string
}

type player = { 
  mutable inside_batiment: batiments option;
  mutable health: int;
  mutable feed: int;
  mutable x: int;
  mutable y:int;
  texture: (Raylib.Texture.t cyclic_list) array;  
  mutable direction: int;
  mutable inventory : (int * item) option array;
  mutable is_inventory_open : bool;
  mutable hand : int;
  mutable is_hitting : bool;
  mutable hit_step : int
} 

type map = 
{
  mutable batiment: batiments list;
  mutable enemies: enemy list;
  floor: Raylib.Texture.t;
  mutable roads: batiments list;
  mutable generated: (int*int) list
}

let cyclic_next c_list =
  if(c_list.i >= c_list.length -2) then 
    c_list.i <- 0
  else c_list.i <- c_list.i + 1;
  ()

let cyclic_top c_list = 
  c_list.elements.(c_list.i)  
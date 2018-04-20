open Tsdl;;
open Objet;;
open Array;;
open Music;;

type scene = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  wallpaper : Sdl.texture;
  state : int;(*0 = menu, 1 = game*)
  objets : objet list (* La liste des objets contenu dans la scène *)
};;

(* On déclare ici des listes représentant nos niveaux *)
let niveau1 = [("Sprite/map.bmp", Wallpaper, 0,0);("Sprite/S1.bmp",Perso,100,500);
	       ("Sprite/sol.bmp",Plateforme,0,1047);("Sprite/ewr1.bmp",Ennemi,1400,390);
	       ("Sprite/ewr1.bmp",Ennemi,1600,390);
	       ("Sprite/border_wall.bmp",Mur, 1919,0); ("Sprite/border_wall.bmp",Mur, 0,0);
	       ("Sprite/pilier.bmp",Mur,900,553);("Sprite/plateforme2.bmp",Plateforme,730,900);
	       ("Sprite/plateforme2.bmp",Plateforme,730,700);("Sprite/plateforme2.bmp",Plateforme,1030,900);
	       ("Sprite/plateforme2.bmp",Plateforme,1030,700);("Sprite/plateforme1.bmp",Plateforme,0,400);
	       ("Sprite/ewr1.bmp",Ennemi,10,300);("Sprite/vide.bmp",Plateforme,525,554);
	       ("Sprite/vide.bmp",Plateforme,430,510);("Sprite/vide.bmp",Plateforme,337,555);
	       ("Sprite/tete.bmp",Bonus,920,505)
	      ];;
let menu = [("Sprite/menu.bmp",Wallpaper,0,0)];;

(* On déclare ici des tableaux contenant tous les sprites que l'on va utiliser pour notre personnage *)
let jl = [|"Sprite/JL1.bmp";"Sprite/JL2.bmp";"Sprite/JL3.bmp";"Sprite/JL4.bmp"|];;
let jr = [|"Sprite/JR1.bmp";"Sprite/JR4.bmp";"Sprite/JR6.bmp";"Sprite/JR8.bmp"|];;
let wr = [|"Sprite/WR1.bmp";"Sprite/WR2.bmp";"Sprite/WR3.bmp";"Sprite/WR4.bmp";"Sprite/WR5.bmp";"Sprite/WR6.bmp";"Sprite/WR7.bmp";"Sprite/WR8.bmp";"Sprite/WR9.bmp";"Sprite/WR10.bmp";"Sprite/WR11.bmp";"Sprite/WR12.bmp";"Sprite/WR13.bmp";"Sprite/WR14.bmp";"Sprite/WR15.bmp";"Sprite/WR16.bmp";"Sprite/WR17.bmp"|];;
let wl = [|"Sprite/WL1.bmp";"Sprite/WL2.bmp";"Sprite/WL3.bmp";"Sprite/WL4.bmp";"Sprite/WL5.bmp";"Sprite/WL6.bmp";"Sprite/WL7.bmp";"Sprite/WL8.bmp";"Sprite/WL9.bmp";"Sprite/WL10.bmp";"Sprite/WL11.bmp";"Sprite/WL12.bmp";"Sprite/WL13.bmp";"Sprite/WL14.bmp";"Sprite/WL15.bmp";"Sprite/WL16.bmp";"Sprite/WL17.bmp"|];;
let s = [|"Sprite/S1.bmp";"Sprite/S2.bmp";"Sprite/S3.bmp";"Sprite/S4.bmp";"Sprite/S5.bmp";"Sprite/S6.bmp";"Sprite/S7.bmp";"Sprite/S8.bmp";"Sprite/S9.bmp";"Sprite/S10.bmp";"Sprite/S11.bmp";"Sprite/S12.bmp";"Sprite/S13.bmp";"Sprite/S14.bmp";"Sprite/S15.bmp";"Sprite/S16.bmp";"Sprite/S17.bmp";"Sprite/S18.bmp";"Sprite/S19.bmp";"Sprite/S20.bmp";"Sprite/S21.bmp";"Sprite/S22.bmp";"Sprite/S23.bmp";"Sprite/S24.bmp";"Sprite/S25.bmp";"Sprite/S26.bmp";"Sprite/S27.bmp";"Sprite/S28.bmp";"Sprite/S29.bmp";"Sprite/S30.bmp";"Sprite/S31.bmp";"Sprite/S32.bmp";"Sprite/S33.bmp";"Sprite/S34.bmp";"Sprite/S35.bmp";"Sprite/S36.bmp";"Sprite/S37.bmp";"Sprite/S36.bmp";"Sprite/S35.bmp";"Sprite/S34.bmp";"Sprite/S33.bmp";"Sprite/S32.bmp";"Sprite/S31.bmp";"Sprite/S30.bmp";"Sprite/S29.bmp";"Sprite/S28.bmp";"Sprite/S27.bmp";"Sprite/S26.bmp";"Sprite/S25.bmp";"Sprite/S24.bmp";"Sprite/S23.bmp";"Sprite/S22.bmp";"Sprite/S21.bmp";"Sprite/S20.bmp";"Sprite/S19.bmp";"Sprite/S18.bmp";"Sprite/S17.bmp";"Sprite/S16.bmp";"Sprite/S15.bmp";"Sprite/S14.bmp";"Sprite/S13.bmp";"Sprite/S12.bmp";"Sprite/S11.bmp";"Sprite/S10.bmp";"Sprite/S9.bmp";"Sprite/S8.bmp";"Sprite/S7.bmp";"Sprite/S6.bmp";"Sprite/S5.bmp";"Sprite/S4.bmp";"Sprite/S3.bmp";"Sprite/S2.bmp";"Sprite/S1.bmp"|];;
let m = [|"Sprite/MJR.bmp";"Sprite/MJL.bmp";"Sprite/MR.bmp";"Sprite/ML.bmp"|];;
let ewl=[|"Sprite/ewl1.bmp";"Sprite/ewl2.bmp"|];;
let ewr=[|"Sprite/ewr1.bmp";"Sprite/ewr2.bmp"|];;
let inv_sprite = "Sprite/vide.bmp";;
 

(* Initialise la sdl *)
let init () =
  match Sdl.init Sdl.Init.video with
  |Error (`Msg e) -> Sdl.log "Init error : %s" e; exit 1
  |Ok () -> ();;

(* Crée une fenetre de taille w*h avec title pour nom *)
let create_window w h title =
  match Sdl.create_window ~w:w ~h:h title Sdl.Window.opengl with
  |Error (`Msg e) -> Sdl.log "Create window error : %s" e; exit 1
  |Ok w -> w;;

(* Crée un renderer avec les options index et flags dans la fenetre w *)
let create_renderer index flags w =
  match Sdl.create_renderer ~index:(index) ~flags:flags w  with
  |Error (`Msg e) -> Sdl.log "Create renderer error : %s" e; exit 1
  |Ok r -> r;;

(* Charge une image depuis son chemin, renvoie une surface *)
let load_img path =
  match Sdl.load_bmp path with
  |Error (`Msg e) -> Sdl.log "Load image error : %s" e; exit 1
  |Ok s -> s;;

(* Crée une texture à partir d'une surface et d'un renderer *)
let create_texture_from_surface r s  =
  match Sdl.create_texture_from_surface r s with
  |Error (`Msg e) -> Sdl.log "Create texture error : %s" e; exit 1
  |Ok t -> Sdl.free_surface s; t;;


(* Charge une texture directement depuis un nom de fichier *)
let img_to_texture r path = 
  let s = load_img path in
  create_texture_from_surface r s
;;

(* Fonction qui charge tous les tableaux d'animation du personnage *)
let load_tables renderer =
  let left = Array.init (length wl) (fun i -> img_to_texture renderer wl.(i)) in
  let right = Array.init (length wr) (fun i -> img_to_texture renderer wr.(i)) in
  let jump_left = Array.init (length jl) (fun i -> img_to_texture renderer jl.(i)) in
  let jump_right = Array.init (length jr) (fun i -> img_to_texture renderer jr.(i)) in
  let stand = Array.init (length s) (fun i -> img_to_texture renderer s.(i)) in
  let missile = Array.init (length m) (fun i -> img_to_texture renderer m.(i)) in
  (left,right, jump_left, jump_right, stand, missile)
;;

let load_tables_ennemi renderer =
  let eleft = Array.init (length ewl) (fun i -> img_to_texture renderer ewl.(i)) in
  let eright = Array.init (length ewr) (fun i -> img_to_texture renderer ewr.(i)) in
  (eleft,eright)
;;

(* Prend une liste de (string * Objet.kind * int * int * int) qui représente un niveau et renvoie une liste contenant les objets correspondants
avec leur texture, leur type, leur position de départ et leur niveau
On match le type de chaque objet pour appeler la fonction de création correspondante *)
let text_list_to_object_list l r =
  let rec aux l acc =
    match l with
    |[] -> acc
    |(path, kind, x, y)::tl ->
       let surface = load_img path in
       let texture = create_texture_from_surface r surface in
       match Sdl.query_texture texture with
       |Error (`Msg e) -> Sdl.log "Create query texture error : %s" e; exit 1
       |Ok (_,_,(w,h)) ->
	  let (left, right, jump_left, jump_right, stand, missile) = load_tables r in
	  let (eleft, eright) = load_tables_ennemi r in
	  match kind with
	  |Perso -> let o = creer_perso x y w h texture left right jump_left jump_right stand missile in let acc = o::acc in aux tl acc
	  |Plateforme -> let o = creer_plateforme x y w h texture in let acc = o::acc in aux tl acc
	  |Mur -> let o = creer_mur x y w h texture in let acc = o::acc in aux tl acc
	  |Ennemi -> let o = creer_ennemi x y w h texture eleft eright in let acc = o::acc in aux tl acc
	  |Bonus -> let o = creer_bonus x y w h texture in let acc = o::acc in aux tl acc
	  |Wallpaper | Missile -> aux tl acc
  in aux l []
;;

(* Fonction qui prend en argument une scène et un objet et affiche cet objet dans le renderer associé à la scène *)
let display scene o =
  match Sdl.query_texture o.texture with
  |Error (`Msg e) -> Sdl.log "Create query texture error : %s" e; exit 1
  |Ok (_,_,(w,h)) -> let destrect = Sdl.Rect.create o.pos.x o.pos.y o.w o.h in
		     Sdl.render_copy ~dst:destrect scene.renderer o.texture; 
;;		     

(* Fonction qui applique display à tous les objets  de la scène *)
let display_list scene =
  Sdl.delay 17l;
  Sdl.render_clear scene.renderer;
  Sdl.render_copy scene.renderer scene.wallpaper;
  let rec aux l =
    match l with
    |[] -> ()    
    |h::t -> match h.kind with
      |Perso -> let pvperso = h.pv in
		let pv = Sdl.Rect.create 49 49 152 22 in
		Sdl.set_render_draw_color scene.renderer 80 80 80 128;
		Sdl.render_fill_rect scene.renderer (Some pv);
		let rect = Sdl.Rect.create 50 50 (30 * pvperso) 20 in
		Sdl.set_render_draw_color scene.renderer 255 255 0 128;
		Sdl.render_fill_rect scene.renderer (Some rect);
        display scene h;
        aux t;
      |_ -> display scene h;
    aux t
  in aux scene.objets
;;

(* Fonction de création d'une scène
Crée une scène à partir d'une fenetre w, d'un renderer r, d'une liste de (string**Objet.kind*int*int*int) lvl et d'un entier state représentant l'attribut state *)
let create_scene w r lvl state =
  {window = w;
   renderer = r;
   wallpaper = (match (List.hd lvl) with
   |(s,_,_,_) -> img_to_texture r s);
   state = state;
   objets = text_list_to_object_list lvl r
  }
;;

(* Fonction qui créé la scène correspondant au menu *)
let load_menu s =
  create_scene s.window s.renderer menu 0
;;

(* Fonction qui prend en argument une scène s et renvoie le personnage contenu dans s *)
let get_perso s =
  let rec aux l =
    match l with
    |[] -> Printf.printf "Erreur"; raise Erreur
    |h::t -> begin
      match h.kind with
      |Perso -> h
      |_ -> aux t
    end
  in aux s.objets
;;

(* Fonction qui prend une scène s et un objet p et renvoie une scène égale à s dont le personnage a été remplacé par p *)
let update_perso s p =
  let oldp = get_perso s in
  {s with objets = p::(remove_objet s.objets oldp)}
;;

(* Fonction qui prend une scène s et renvoie une scène égale à s dans laquelle on a rajouté un missile qu'on a créé à gauche du personnage *)
let creer_missile_left s =
  let p = get_perso s in
  let newproj = creer_missile_left_obj s (img_to_texture s.renderer "Sprite/couteau2.bmp") p in
  let p = {(get_perso s) with timer_missile = 1} in
  let l = (remove_objet s.objets (get_perso s))@[newproj]@[p] in
  {s with objets = l}
;;

(* Meme fonction que précédemment mais pour la droite *)
let creer_missile_right s =
  let p = get_perso s in
  let newproj = creer_missile_right_obj s (img_to_texture s.renderer "Sprite/couteau1.bmp") p in
  let p = {(get_perso s) with timer_missile = 1} in
  let l = (remove_objet s.objets (get_perso s))@[newproj]@[p] in
  {s with objets = l}
;;

(* Fonciton qui prend une scène s et un entier m et renvoie une scène dans laquelle on a rajouté un missile en fonction de la valeur de m *)
let creer_missiles s m =
  let p = get_perso s in
  if p.timer_missile <> 0 || m = 0 then s
  else
    begin
      play_effet "Son/tir.wav";
      if m > 0 then
    creer_missile_right s
      else creer_missile_left s
    end
;;

(* Fonction qui prend en argument un objet o, teste si c'est un ennemi et renvoie un objet avec une nouvelle texture en fonction de son animation si oui,
renvoie o sinon *)
let anime_ennemy e =
  if e.kind = Ennemi then
    begin
      if e.speed.vx <= 0 then 
	{e with texture = e.wl.(e.frame/12); frame = (e.frame mod 23) +1 }
      else
	{e with texture = e.wr.(e.frame/12); frame = (e.frame mod 23) +1 }
    end
  else
    e
;;


(* Fonction qui prend une liste d'objets l et renvoie une liste égale à l où tous les objets on été déplacé 
Si un objet sort de la fenetre il est supprimé plutot que déplacé *)
let move_objet_list l =
  let rec aux l acc =
    match l with
    |[] -> acc
    |h::t -> if is_in_window h then
	begin
	  let acc = (move_objet (anime_ennemy h))::acc in
	  aux t acc
	end
      else let acc = remove_objet acc h in
	   aux t acc
  in aux l []
;;

(* Fonction qui prend en argument deux objets p et o et renvoie un objet p correspondant au résultat de la collision entre p et o 
On appele cette fonction uniquement si p est un objet de type Personnage *)
let collision_perso p o =
  match o.kind with
  |Missile | Wallpaper | Perso -> p
  |Ennemi ->
     if p.timer_collision = 0 then
       begin
	 play_effet "Son/degat.wav";
	 if collision_down p o then
           {p with pv = p.pv-1;jumping = false; timer_collision = 1}
	 else
	   {p with  pv = p.pv-1; timer_collision = 1}
       end
     else
       p
  |Bonus ->
      if p.timer_collision = 0 then
       begin
	 play_effet "Son/bonus.wav";
         {p with timer_collision = -140}
       end
     else
       p
  |Plateforme ->
     begin
       if collision_down p o then
	 {p with pos = {p.pos with y = o.pos.y-p.h}; speed = {vx = base_speed_x; vy = 0}; jumping = false}
       else p
     end
  |Mur -> 
     begin
       if collision_right p o then
	 {p with pos = {p.pos with x = o.pos.x-p.w}; speed = {vx = base_speed_x; vy = p.speed.vy}}
       else if collision_left p o then 
	 {p with pos = {p.pos with x = o.pos.x+o.w}; speed = {vx = base_speed_x; vy = p.speed.vy}}
       else if collision_up p o then
	 {p with pos = {p.pos with y = o.pos.y+o.h}; speed = {vx = base_speed_x; vy = p.speed.vy}}
       else
	 {p with pos = {p.pos with y = o.pos.y-p.h}; speed = {vx = base_speed_x; vy = 0}; jumping = false; frame = 0}
     end    
;;

(* Meme fonction que la précédente mais cette fois ci le premier objet est un ennemi *)
let collision_ennemi e o =
  match o.kind with
  |Perso| Ennemi | Wallpaper | Bonus -> e
  |Missile -> {e with pv = e.pv-1}
  |Mur | Plateforme ->
    begin
      if collision_right e o then
	{e with pos = {e.pos with x = o.pos.x-e.w}; speed = {vx = (-e.speed.vx); vy = base_speed_y}}
      else if collision_left e o then 
	{e with pos = {e.pos with x = o.pos.x+o.w}; speed = {vx = (-e.speed.vx); vy = base_speed_y}}
      else if collision_up e o then
	{e with pos = {e.pos with y = o.pos.y+o.h}; speed = {vx = e.speed.vx; vy = base_speed_y}}
      else {e with pos = {e.pos with y = o.pos.y-e.h}; speed = {vx = e.speed.vx; vy = 0}}
    end
;;

(* Toujours la meme fonction mais pour les missiles *)
let collision_missile m o =
  match o.kind with
  |Perso | Missile | Wallpaper | Bonus -> m
  |Ennemi | Mur | Plateforme -> {m with pv = 0}
;;

(* Toujours la meme fonction mais pour les missiles *)
let collision_bonus b o =
  match o.kind with
  |Perso  -> {b with pv = 0}
  |Ennemi | Mur | Plateforme | Missile | Wallpaper | Bonus -> o
;;


(* Fonction qui prend deux objets o1 et o2 en argument et renvoie un objet égale au résultat de la collision entre o1 et o2 en fonction du type de o1 *)
let collision o1 o2 =
  if check_collision o1 o2 then
    begin
      match o1.kind with
      |Perso -> collision_perso o1 o2
      |Ennemi -> collision_ennemi o1 o2
      |Missile -> collision_missile o1 o2
      |Bonus -> collision_bonus o1 o2
      |Plateforme | Mur | Wallpaper -> o1
    end
  else o1
;;

(* Fonction qui prend une scène s et itère sur s pour gérer toutes les collisions *)
let check_collision_all s =
  {s with objets = List.map (fun o -> (List.fold_left collision o (remove_objet s.objets o))) s.objets}
;;

(* Fonction qui prend une scene et renvoie une scène où tous les objets ont été déplacés et les collisions gérées *)
let moveall scene =
  let s = {scene with objets = move_objet_list scene.objets} in
  check_collision_all  s
;;

(* Fonction qui prend une scene, un personnage, une texture et un entier,
renvoie la texture correspondante à l'état du personnage passé en argument *)
let check_inv scene perso text missile =
  if perso.timer_collision <> 0 && perso.timer_collision mod 3 = 0 then
    (img_to_texture scene.renderer inv_sprite)
  else if perso.timer_missile >= 1 then
    begin
      if perso.jumping then
	if missile>0 then perso.m.(0)
	else if missile<0 then perso.m.(1)
	else text
      else
	if missile>0 then perso.m.(2)
	else if missile<0 then perso.m.(3)
	else text
    end
  else text
;;
  
(* Fonction qui prend une scene, le personnage, un (int*int) et un entier et renvoie la texture correspondant au bon moment de l'animation du personnage *)    
let check_texture scene perso (vx, vy) missile =
  if perso.jumping then
    begin
      if vx >= 0 then
	(check_inv scene perso perso.jr.(perso.frame/timer_saut) missile, (if (perso.frame/timer_saut) < 3 then perso.frame+1 else 15), repos_jump)
      else (check_inv scene perso perso.jl.(perso.frame/timer_saut) missile,(if (perso.frame/timer_saut) < 3 then perso.frame+1 else 15) ,repos_jump)
    end
  else if vy < 0 then
    begin
      if vx >= 0 then
	(check_inv scene perso perso.jr.(0) missile, 0 , 30)
      else
	(check_inv scene perso perso.jl.(0) missile, 0, repos_jump)
    end
  else
    begin
      if vx > 0 then
	begin
	  if perso.repos <> 0 then 
	    (check_inv scene perso perso.wr.(perso.frame/timer_marche) missile, (perso.frame mod 50)+1, repos_walk)
	  else
	    (check_inv scene perso perso.wr.(0) missile, 0, repos_walk)
	end
      else if vx < 0 then
	begin
	  if perso.repos <> 0 then
	    (check_inv scene perso perso.wl.(perso.frame/timer_marche) missile, (perso.frame mod 50)+1, repos_walk)
	  else
	    (check_inv scene perso perso.wl.(0) missile, 0, repos_walk)
	end
      else if perso.repos > 0 then
	(check_inv scene perso perso.texture missile, 0, perso.repos-1)
      else
	(check_inv scene perso perso.s.(perso.frame/timer_repos) missile,(perso.frame mod 74)+1 , 0)
    end
;;

(* Fonction qui prend une scène, un (int*int) et un entier et renvoie une scène 
où la vitesse du personnage à été mise à jour en fonction des entiers correspondant aux entrées clavier *)
let move_perso s (vx,vy) m =
  let p = get_perso s in
  let (text, frame, timer) = check_texture s p (vx, vy) m in
  if vy < 0 then
    begin
      if p.jumping then
	let newp = {p with speed = {vx = vx; vy = p.speed.vy}; texture = text; frame = frame; repos = timer} in
	match newp.timer_collision, newp.timer_missile with
	|0,0 -> update_perso s newp
	|70, 10 -> let newp = {newp with timer_collision = 0; timer_missile = 0} in update_perso s newp
	|0, 10 -> let newp = {newp with timer_missile = 0} in update_perso s newp
	|70, 0 -> let newp = {newp with timer_collision = 0} in update_perso s newp
	|0, _ -> let newp = {newp with timer_missile = newp.timer_missile + 1} in update_perso s newp
	|_, 0 -> let newp = {newp with timer_collision = newp.timer_collision + 1} in update_perso s newp
	|70, _ -> let newp = {newp with timer_collision = 0; timer_missile = newp.timer_missile + 1} in update_perso s newp
	|_, 10 -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = 0} in update_perso s newp
	|_,_ -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = newp.timer_missile + 1} in update_perso s newp
      else
	let newp = {p with speed = {vx = vx; vy = vy}; jumping = true; texture = text; frame = frame; repos = timer} in
	match newp.timer_collision, newp.timer_missile with
	|0,0 -> update_perso s newp
	|70, 10 -> let newp = {newp with timer_collision = 0; timer_missile = 0} in update_perso s newp
	|0, 10 -> let newp = {newp with timer_missile = 0} in update_perso s newp
	|70, 0 -> let newp = {newp with timer_collision = 0} in update_perso s newp
	|0, _ -> let newp = {newp with timer_missile = newp.timer_missile + 1} in update_perso s newp
	|_, 0 -> let newp = {newp with timer_collision = newp.timer_collision + 1} in update_perso s newp
	|70, _ -> let newp = {newp with timer_collision = 0; timer_missile = newp.timer_missile + 1} in update_perso s newp
	|_, 10 -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = 0} in update_perso s newp
	|_,_ -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = newp.timer_missile + 1} in update_perso s newp
    end
  else
    begin
      let newp = {p with speed = {vx = vx; vy = p.speed.vy}; texture = text; frame = frame; repos = timer} in
      match newp.timer_collision, newp.timer_missile with
      |0,0 -> update_perso s newp
      |70, 10 -> let newp = {newp with timer_collision = 0; timer_missile = 0} in update_perso s newp
      |0, 10 -> let newp = {newp with timer_missile = 0} in update_perso s newp
      |70, 0 -> let newp = {newp with timer_collision = 0} in update_perso s newp
      |0, _ -> let newp = {newp with timer_missile = newp.timer_missile + 1} in update_perso s newp
      |_, 0 -> let newp = {newp with timer_collision = newp.timer_collision + 1} in update_perso s newp
      |70, _ -> let newp = {newp with timer_collision = 0; timer_missile = newp.timer_missile + 1} in update_perso s newp
      |_, 10 -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = 0} in update_perso s newp
      |_,_ -> let newp = {newp with timer_collision = newp.timer_collision + 1; timer_missile = newp.timer_missile + 1} in update_perso s newp
    end
;;

(* Fonction qui prend en argument une scène et renvoie true si la scène ne contient pas de personnage, false sinon *)
let is_perso_dead s =
  let rec aux l =
    match l with
    |[] -> true
    |h::t -> match h.kind with
      |Perso -> false
      |_ -> aux t
  in aux s.objets
;;

(* Fonction qui prend en argument une scène et renvoie une scène où la liste d'objets ne contient plus les objets avec zéro point de vie *)
let gestion_pv s =
  {s with objets = remove_if_dead s.objets}
;;

(* Fonction qui prend une scène en argument et quitte l'application, en détruisant toutes les textures *)
let quit s =
  let rec aux l =
    match l with
    |[] -> ()
    |h::t -> Sdl.destroy_texture h.texture;
      aux t
  in aux s.objets;
  Sdl.destroy_renderer s.renderer;
  Sdl.destroy_window s.window;
  Sdl.quit ();
  exit 0
  
;;

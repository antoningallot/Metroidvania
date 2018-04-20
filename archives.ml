(*Créé le personnage à partir du nom de son fichier*)
let creer_perso r path =
  let surface = load_img path in
  let texture = create_texture_from_surface r surface in
  match Sdl.query_texture texture with
  |Error (`Msg e) -> Sdl.log "Create query texture error : %s" e; exit 1
  |Ok (_,_,(w,h)) -> creer_objet  base_speed_x base_speed_y "perso" w h texture false 0 5;;
  
(*Sprite Dio : x=0 y=380 h=40 w=100*)

let moveall_list_missile l s= 
  let rec aux l acc =
    match l with
    |[] -> acc
    |h::t ->
       if (collisionRight h s.plateformes || collisionLeft h s.plateformes || collisionUp h s.plateformes || collisionDown h s.plateformes) then
	 aux t acc 
       else if (collisionRight h s.ennemis || collisionLeft h s.ennemis || collisionUp h s.ennemis || collisionDown h s.ennemis) then
	 
	 let acc = ({h with pos = {x=h.pos.x+h.speed.vx;y=h.pos.y}})::acc in
	 aux t acc in
  aux l []
;;
*)


let moveall s =
  if is_in_window s.perso then
    begin
      display_list {s with perso = move_perso s.perso s.plateformes s.ennemis;
	ennemis = moveall_list s.ennemis s.plateformes} s.plateformes s.ennemis;
      Sdl.render_present s.renderer;
      {s with perso = move_perso s.perso s.plateformes s.ennemis;
	ennemis = moveall_list s.ennemis s.plateformes}
    end
  else
    begin
      if s.perso.pos.x+s.perso.l > 1920 then begin (*Printf.printf "Change droite\n";*) change_niveau s (Some Right) end
      else change_niveau s (Some Left)
    end
;;

let moveall scene =
  let newscene = 
    let rec aux_missiles s l =
      match l with
      |[] -> s
      |h::t ->
	 begin
	   if collisionRight h s.ennemis || collisionLeft h s.ennemis ||
	     collisionUp h s.ennemis || collisionDown h s.ennemis then
	     let o = who_collision h s.ennemis in
	     aux_missiles {s with ennemis = ((remove_objet o s.ennemis)@ [{o with pv = o.pv-1}]);
	       missiles = remove_objet h s.missiles} t
	   else if collisionRight h s.plateformes || collisionLeft h s.plateformes ||
	       collisionUp h s.plateformes || collisionDown h s.plateformes then
	     aux_missiles {s with missiles = remove_objet h s.missiles} t
	   else aux_missiles {s with missiles = move_objet_list h s.missiles} t
	 end
    in
    aux_missiles scene scene.missiles in
  let newscene2 =
    let rec aux_ennemis s l =  
      match l with
      |[] -> s
      |h::t ->
	 begin
	   if collisionRight h s.missiles || collisionLeft h s.missiles ||
	     collisionUp h s.missiles || collisionDown h s.missiles then
	     let o = who_collision h s.missiles in
	     aux_ennemis {s with ennemis = ((remove_objet h s.ennemis) @[{h with pv = h.pv - 1}]);
	       missiles = remove_objet o s.missiles} t 
	   else if collisionRight h s.plateformes || collisionLeft h s.plateformes ||
	       collisionUp h s.plateformes then
	     aux_ennemis {s with ennemis = ((remove_objet h s.ennemis)@[{h with speed = {vx = base_speed_x; vy = base_speed_y}}])} t
	   else if collisionDown h s.plateformes then
	     aux_ennemis {s with ennemis = ((remove_objet h s.ennemis)@[{h with speed = {vx = base_speed_x; vy = 0}}])} t
	   else aux_ennemis {s with ennemis = ((remove_objet h s.ennemis) @ [move_objet h])} t
	 end
    in aux_ennemis newscene newscene.ennemis in
  let newscene3 = {newscene2 with perso = (move_perso newscene2.perso newscene2.plateformes newscene2.ennemis)} in
  display_list newscene3 newscene3.plateformes newscene3.ennemis newscene3.missiles;
  Sdl.render_present newscene3.renderer;
  newscene3
;;

let move_perso o plateformes ennemis =
  if collision_right o plateformes || collision_left o plateformes || collision_up o plateformes then
    begin
      (*Format.eprintf "UpLR@.";*)
      {o with speed = {vx = base_speed_x; vy = base_speed_y}}
    end
  else if collision_down o plateformes then
    begin
      (*Format.eprintf "Down@.";*)
      {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y};
	speed = {vx = base_speed_x; vy = 0}; jumping = false}
    end
  else if collision_right o ennemis then
      {o with speed = {vx = -10; vy = base_speed_y}; pv = o.pv-1}

  else if collision_left o ennemis then
    {o with speed = {vx = 10; vy = base_speed_y}; pv = o.pv-1}
      
  else if collision_up o ennemis then
    {o with speed = {vx = base_speed_x; vy = 10}; pv = o.pv-1}
	
  else if collision_down o ennemis then
    {o with speed = {vx = 5; vy = -10}; pv = o.pv-1}

  else
    if o.jumping then
      {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y+o.speed.vy};
	speed = {o.speed with vy = base_speed_y}}
    else
      {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y+o.speed.vy};
	speed = {vx = base_speed_x; vy = base_speed_y}}
;;  

let move_ennemi o plateformes ennemis =
  if collision_right o plateformes || collision_left o plateformes || collision_up o plateformes then
    {o with speed = {vx = base_speed_x; vy = base_speed_y}}
      
  else if collision_down o plateformes then
    {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y};
      speed = {vx = base_speed_x; vy = 0}; jumping = false}
      
  else if collision_right o ennemis || collision_left o ennemis || collision_up o ennemis then
    {o with speed = {vx = base_speed_x; vy = base_speed_y}}
      
  else if collision_down o ennemis then
    {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y};
      speed = {vx = base_speed_x; vy = 0};}
      
  else
    {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y+o.speed.vy};
      speed = {vx = base_speed_x; vy = base_speed_y}}
;;  

let collision_perso s =
  let newscene =
    let rec aux_plat l s =
      match l with
      |[] -> s
      |h::t -> if check_collision h s.perso then
	  begin
	    (*Printf.printf "collision\n";*)
	    if collision_right s.perso h then
	      aux_plat t {s with perso = {s.perso with pos = {s.perso.pos with x = h.pos.x-s.perso.l}; speed = {vx = base_speed_x; vy = base_speed_y}}}
	    else if collision_left s.perso h then 
	      aux_plat t {s with perso = {s.perso with pos = {s.perso.pos with x = h.pos.x+h.l}; speed = {vx = base_speed_x; vy = base_speed_y}}}
	    else if collision_up s.perso h then
	      aux_plat t {s with perso = {s.perso with pos = {s.perso.pos with y = h.pos.y+h.h}; speed = {vx = base_speed_x; vy = base_speed_y}}}
	    else begin
	      (*Printf.printf "%d\n" s.perso.pos.y;
		Printf.printf "%d\n" (h.pos.y-s.perso.h);*)
	      aux_plat t {s with perso = {s.perso with pos = {s.perso.pos with y = h.pos.y-s.perso.h}; speed = {vx = base_speed_x; vy = 0}; jumping = false}} end
	  end
	else aux_plat t s
    in aux_plat s.plateformes s
  in
  let rec aux_enn l s =
    match l with
    |[] -> s
    |h::t -> if check_collision h s.perso then
	begin
	       (*Printf.printf "collision\n";*)
	  if collision_right s.perso h then
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else if collision_left s.perso h then 
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else if collision_up s.perso h then
	    aux_enn t {s with perso = {s.perso with pos = oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else begin
		 (*Printf.printf "%d\n" s.perso.pos.y;
		   Printf.printf "%d\n" (h.pos.y-s.perso.h);*)
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}} end
	end
      else aux_enn t s
  in aux_enn newscene.ennemis newscene	    
;;

let check_collision_perso s =
  if collision_right s.perso s.plateformes || collision_left s.perso s.plateformes || collision_up s.perso s.plateformes then
    {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
  else if collision_down s.perso s.plateformes then
    {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = 0}}}
  else s
  ;;
let collision_perso_all s =
  let newscene =
    let rec aux_plat l s =
      match l with
      |[] -> s
      |h::t -> let p = collision_perso 
    in aux_plat s.plateformes s
  in
  let rec aux_enn l s =
    match l with
    |[] -> s
    |h::t -> if check_collision h s.perso then
	begin
	       (*Printf.printf "collision\n";*)
	  if collision_right s.perso h then
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else if collision_left s.perso h then 
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else if collision_up s.perso h then
	    aux_enn t {s with perso = {s.perso with pos = oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}}
	  else begin
		 (*Printf.printf "%d\n" s.perso.pos.y;
		   Printf.printf "%d\n" (h.pos.y-s.perso.h);*)
	    aux_enn t {s with perso = {s.perso with pos = s.perso.oldpos; speed = {vx = base_speed_x; vy = base_speed_y}}} end
	end
      else aux_enn t s
  in aux_enn newscene.ennemis newscene	    
;;


let movement s =
  (*Printf.printf "niv : %d, y : %d\n" s.perso.niv s.perso.pos.y;*)
  Printf.printf "pv : %d\n"  s.perso.pv;
  if s.perso.pv <= 0 then create_scene s.window s.renderer menu 0 [] text_perso
  else if s.perso.jumping then begin (*Printf.printf "Mouvement jump\n";*) moveall s end
  else
    begin
      (*Printf.printf "Mouvement sans jump\n";*)
      let t = Sdl.get_keyboard_state () in
      (*print_int t.{Sdl.get_scancode_from_key Sdl.K.right}; Printf.printf "\n";*)
      if t.{Sdl.get_scancode_from_key Sdl.K.escape} = 1 then quit s
      else let h = if t.{Sdl.get_scancode_from_key Sdl.K.right} = 1 then Some Right
	else if t.{Sdl.get_scancode_from_key Sdl.K.left} = 1 then Some Left 
	else None in
	   let v = if t.{Sdl.get_scancode_from_key Sdl.K.up} = 1 || t.{Sdl.get_scancode_from_key Sdl.K.space} = 1 then Some Jump
	     else if t.{Sdl.get_scancode_from_key Sdl.K.down} = 1 then Some Down
	     else None in
	   match h, v with
	   |None, Some u -> if u = Jump then moveall(up_perso s) else moveall(down_perso s);
	   |Some r, None -> if r = Right then moveall (right_perso s) else moveall (left_perso s);
	   |Some r, Some u ->
	      begin
		if r = Right && u = Jump then moveall(up_perso(right_perso s))
		else if r = Right && u = Down then moveall(right_perso (down_perso s))
		else if r = Left && u = Jump then moveall(up_perso(left_perso s))
		else moveall(left_perso (down_perso s))
	      end
	   |None, None -> moveall s
    end
  ;;
let event_handler e s =
  match Sdl.poll_event (Some e) with
  |true-> 
     begin
       match Sdl.Event.enum(Sdl.Event.get e Sdl.Event.typ) with
       |`Window_event ->
	  begin
	    let n = Sdl.Event.window_event_enum(Sdl.Event.get e Sdl.Event.window_event_id) in
	    match n with
	    |`Close -> quit s
	    |_-> moveall s;
	  end
       |_ -> s;
     end
  |false->  moveall s
;;

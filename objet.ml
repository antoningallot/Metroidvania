open Tsdl;;
open Music;;

type kind =
  |Perso
  |Ennemi
  |Plateforme
  |Mur
  |Missile
  |Wallpaper
      
type position = {x : int; y : int};;

type vitesse = {vx : int; vy : int};;

type objet = {pos : position;
	      oldpos : position;(* La position de l'objet à l'itération précédente *) 
	      speed : vitesse;
	      kind : kind; (* Le type de l'objet *)
	      w : int; (* La largeur de l'objet *)
	      h : int; (* La hauteur de l'objet *)
	      texture: Sdl.texture;
	      jumping : bool; (*booléen qui indique si l'objet est en train de sauter *)
	      pv : int; (* Les points de vie de l'objet *)
	      timer_collision : int; (* Le timer qui déclenche les frames d'invincibilité du personnage après une collision avec un ennemi *)
	      timer_missile : int; (* Le timer qui empeche le personnage de tirer trop de missiles dans un court laps de temps *)
	      frame : int; (* Le compteur des frames du personnage qui sert d'indice pour accéder à la texture dans le tableau de texture du personnage *)
	      repos : int; (* Le timer qui déclenche l'animation d'idle si aucune entrée clavier n'est detecté pendant un certain lap de temps *)
	      wl : Sdl.texture array; (* Le tableau de sprites pour l'animation de marche vers la gauche *)
	      wr : Sdl.texture array; (* Le tableau de sprites pour l'animation de marche vers la droite *)
	      jl : Sdl.texture array; (* Le tableau de sprites pour l'animation de saut vers la gauche *)
	      jr : Sdl.texture array; (* Le tableau de sprites pour l'animation de saut vers la droite *)
	      s : Sdl.texture array; (* Le tableau de sprites pour l'animation idle *)
	      m : Sdl.texture array (* La tableau de sprites pour l'animation de tir *)
	     };;

exception Erreur;;

(* On déclare ici différents entiers qui servent à définir les valeurs du jeu (vitesse, timers, taille de la fenetre...)*)
let base_speed_x = 0;;
let base_speed_y = 1;;
let base_speed_x_ennemis = 2;;
let timer_marche = 3;;
let timer_saut = 5;;
let timer_repos = 5;;
let repos_walk = 5;;
let repos_jump = 5;;
let window_w = 1920;;
let window_h = 1080;;

(* Fonction qui créé un objet personnage *)
let creer_perso x y width height text wl wr jl jr s m=
  let pos = {x=x;y=y} in
  let speed = {vx = base_speed_x; vy = base_speed_y} in
  {pos = pos; oldpos= pos; speed=speed; kind=Perso; w=width; h=height; texture=text; jumping=false; pv = 5; timer_collision = 0; timer_missile = 0; frame = 0; repos = 35; wl = wl; wr = wr; jl = jl; jr = jr; s = s; m = m}
;;

(* Fonction qui créé un objet ennemi *)
let creer_ennemi x y width height text left right =
  let pos = {x=x;y=y} in
  let speed = {vx = base_speed_x_ennemis; vy = base_speed_y} in
  {pos = pos; oldpos= pos; speed=speed; kind=Ennemi; w=width; h=height; texture=text; jumping=false; pv = 5; timer_collision = 0; timer_missile = 0; frame = 0; repos = 0; wl = left; wr = right; jl =[||]; jr = [||]; s = [||]; m = [||] }
;;

(* Fonction qui créé un objet plateforme *)
let creer_plateforme x y width height text =
  let pos = {x=x;y=y} in
  let speed = {vx = 0; vy = 0} in
  {pos = pos; oldpos= pos; speed=speed; kind=Plateforme; w=width; h=height; texture=text; jumping=false; pv = 1; timer_collision = 0; timer_missile = 0; frame = 0; repos = 0; wl =[||]; wr = [||]; jl =[||]; jr = [||]; s = [||]; m = [||] }
;;

(* Fonction qui créé un objet mur *)
let creer_mur x y width height text =
  let pos = {x=x;y=y} in
  let speed = {vx = 0; vy = 0} in
  {pos = pos; oldpos= pos; speed=speed; kind=Mur; w=width; h=height; texture=text; jumping=false; pv = 1; timer_collision = 0; timer_missile = 0; frame = 0; repos = 0; wl =[||]; wr = [||]; jl =[||]; jr = [||];  s = [||]; m = [||] }
;;

(* Fonction qui créé un objet missile à droite du personnage *)
let creer_missile_right_obj s text p =
  let pos = {x=p.pos.x+p.w+7; y=p.pos.y+22} in
  {pos= pos; oldpos = pos; speed={vx=10; vy=0}; kind = Missile; w=25; h=10; texture = text;
   jumping = false; pv = 1; timer_collision = 0; timer_missile = 0; frame = 0; repos = 0; wl = [||]; wr = [||]; jl =[||]; jr = [||];  s = [||]; m = [||] }
;;

(* Fonction qui créé un objet missile à gauche du personnage *)
let creer_missile_left_obj s text p =
  let pos = {x=p.pos.x-25; y=p.pos.y+22} in
  {pos= pos; oldpos = pos; speed={vx= (-10); vy=0}; kind = Missile; w=25; h=10;
   texture = text; jumping = false; pv = 1;timer_collision = 0; timer_missile = 0; frame = 0; repos = 0; wl = [||]; wr = [||]; jl =[||]; jr = [||];  s = [||]; m = [||] }
;;

(* Fonction de debugage qui permet d'afficher le type d'un objet *)
let kind_to_string o =
  match o.kind with
  |Perso -> Printf.printf "Perso\n";
  |Plateforme -> Printf.printf "Plateforme\n";
  |Mur -> Printf.printf "Mur\n";
  |Missile -> Printf.printf "Missile\n";
  |Ennemi -> Printf.printf "Ennemi\n";
  |Wallpaper -> Printf.printf "Wallpaper\n";
;;

(* Fonction qui prend en argument un objet et le déplace, i.e ajoute sa vitesse à sa position 
Pour les missiles on n'applique pas la gravité, 
pour les ennemis et le personnage on incrémente la vitesse en y de 1 à chaque itération pour simuler une accélération
pour les ennemis la vitesse en x reste constante
pour le personnage la vitesse en x est remise à zéro à chaque itération pour éviter d'avoir une accélération en x *)
let move_objet o =
  if o.kind = Plateforme || o.kind = Mur then o
  else if o.kind = Missile then
    {o with pos = {o.pos with x = o.pos.x + o.speed.vx}; oldpos = o.pos}
  else if o.kind = Ennemi then
    {o with pos = {x = o.pos.x + o.speed.vx; y = o.pos.y + o.speed.vy};
      oldpos = o.pos;
      speed = {vx = o.speed.vx; vy = o.speed.vy+1}}
  else
    {o with pos = {x = o.pos.x+o.speed.vx; y = o.pos.y + o.speed.vy};oldpos = o.pos;speed = {vx = base_speed_x; vy = o.speed.vy+1}}
;;

(* Fonction qui prend en argument une liste l et un objet o et renvoie une liste égale à l dans laquelle on a enlevé o *)
let remove_objet l o =
  let rec aux l acc =
    match l with
    |[] -> acc
    |h::t -> if h = o then aux t acc else aux t (h::acc)
  in aux l []
;;

(* Fonction qui prend en argument un objet et renvoie vrai si l'objet est dans la fenetre, faux sinon *)
let is_in_window o =
  o.pos.x >= 0 && o.pos.x+o.w <= window_w && o.pos.y >= 0 && o.pos.y+o.h <= window_h
;;

(* Fonction qui prend en argument un objet et renvoie vrai si les pv de l'objet sont à 0, faux sinon *)
let is_dead o = o.pv = 0;;

(*Fonction qui prend en argument une liste l et renvoie une liste égale à l dans laquelle on a enlevé tous les objets dont les pv sont à 0 *)
let remove_if_dead l =
  let rec aux l acc =
    match l with
    |[] -> acc
    |h::t -> if is_dead h then begin if h.kind == Ennemi then play_effet "Son/die.wav"; aux t acc end else aux t (h::acc)
  in aux l []
;;

(* Fonction qui prend en argument deux objet a et b et renvoie vrai si a et b sont en collision 
i.e si les carrés de leur texture ont une intersection, faux sinon*)
let check_collision a b =
  let rectA = Sdl.Rect.create (a.pos.x+a.speed.vx) (a.pos.y+a.speed.vy) a.w a.h in
  let rectB = Sdl.Rect.create (b.pos.x+b.speed.vx) (b.pos.y+b.speed.vy) b.w b.h in
  Sdl.has_intersection rectA rectB
;;

(* Fonction qui prend en argument deux objets o1 et o2 dont on sait qu'ils sont en collision et renvoie vrai si o1 a collisioné o2 par la droite,
 faux sinon*)
let collision_right o1 o2 =
  o1.oldpos.x <= o2.oldpos.x && o1.oldpos.y + o1.h > o2.oldpos.y && o1.oldpos.y < o2.oldpos.y + o2.h
;;

(* Fonction qui prend en argument deux objets o1 et o2 dont on sait qu'ils sont en collision et renvoie vrai si o1 a collisioné o2 par la gauche,
 faux sinon*)
let collision_left o1 o2 =
  o1.oldpos.x >= o2.oldpos.x+o2.w && o1.oldpos.y + o1.h > o2.oldpos.y && o1.oldpos.y < o2.oldpos.y + o2.h
;;

(* Fonction qui prend en argument deux objets o1 et o2 dont on sait qu'ils sont en collision et renvoie vrai si o1 a collisioné o2 par le bas,
 faux sinon*)
let collision_down o1 o2 =
  o1.oldpos.y <= o2.oldpos.y && o1.oldpos.y+o1.h <= o2.oldpos.y
;;

(* Fonction qui prend en argument deux objets o1 et o2 dont on sait qu'ils sont en collision et renvoie vrai si o1 a collisioné o2 par le haut,
 faux sinon*)
let collision_up o1 o2 =
  o1.oldpos.y >= o2.oldpos.y+o2.h && o1.oldpos.y >= o2.oldpos.y + o2.h
;;

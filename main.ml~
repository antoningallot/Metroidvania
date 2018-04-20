open Tsdl
open Result
open Scene
open Bigarray
open Pervasives
open Music

(* La fonction de gestion des entréees clavier pour le personnage, prend une scène en entrée et renvoie une scène
Si on appuie sur echap on quitte
On récupère l'état du clavier puis on déclare une valeur en fonction des touches enfoncées
vx et vy corresponde à la vitesse que l'on veut donner à notre personnage, m nous permet de récupérer la direction où on veut tirer *)
let movement s =
  Sdl.pump_events ();
  let t = Sdl.get_keyboard_state () in
  if t.{Sdl.get_scancode_from_key Sdl.K.escape} = 1 then quit s
  else if t.{Sdl.get_scancode_from_key Sdl.K.m} = 1 then let p = get_perso s in let newp = {p with pv = 0} in update_perso s newp
  else
    let vx = 10 * t.{Sdl.get_scancode_from_key Sdl.K.right} - 10 * t.{Sdl.get_scancode_from_key Sdl.K.left} in
    let vy = -20*t.{Sdl.get_scancode_from_key Sdl.K.up} in
    let m = 1*t.{Sdl.get_scancode_from_key Sdl.K.e} - 1*t.{Sdl.get_scancode_from_key Sdl.K.a} in
    let news = creer_missiles s m in
    move_perso news (vx, vy) m
;;


(* La fonction qui gère les entrées clavier pour le menu 
Si on appuie sur s on lance le premier niveau, si on appuie sur echap ou clique sur la croix on quitte 
On réafficje toujours la meme scene tant qu'on ne détecte pas une de ces entrées clavier *)
let event_handler_menu e s =
  display_list s;
  Sdl.render_present s.renderer;
  match Sdl.poll_event (Some e) with
  |true->
     begin
       match Sdl.Event.enum(Sdl.Event.get e Sdl.Event.typ) with
     |`Window_event ->
      begin
        let n = Sdl.Event.window_event_enum(Sdl.Event.get e Sdl.Event.window_event_id) in
        match n with
        |`Close -> quit s
        |_-> s
      end
     |_ ->
        begin
          let t = Sdl.get_keyboard_state () in
          if t.{Sdl.get_scancode_from_key Sdl.K.escape} = 1 then quit s
          else if t.{Sdl.get_scancode_from_key Sdl.K.s} = 1 then
	    begin
	      play_music "Son/bloody_tears.wav"; 
              let scene = create_scene s.window s.renderer niveau1 1 in
              display_list scene;
              scene
	    end
	  else s
        end
     end
  |false-> s
;;    

(* La fonction récursive principale du programme, prend une scène en argument et renvoie une scène
Charge soit le menu soit le jeu en fonction da la valeur de l'attribut state de la scène
Si on charge le jeu, on teste d'abord si le personnage est toujours en vie puis on gère tous les déplacements *)
let rec main scene =
  if scene.state = 0 then
    let e = Sdl.Event.create () in
    let s = event_handler_menu e scene in
    main s
  else
    begin
      let b = is_perso_dead scene in
      if b then
	begin
	  play_music "Son/menu.wav"; 
	  let e = Sdl.Event.create () in
	  let scene_menu = event_handler_menu e (load_menu scene) in
	  main scene_menu
	end
      else
	begin
	  let tomovescene = movement scene in
	  let movedscene = moveall tomovescene in
	  let pvscene = gestion_pv movedscene in
	  display_list pvscene;
	  Sdl.render_present pvscene.renderer;
	  main pvscene
	end
    end
;;

(* Fonction anonyme qui créé la fenetre et le renderer puis lance la fonction principale *)
let () =
  let window = create_window 1920 1080 "Metroidvania" in
  let renderer = create_renderer (-1) Sdl.Renderer.accelerated window in
  let s = create_scene window renderer menu 0 in
  play_music "Son/menu.wav"; 
  main s
;;

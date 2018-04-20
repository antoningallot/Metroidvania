open Tsdl;;

(*La fonction prends un chemin, va ouvrir un channel pour lancer la musique, 
va load la musique puis la lance (non cumulable, si une autre musique se lance, cela detruira la musique en cours) *)
let play_music s =
  match (Tsdl_mixer.Mixer.open_audio 44100 Tsdl_mixer.Mixer.default_format Tsdl_mixer.Mixer.default_channels 1024) with
  |Error (`Msg e) -> Sdl.log "Music error : %s" e; exit 1
  |Ok() -> 
     match (Tsdl_mixer.Mixer.load_mus s) with
     |Error (`Msg e) -> Sdl.log "Error Load Music : %s" e; exit 1
     |Ok m ->
	begin
	  match (Tsdl_mixer.Mixer.play_music m (-1)) with
	  |Error (`Msg e) -> Sdl.log "Music error : %s" e; exit 1
	  |Ok x -> ()
	end
;;

(*La fonction prends un chemin, va ouvrir un channel pour lancer l'effet, 
va load l'effet puis lance l'effet (cumulable avec d'autre effets) *)
let play_effet s =
  match (Tsdl_mixer.Mixer.open_audio 44100 Tsdl_mixer.Mixer.default_format Tsdl_mixer.Mixer.default_channels 1024) with
  |Error (`Msg e) -> Sdl.log "Music error : %s" e; exit 1
  |Ok() -> 
     match (Tsdl_mixer.Mixer.load_wav s) with
     |Error (`Msg e) -> Sdl.log "Error Load Music : %s" e; exit 1
     |Ok m ->
	begin
	  match (Tsdl_mixer.Mixer.play_channel (-1) m 0) with
	  |Error (`Msg e) -> Sdl.log "Music error : %s" e; exit 1
	  |Ok x -> ()
	end
;;

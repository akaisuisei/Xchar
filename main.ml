let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* init de SDL *)
let sdl_init () =
	begin
	Sdl.init [`EVERYTHING];
	Sdlevent.enable_events Sdlevent.all_events_mask;
	end

(* attendre une touche ... *)
let rec wait_key () =
	let e = Sdlevent.wait_event () in
	match e with
		Sdlevent.KEYDOWN _ -> ()
		| _ -> wait_key ()

(*
show img dst
affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
	let d = Sdlvideo.display_format img in
	Sdlvideo.blit_surface d dst ();
	Sdlvideo.flip dst


let main _=
	let diver = ref 0 in
	let rotok= ref (-1) and d_rot = ref 0 and b_show = ref false in
	let range = Array.lenght Sys.argv in
	for i = 1 to range do
		match Sys.argv.(i) with
			| "-r" -> if i + 1 < range then
					begin
					rotok := true;
					d_rot := int_of_string (Array.lenght Sys.argv.(i+1) );
					end
			| "-s" -> show := true
			| _ -> divers := 1; 
	done
	let img = Sdlloader.load_image Sys.argv.(1);
	let (w,h) = get_dims img in 
	(*truc de cedric*)
	let a = 0 in
	if rotok > 0 then
		a <- rotok;
	else
		a <- Rotation.detect_angle img2;

	let img3 = Rotation.rotate img2 a in
	let img4 = Recognition.identifyChars img3 in
	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
	show img display;
	wait_key ();
	if b_show then
		begin
		show img1 display;
		wait_key ();
		show img2 display;
		wait_key ();
		show img3 ();
		wait_key ();
		end
	show img_final display;
	wait_key ();

let main ();

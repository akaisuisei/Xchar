let leftSide img =
begin
  let width = (Sdlvideo.surface_info !img).Sdlvideo.w
  and height = (Sdlvideo.surface_info !img).Sdlvideo.h
  and posx = ref 0
  and posy = ref 0 in
  while (Sdlvideo.get_pixel_color !img !posx !posy) <> Sdlvideo.black do
    (if (!posy = height - 1) then
      (posx := !posx + 1;
       posy := 0)
    else
      (posy := !posy + 1))
  done;
  posx := !posx - 1;
  let clip = Sdlvideo.create_RGB_surface_format !img [] (width - !posx) height in
   for i = !posx to width - 1 do
     for j = 0 to height - 1 do
       let pixel = Sdlvideo.get_pixel !img i j in
         Sdlvideo.put_pixel clip i j pixel;
     done
   done;
   img := clip
end

let rightSide img =
begin
  let width = (Sdlvideo.surface_info !img).Sdlvideo.w
  and height = (Sdlvideo.surface_info !img).Sdlvideo.h in
  let posx = ref (width - 1)
  and posy = ref 0 in
  while (Sdlvideo.get_pixel_color !img !posx !posy) <> Sdlvideo.black do
    (if (!posy = height - 1) then
      (posx := !posx - 1;
       posy := 0)
    else
      (posy := !posy + 1))
  done;
  posx := !posx + 1;
  let clip = Sdlvideo.create_RGB_surface_format !img [] width height in
   for i = 0 to !posx do
     for j = 0 to height - 1 do
       let pixel = Sdlvideo.get_pixel !img i j in
         Sdlvideo.put_pixel clip i j pixel;
     done
   done;
   img := clip
end

(* Dimensions d'une image *)
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
 
(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = ref (Sdlloader.load_image Sys.argv.(1)) in
    let (w,h) = get_dims !img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show !img display;
      (* on attend une touche *)
      wait_key ();
      top img;
      show !img display;
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

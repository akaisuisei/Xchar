
let first (a,b,c)=a
let whiteorblack x=  match x with
    x when x<128 -> 0
  | _ -> 255

let flemme a= float_of_int a
let flemme2 b= int_of_float b
let level (r,g,b)= ((flemme r *.0.3 +. flemme g *. 0.59 +. flemme b *. 0.11)/.255.0)

let color2gray (r,g,b)= 
  let alpha= flemme2 (level (r,g,b)*.255.0) in
  (alpha,alpha,alpha)


(*                         *)      
(*      GRAYSCALE          *)
(*                         *)

let image2gray src=
  let (w,h)=get_dims src in
  for i=0 to (w-1) do
    for j=0 to (h-1) do
      Sdlvideo.put_pixel_color src i j (color2gray(Sdlvideo.get_pixel_color src i j))
    done
  done

(*                         *)      
(*     BLACK AND WHITE     *)
(*                         *)

let image2black src=
  let (w,h)=get_dims src in
  for i=0 to (w-1) do
    for j=0 to (h-1) do
      if (first ( Sdlvideo.get_pixel_color src i j)) >175 then 
      Sdlvideo.put_pixel_color src i j (255,255,255)
	  else 
      Sdlvideo.put_pixel_color src i j (0,0,0)
    done
  done

(*                         *)      
(*     MEDIAN FILTER       *)
(*                         *)

let median_filter img out=
  let (w,h)= get_dims img  in
  for i=1 to w-1 do
    for j=1 to h-1 do
      let acc=ref 0 in
      for a=i-1 to i+1 do
        for b=j-1 to j+1 do
	  (*  if (a<>i && b<>j) then *)
          acc:=!acc+first (Sdlvideo.get_pixel_color img a b);
        done;
      done;
      let alpha=whiteorblack ((!acc)/9) in
      Sdlvideo.put_pixel_color out i j (alpha,alpha,alpha);
      acc:=0;
    done
  done

(*                         *)      
(*        USELESS          *)
(*                         *)

(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end


(* Dimensions d'une image *)
		     let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)






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
 


	    
	    

(* main *)let main () =
	    begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
	    let (w,h)=get_dims img in
    let out= Sdlvideo.create_RGB_surface_format img [] w h in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
     
       let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
       Sdlvideo.put_pixel_color img 12 12 (0,0,0);
       show img  display;
       wait_key ();
       image2gray img;
       show img  display; 
       wait_key ();
       Sdlvideo.save_BMP  img "gris.BMP";

      (* on affiche l'image *)
      image2black img;
      show img  display;
      Sdlvideo.save_BMP  img "noiretblanc2.BMP";
      (* on attend une touche *)
      wait_key ();
      median_filter img out;
      show img  display;
      Sdlvideo.save_BMP  out "filtre.BMP";

 
      (* on quitte *)
     wait_key ();
      exit 0
	    end

	  let _ = main ()




 

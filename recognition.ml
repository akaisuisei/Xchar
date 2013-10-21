(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(*Retourne la valeur maximum d'une liste
  d'entiers.*)
let max list =
  begin
    let maximum = ref 0 in
      for i=0 to (List.length list) - 1 do
        if !maximum < (List.nth list i) then
          maximum := List.nth list i
      done;
      !maximum
  end

(*Renvoie la copie d'une image.*)
let copyImg img =
  begin
    let width = (Sdlvideo.surface_info img).Sdlvideo.w
    and height = (Sdlvideo.surface_info img).Sdlvideo.h in
    let surface = Sdlvideo.create_RGB_surface_format img [] width height in
    for i=0 to width - 1 do
      for j=0 to height - 1 do
        let pixel = Sdlvideo.get_pixel_color img i j in
	Sdlvideo.put_pixel_color surface i j pixel
      done
    done;
    surface
  end

(*Colorie une ligne d'une image.*)
let colorLine img ord =
  begin
    let surface = ref (copyImg !img) in
    for i=0 to (Sdlvideo.surface_info !img).Sdlvideo.w - 1 do
      Sdlvideo.put_pixel_color !surface i ord Sdlvideo.blue
    done;
    img := !surface
  end

(*Encadre les lignes avec la hauteur normalisée.*)
let identifyLines img height list =
  begin
    let surface = ref (copyImg !img) in
    for i=0 to (List.length list)- 1 do
      colorLine surface (List.nth list i);
      colorLine surface ((List.nth list i) + height)
    done;
    img := !surface
  end

let identifyChar img abs ord height =
  begin
    let surface = ref (copyImg !img) in
    for i=ord to ord + height - 1 do
      Sdlvideo.put_pixel_color !surface abs i Sdlvideo.red
    done;
    img := !surface
  end

(*Détermine la position la plus à gauche
  d'un début de ligne dans le texte.*)
let findStart img =
  begin
    let (width, height) = get_dims !img
    and i = ref 0
    and j = ref 0 
    and searching = ref true in
    while ((!i < width - 1) && (!searching = true)) do
      while ((!j < height - 1) && (!searching = true)) do
        (if (Sdlvideo.get_pixel_color !img !i !j = Sdlvideo.black) then
	  (searching := false)
	else
	  (j := !j + 1))
      done;
      j := 0;
      i := !i + 1
    done;
    !i - 1
  end

(*Prend une image et renvoie la hauteur normalisée,
  ainsi que la liste des ordonnées respectives 
  des hauts de lignes.*)
let normalizeH img =
  begin
    let list = ref []
    and list2 = ref []
    and inLine = ref false
    and width = (Sdlvideo.surface_info !img).Sdlvideo.w
    and height = (Sdlvideo.surface_info !img).Sdlvideo.h
    and countH = ref 0
    and whiteLine = ref 0 in
      for j=0 to height - 1 do
        for i=0 to width - 1 do
          if ((!inLine = false) 
	    && (Sdlvideo.get_pixel_color !img i j = Sdlvideo.black)) then
              (inLine := true;
	      list2 := !list2 @ [j])
	  else if ((!inLine = true)
		&& (i = width - 1)) then
	      (if (!whiteLine = i) then
	          (list := !list @ [!countH];
	          inLine := false;
		  countH := 0;
		  whiteLine := 0)
	      else
	          (countH := !countH + 1;
	           whiteLine := 0))
	  else if (Sdlvideo.get_pixel_color !img i j = Sdlvideo.white) then
	      (whiteLine := !whiteLine + 1)
        done
      done;
    let array = Array.of_list !list2 in
    (max !list, array)
  end

let normalizeW img height array =
  begin
    let start = findStart img
    and list3 = ref []
    and (width, height) = get_dims !img
    and inChar = ref false
    and countW = ref 0 
    and whiteLine = ref 0 in
    for i=0 to ((Array.length array) - 1) do
      for abs=start to width - 1 do
	for ord=(array.(i)) to array.(i) + height - 1 do
	  (if (ord = (array.(i) + height - 1)) then
	    (if ((!inChar = true) && (!whiteLine = ord)) then
		(inChar := false;
		 list3 := !list3 @ [!countW];
		 countW := 0;
		 identifyChar img (abs-1) array.(i) height)
	     else if (!inChar = true) then
		(countW := !countW + 1);
	     whiteLine := 0)
	  else if ((!inChar = false)
	    && (Sdlvideo.get_pixel_color !img abs ord = Sdlvideo.black)) then
	    (inChar := true;
	     identifyChar img (abs-1) array.(i) height)
	  else if (Sdlvideo.get_pixel_color !img abs ord = Sdlvideo.white) then
	    (whiteLine := !whiteLine + 1))
	done
      done
    done;
    (*max !list3*)
  end
 
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
      let (max, array) = normalizeH img in
      normalizeW img max array;
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

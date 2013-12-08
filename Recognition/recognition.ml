(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

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

let rec exists (a,b) l = 
  match l with
    |[] -> false
    |(x,y)::l1 -> 
      if a = x & b = y then
	true
      else
	false || exists (a,b) l1

let rec print_list l1 =
  match l1 with
    |[] -> ()
    |(a,b)::l ->
      print_int a;
      print_int b;
      print_list l1

let rec rec_get_chars img sol (startx,starty) =
  if Sdlvideo.get_pixel_color img startx starty = Sdlvideo.black (*|| exists (startx, starty) !sol*) then
    (sol := (startx,starty):: !sol;
     rec_get_chars img sol (startx+1,starty) ;
     rec_get_chars img sol (startx, starty+1);
     ())
  else
    ()
  
let rec get_bounds1 l1 sol = 
    match l1 with 
      |[] -> sol
      |(x,y)::l -> 
	if x < sol then
	  get_bounds1 l x
	else
	  get_bounds1 l sol

let rec get_bounds2 l1 sol =
    match l1 with 
      |[] -> sol
      |(x,y)::l -> 
	if x > sol then
	  get_bounds2 l x
	else
	  get_bounds2 l sol

let rec get_bounds3 l1 sol =
    match l1 with 
      |[] -> sol
      |(x,y)::l -> 
	if y < sol then
	  get_bounds3 l y
	else
	  get_bounds3 l sol

let rec get_bounds4 l1 sol =
    match l1 with 
      |[] -> sol
      |(x,y)::l -> 
	if y > sol then
	  get_bounds4 l y
	else
	  get_bounds4 l sol

let get_box l1 sol = 
  let xmax = get_bounds2 l1 0 in
  let xmin = get_bounds1 l1 xmax
  and ymax = get_bounds4 l1 0 in
  let ymin = get_bounds3 l1 ymax
  in
  sol := (xmin,ymin,(xmax),(ymax)) :: !sol;
  ()

let rec check (x,y) l1 = 
  match l1 with 
    |[] -> true
    |(a,b,c,d)::l -> 
      if x > a & x < c & y > b & y < d then
	false
      else
	true & check (x,y) l

let get_all img boxs p = 
  let width = (Sdlvideo.surface_info img).Sdlvideo.w
  and height = (Sdlvideo.surface_info img).Sdlvideo.h in
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      if Sdlvideo.get_pixel_color img j i = Sdlvideo.black (*& check (j,i) !boxs*) then
	begin
	  rec_get_chars img p (j,i);
	  get_box !p boxs;
	  p := []
	end
    done;
  done;
  ()
      
let end_of_it_all img = 
  let boxs = ref []
  and p = ref [] in
  get_all img boxs p;
  !boxs

let line_it_up img xmin xmax ymin ymax = 
  for i = xmin to xmax-1 do
    for j = ymin to ymax-1 do
      Sdlvideo.put_pixel_color img xmin j Sdlvideo.blue;
      Sdlvideo.put_pixel_color img i ymin Sdlvideo.blue;
    done;
  done;
  ()

let draws_chars img boxs =
  match boxs with
    |[] -> img
    |(a,b,c,d)::l -> line_it_up img a b c d;
      img
      



    






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


(*Colorie une ligne d'une image.*) (*Fonction test.*)
let colorLine img ord =
  begin
    let surface = ref (copyImg !img) in
    for i=0 to (Sdlvideo.surface_info !img).Sdlvideo.w - 1 do
      Sdlvideo.put_pixel_color !surface i ord Sdlvideo.blue
    done;
    img := !surface
  end

(*Encadre les lignes avec la hauteur normalisée.*) (*Fonction test.*)
let identifyLines img height list =
  begin
    let surface = ref (copyImg !img) in
    for i=0 to (List.length list)- 1 do
      colorLine surface (List.nth list i);
      colorLine surface ((List.nth list i) + height)
    done;
    img := !surface
  end

(*Délimite par des colonnes colorées les caractères.*) (*Fonction de test.*)
let identifyChar img abs ord height color =
  begin
    let surface = ref (copyImg !img) in
    for i=ord to ord + height - 1 do
      Sdlvideo.put_pixel_color !surface abs i color
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
    let lines = Array.of_list !list2 in
    (max !list, lines)
  end

(*Prend une image et stocke la largeur normalisée
(cette dernière n'est pas encore utilisée pour des raisons de performances).*)
let normalizeW img maxH lines =
  begin
    let start = findStart img
    and list3 = ref []
    and width = (Sdlvideo.surface_info !img).Sdlvideo.w
    and inChar = ref false
    and countW = ref 0 
    and whiteLine = ref 0 in
    for i=0 to (Array.length lines) - 1 do
      for abs=start to width - 1 do
	for ord=lines.(i) to lines.(i) + maxH - 1 do
	  if (Sdlvideo.get_pixel_color !img abs ord = Sdlvideo.black) then
	    (if (!inChar = false) then
		(inChar := true;
	        identifyChar img (abs-1) lines.(i) maxH Sdlvideo.blue)
	     else
	       (if (ord = lines.(i) + maxH - 1) then
		   countW := !countW + 1))
          else if (Sdlvideo.get_pixel_color !img abs ord = Sdlvideo.white) then
	    (if (ord = lines.(i) + maxH - 1) then
		(if (!inChar = true) then
		    (if (!whiteLine = maxH - 1) then
		    (identifyChar img abs lines.(i) maxH Sdlvideo.red;
		     inChar := false;
		     list3 := !list3 @ [!countW]))
		 else
		    (countW := !countW + 1))
	     else
		(whiteLine := !whiteLine + 1))
	done;
	whiteLine := 0;
      done;
      whiteLine := 0;
      countW := 0;
      inChar := false;
    done;
    (*max !list3*)
  end 

(*Rassemble en une fonction la reconnaissance de caractères,
et renvoie l'image correspondante.*)
let recognition img =
  begin
    let image = ref img in
    let (maxHeight, lines) = normalizeH image in
    normalizeW image maxHeight lines;
    let newImage = !image in
    newImage
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
    let img = (Sdlloader.load_image Sys.argv.(1)) in
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
      let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
      (*let image = recognition img in *)
      (*let img2 = copyImg img in*)
      (*let l42 = ref [] in
      rec_get_chars img l42 (0,0);*)
      let boxs = end_of_it_all img in
      let image = draws_chars img boxs in
      show image display;
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

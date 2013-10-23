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
    !image
  end

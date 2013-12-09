(*Boîte*)
type bounding_box = {
  pos : int * int;
  width : int;
  height : int
}

(*Dimensions d'une image.*)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
  (Sdlvideo.surface_info img).Sdlvideo.h)

(*show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(*Initialisation de SDL.*)
let sdl_init () =
  Sdl.init [`EVERYTHING];
  Sdlevent.enable_events Sdlevent.all_events_mask
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
    | _ -> wait_key ()

(*Hauteur moyenne d'un ensemble de boîtes.*)
let rec averageH boxList aux length =
  match boxList with
      [] -> aux / length
    | b::l -> averageH l (aux + b.height) (length + 1)

(*Largeur moyenne.*)
let rec averageW boxList aux length =
  match boxList with
      [] -> aux / length
    | b::l -> averageW l (aux + b.width) (length + 1)

(*Fonction d'appel.*)
let average boxList vertically =
  match boxList with
     [] -> 0
    | _ ->  if vertically then averageW boxList 0 0
      else averageH boxList 0 0

(*Vérifie si une boîte est vide.*)
let isEmpty box img =
  let empty = ref true
  and (x,y) = box.pos in
    for i = x to x + box.width - 1 do
      for j = y to y + box.height - 1 do
        if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
          empty := false
      done;
    done;
    !empty

(*Retire les boîtes de tailles anormales ou vides.x*)
let rec filterHBoxes boxList img =
  let med = average boxList false in
    match boxList with
        [] -> []
      | b::l ->
        if ((b.height < (med * 1/2))
            || (b.height > (med * 21 / 8))
            || (isEmpty b img)) then
          filterHBoxes l img
        else
          b::(filterHBoxes l img)

let rec filterVBoxes boxList img =
  let med = average boxList true in
    match boxList with
        [] -> []
      | b::l ->
        if ((b.width < (med * 2/3))
            || (b.width > (med * 2))
            || (isEmpty b img)) then
          filterVBoxes l img
        else
          b::(filterVBoxes l img)

let rec addToList x = function
    [] -> [x]
  | e::l -> e::(addToList x l)

let whichDim boolean a b =
  if boolean then a else b

(*Projection horizontale ou verticale d'une boîte.*)
let projection img box vertically =
  let (w,h) = (box.width, box.height)
  and (x,y) = box.pos in
  let tab = Array.make (whichDim vertically w h) 0
  and tab2 = Array.make (whichDim vertically w h) 0
  and c = ref 0
  and pos = ref 0 in
    (if vertically then
      (
      for i = x to x + w - 1 do
	for j = 0 to y + h - 1 do
          if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
            c := !c + 1     
        done;
        tab.(!pos) <- !c;
        tab2.(!pos) <- i;
        pos := !pos + 1;
        c := 0;
      done
      )
    else
      (
      for j = y to y + h - 1 do
        for i = x to x + w - 1 do
          if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
            c := !c + 1
        done;
        tab.(!pos) <- !c;
        tab2.(!pos) <- j;
        pos := !pos + 1;
        c := 0;
      done
      )
    );
    (tab,tab2)
  

(*Découpe horizontale d'un bloc,
  via une liste des points de coupe.*)
let rec h_cut_box box y_list =
  match y_list with
      [] -> [box]
    | y::l ->
      let (a,o) = box.pos
      and w = box.width
      and h = box.height in
      let box1 = {
        pos = (a,o);
        width = w;
        height = y - o
      }
      and box2 = {
        pos = (a,y+1);
        width = w;
        height = h - (y - o)
      } in
        box1::(h_cut_box box2 l)

(*Découpe verticale d'un bloc selon
  le même principe.*)
let rec v_cut_box box x_list =
  match x_list with
      [] -> [box]
    | x::l ->
      let (a,o) = box.pos
      and w = box.width
      and h = box.height in
      let box1 = {
        pos = (a,o);
        width = x - a;
        height = h
      }
      and box2 = {
        pos = (x+1,o);
        width = w - (x - a);
        height = h
      } in
        box1::(v_cut_box box2 l)

(*Calcule le profil d'un tableau de projections:
  le nombre de pixels noirs contenus dans la boite.*)
let profile projTab s e =
  let sum = ref 0 in
    for pos = s to e do
      sum := !sum + projTab.(pos)
    done;
    !sum

(*Détermine les points de forte variation de densité de pixels.*)
let findGaps projTab posTab =
  let l = ref [] in
    for i = 0 to (Array.length projTab) - 2 do
      if ((projTab.(i) = 0) & (projTab.(i+1) != 0)) then
        l := addToList posTab.(i) !l
      else if ((projTab.(i) != 0) & (projTab.(i+1) = 0)) then
        l := addToList posTab.(i+1) !l
    done;
    !l

(*Supprime les boîtes blanches après un découpage vertical.*)
let rec suppress_V_Blank boxList projTab =
  match boxList with
      [] -> []
    | [b] -> []
    | b::l ->
      let (x,y) = b.pos
      and w = b.width in
        if (profile projTab y (y+w-1)) = 0 then
          suppress_V_Blank l projTab
        else
          b::(suppress_V_Blank l projTab)

(*Supprime les boîtes vides après un découpage horizontal.*)
let rec suppress_H_Blank boxList projTab =
  match boxList with
      [] -> []
    | [b] -> []
    | b::l ->
      let (x,y) = b.pos
      and h = b.height in
        if (profile projTab x (x+h-1)) = 0 then
          b::(suppress_H_Blank l projTab)
        else
          suppress_H_Blank l projTab

let showBox img box =
  let (x,y) = box.pos in
    for i = x to (x + box.width -1) do
      Sdlvideo.put_pixel_color img i y Sdlvideo.blue
    done;
    for i = x to (x + box.width -1) do
      Sdlvideo.put_pixel_color img i (y+ box.height -1) Sdlvideo.red
    done;
    for j = y to (y + box.height -1) do
      Sdlvideo.put_pixel_color img x j Sdlvideo.blue
    done;
    for j = y to (y + box.height -1) do
      Sdlvideo.put_pixel_color img (x + box.width - 1) j Sdlvideo.red
    done

(*Traitement d'une boîte selon un mode vertical ou horizontal.*)
let firstExec img box =
  (*calcul histogrammes ->(tableau)*)
  let (tab,tab2) = projection img box false
  and list = ref [] in
  (*détermination des points de variation forte ->(liste)*)
    list := findGaps tab tab2;
  let list2 = ref [] in
      list2 := h_cut_box box !list;
  (*élimination des nouvelles boîtes inutiles ->(liste)*)
      list2 := suppress_H_Blank !list2 tab;
      list2 := filterHBoxes !list2 img;
    !list2

let secondExec img box =
  let (tab,tab2) = projection img box true
  and list = ref [] in
  (*détermination des points de variation forte ->(liste)*)
    list := findGaps tab tab2;
  let list2 = ref [] in
    list2 := v_cut_box box !list;
    (*list2 := filterVBoxes !list2 img;*)
    !list2

let transfer list =
  let q = Queue.create () in
  for i = 0 to (List.length list) - 1 do
    Queue.add (List.nth list i) q
  done;
  q
  

(*#####################################################################
  #####################################################################*)

let cut img =
  let (w,h) = get_dims img in
   let box = {
      pos = (0,0);
      width = w;
      height = h
    } in
  let one = firstExec img box in
    let q = transfer one in
    while (Queue.is_empty q) = false do
      List.iter (showBox img) (secondExec img (Queue.take q))
    done;
    img

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
    show img display;
    wait_key ();
    cut img
  end
 
let _ = main ()

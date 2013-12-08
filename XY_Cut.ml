(*Boîte*)
type bounding_box = {
  pos : int * int;
  width : int;
  height : int
}

(*Noeud d'arbre.*)
type node = {
  box : bounding_box;
  sons : bounding_box list
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

let rec addToList x = function
    [] -> [x]
  | e::l -> e::(addToList x l)

let whichDim vertically w h =
  if vertically then h else w

(*Projection horizontale ou verticale d'une boîte.*)
let projection img box vertically =
  let (w,h) = (box.width, box.height)
  and (x,y) = box.pos in
  let tab = Array.make (whichDim vertically w h) 0
  and c = ref 0
  and pos = ref 0 in
    (if vertically then
      (
      for i = x to x + w - 1 do
	for j = y to y + h - 1 do
          if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
            c := !c + 1        
        done;
        tab.(!pos) <- !c;
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
        pos := !pos + 1;
        c := 0;
      done
      )
    );
    tab

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
let findGaps projTab =
  let l = ref [] in
    for i = 0 to (Array.length projTab) - 2 do
      if (projTab.(i)*15 < projTab.(i+1)) then
        l := addToList i !l
      else if (projTab.(i) > projTab.(i+1)*15) then
        l := addToList (i+1) !l
    done;
    !l

(*Supprime les boîtes blanches après un découpage vertical.*)
let rec suppress_V_Blank boxList projTab =
  match boxList with
      [] -> []
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
    | b::l ->
      let (x,y) = b.pos
      and h = b.height in
        if (profile projTab x (x+h-1)) = 0 then
          suppress_H_Blank l projTab
        else
          b::(suppress_H_Blank l projTab)

let showBox img box =
  let (x,y) = box.pos in
    for i = x to (x + box.width -1) do
      Sdlvideo.put_pixel_color img i y Sdlvideo.red
    done;
    for i = x to (x + box.width -1) do
      Sdlvideo.put_pixel_color img i (y+ box.height -1) Sdlvideo.red
    done;
    for j = y to (y + box.height -1) do
      Sdlvideo.put_pixel_color img x j Sdlvideo.red
    done;
    for j = y to (y + box.height -1) do
      Sdlvideo.put_pixel_color img (x + box.width - 1) j Sdlvideo.red
    done

let showBoxes img boxList =
  List.iter (showBox img) boxList

(*Traitement d'une boîte selon un mode vertical ou horizontal.*)
let exec img box vertically =
  (*calcul histogrammes -> (tableau)*)
  let tab = projection img box vertically
  and list = ref [] in
  (*détermination des points de variation forte ->(liste)*)
    list := findGaps tab;
  let list2 = ref [] in
    (if vertically then
      list2 := v_cut_box box !list
    else
      list2 := h_cut_box box !list);
  (*élimination des nouvelles boîtes vides ->(liste)*)
    (if vertically then
      list2 := suppress_V_Blank !list2 tab
    else
      list2 := suppress_H_Blank !list2 tab);
    !list2
  (*mettre chaque élément dans l'arbre ->(unit)*) (*FIX ME*)

(*#####################################################################
  #####################################################################*)

(*
let rec average aux length list =
  match list with
      [] -> aux / length
    | e::l -> average (aux + e) (length + 1) l



(*Supprime les boîtes inutiles d'une liste de boîtes.(coupe verticale)*)
let rec suppressVNoise boxList img =
  let profiles = List.map (vProfile img) boxList in
    let average = average 0 0 profiles in
      match boxList with
         [] -> []
       | b::l -> if (vProfile img b) < average / 10 then
                   suppressVNoise l img
         else
                   b::(suppressVNoise l img)


let mediumHProfile boxList img =
   let profiles = List.map (hProfile img) boxList in
     average 0 0 profiles

(*Supprime les boîtes inutiles d'une liste de boîtes.(coupe horizontale)*)
let rec suppressHNoise boxList img average =
  match boxList with
      [] -> []
    | b::l -> if (hProfile img b) < average / 10 then
                suppressHNoise l img average
              else
                b::(suppressHNoise l img average)

(*Fusionne verticalement deux boîtes.*)
let mergeBoxes_v box1 box2 =
  let (x1,y1) = box1.pos
  and (x2,y2) = box2.pos
  and w1 = box1.width
  and h1 = box1.height
  and h2 = box2.height in
    {
      pos = (x1,y1);
      width = w1;
      height = h1 + h2 + (y2 - (h1+y1))
    }            

(*Dimensions d'une image.*)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
  (Sdlvideo.surface_info img).Sdlvideo.h)

(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let colourLine img vertically pos color =
  let (w,h) = get_dims img in
  if vertically then
    for j = 0 to h - 1 do
      Sdlvideo.put_pixel_color img pos j color
    done
  else
    for i = 0 to w - 1 do
      Sdlvideo.put_pixel_color img i pos color
    done

let img_vProjection img =
  let (w,h) = get_dims img
  and l = ref []
  and c = ref 0 in
    for j = 0 to h - 1 do
      for i = 0 to w - 1 do
        if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
          c := !c + 1
      done;
      l := addToList !c !l;
      c := 0;
    done;
    !l

let img_hProjection img =
  let l = ref []
  and c = ref 0
  and (w,h) = get_dims img in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        if Sdlvideo.get_pixel_color img i j = Sdlvideo.black then
          c := !c + 1
      done;
      l := addToList !c !l;
      c := 0;
    done;
    !l



(*FONCTION TEMPORAIRE DE TEST*)
let xy_cut img box =
  let v_p = v_Projection img box in
  let av_p = Array.of_list v_p in
  let l = ref []
  and  le = Array.length av_p / 4 in
    for i = 1 to Array.length av_p - 1 do
        if ((av_p.(i) > (le*av_p.(i-1)))
           || (le*av_p.(i) < av_p.(i-1))) then
          l := addToList i !l
    done;
  let boxList = h_cut_box box !l in
  let med = mediumHProfile boxList img in
    let boxList2 = suppressHNoise boxList img med in
      let a = Array.of_list boxList2 in
        for i = 0 to (Array.length a) - 1 do
          let (x,y) = (Array.get a i).pos in
          colourLine img false y Sdlvideo.blue;
          colourLine img false (y + (Array.get a i).height-1) Sdlvideo.red
        done

(* TEST 2*)
let xy_cut2 img box =
  let h_p = h_Projection img box in
  let ah_p = Array.of_list h_p in
  let l = ref []
  and le = Array.length ah_p / 18 in 
    for i = 1 to Array.length ah_p - 1 do
        if ((ah_p.(i) > (le*ah_p.(i-1)))
           || (le*ah_p.(i) < ah_p.(i-1))) then
          l := addToList i !l
    done;
  let boxList = v_cut_box box !l in
    let boxList2 = suppressVNoise boxList img in
      let a = Array.of_list boxList2 in
        for i = 0 to (Array.length a) - 1 do
          let (x,y) = (Array.get a i).pos in
          colourLine img true y Sdlvideo.red;
          colourLine img true (y + (Array.get a i).height-1) Sdlvideo.blue
        done
*)

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
    let box = {
      pos = (0,0);
      width = w;
      height = h
    } in
    let p = projection img box false in
      show img display;
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

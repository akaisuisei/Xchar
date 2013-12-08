(*------------------------------------------*)
(*---------------MAIN WINDOW----------------*)
(*------------------------------------------*)

let window = GWindow.window
  ~width:800
  ~height:600
  ~resizable:false
  ~title:"X-Char GUI" ()


(*------------------------------------------*)
(*--------------MAIN CONTENT----------------*)
(*------------------------------------------*)

let mainbox = GPack.vbox
  ~spacing:10
  ~border_width:10
  ~packing:window#add ()

let toolbar= GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`BOTH
  ~packing:(mainbox#pack ~expand:false) ()

let visubox= GPack.hbox
  ~homogeneous: true
  ~spacing:10
  ~border_width:10
  ~packing:mainbox#add ()

let toolbar2= GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`BOTH
  ~packing:(mainbox#pack ~expand:false) ()


(*------------------------------------------*)
(*--------------IMAGE WINDOW----------------*)
(*------------------------------------------*)
let scroll = GBin.scrolled_window
  ~height:200
  ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:visubox#add ()


let image=
  let buffer= GdkPixbuf.create
    ~width:750
    ~height: 600 () 
  in
  GMisc.image
    ~pixbuf:buffer
    ~packing:scroll#add_with_viewport ()

(*text*)

let text_output= GText.buffer ()




(*------------------------------------------*)
(*---------------OPEN-----------------------*)
(*------------------------------------------*)
let file= ref ""

let default = function
  | None -> ""
  | Some v -> v

let ask_for_file parent _ =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Load an image"
    ~parent () in
    dialog#add_button_stock `CANCEL `CANCEL ;
    dialog#add_select_button_stock `OPEN `OPEN ;
    begin match dialog#run () with
      | `OPEN ->
          file := (default dialog#filename);
          image#set_file (default dialog#filename)
      | `DELETE_EVENT | `CANCEL -> ()
    end;
    dialog#destroy ()

let _open =
  let button = GButton.tool_button
    ~stock:`OPEN
    ~label:"Importer image"
    ~packing:toolbar#insert () in
  ignore(button#connect#clicked ~callback: (ask_for_file window));
  button

(*------------------------------------------*)
(*--------------ZOOM------------------------*)
(*------------------------------------------*)

let resize scale =
  if (!file<>"") then
    begin
      let img=image#pixbuf in
      let h= int_of_float (float_of_int (GdkPixbuf.get_height img) *. scale) in
      let w= int_of_float (float_of_int (GdkPixbuf.get_width img) *. scale) in
      let newimg=GdkPixbuf.from_file_at_size !file w h in
      image#set_pixbuf newimg
    end
  else ()

let pzoom _= resize 1.25
let dzoom _= resize 0.75

let zoom_in =
  let zi = GButton.tool_button
    ~stock:`ZOOM_IN
    ~packing:toolbar#insert () in
    ignore(zi#connect#clicked ~callback:pzoom);
    zi

let zoom_out =
  let zo = GButton.tool_button
    ~stock:`ZOOM_OUT
    ~packing:toolbar#insert () in
    ignore(zo#connect#clicked ~callback:dzoom);
    zo




(*------------------------------------------*)
(*----------------GRAY SCALE----------------*)
(*------------------------------------------*)

let processing ()=
  Binarization.sdl_init ();
  let img = Sdlloader.load_image (!file) in
  let img3=Binarization.image2gray img in
  Sdlvideo.save_BMP img3 "picture2.BMP";
  file:="picture2.BMP";
 image#set_file ("picture2.BMP")



let process_button =
  let pb = GButton.tool_button
    ~stock:`EXECUTE
    ~label:"Gris"
    ~packing:toolbar#insert () in
    ignore(pb#connect#clicked ~callback:processing);
    pb


(*------------------------------------------*)
(*----------------PROCESSING----------------*)
(*------------------------------------------*)




let processing ()=
  Binarization.sdl_init ();
  let img = Sdlloader.load_image (!file) in
  let img3=Binarization.image2gray img in
  let img2= ( Binarization.binarization img3 ((Binarization.otsu img3 )-35)) in
  Sdlvideo.save_BMP img2 "picture2.BMP";
  file:="picture2.BMP";
 image#set_file ("picture2.BMP")



let process_button =
  let pb = GButton.tool_button
    ~stock:`EXECUTE
    ~packing:toolbar#insert () in
    ignore(pb#connect#clicked ~callback:processing);
    pb

(*------------------------------------------*)
(*----------------TEXT WINDOW---------------*)
(*------------------------------------------*)

let scroll2 = GBin.scrolled_window
  ~height:200
  ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:visubox#add ()

let textview=GText.view
  ~buffer:text_output
  ~wrap_mode:`WORD
  ~editable:true
  ~height:200
  ~packing:scroll2#add_with_viewport ()



(*------------------------------------------*)
(*----------------QUIT BUTTON---------------*)
(*------------------------------------------*)



let quit =
  let buttonquit = GButton.tool_button
    ~stock:`QUIT
    ~packing:toolbar2#insert () in
    ignore(buttonquit#connect#clicked ~callback:GMain.quit);
    buttonquit


(*------------------------------------------*)
(*-------------SPELL CHECK------------------*)
(*------------------------------------------*)
let language= ref "fr_FR"

let spell _ =
  if(text_output#get_text () <> "") then
    if(GtkSpell.is_attached textview) then
      GtkSpell.set_language textview (Some !language)
    else GtkSpell.attach ?lang:(Some !language) textview

let fr _ = language := "fr_FR"
let en _ = language := "en_EN"
let de _ = language := "de_DE"

let languages =
  let menu = GMenu.menu () in
    let item_fr = GMenu.menu_item ~label:"Français" ~packing:menu#add () in
    let item_en = GMenu.menu_item ~label:"English" ~packing:menu#add () in
    let item_de = GMenu.menu_item ~label:"Deutsch" ~packing:menu#add () in
      ignore(item_fr#connect#activate ~callback:fr);
      ignore(item_en#connect#activate ~callback:en);
      ignore(item_de#connect#activate ~callback:de);
  menu

let spell_check =
  let sc = GButton.menu_tool_button
    ~menu:languages
    ~stock:`SPELL_CHECK
    ~packing:toolbar#insert () in
    ignore(sc#connect#clicked ~callback:spell);
    sc

(*------------------------------------------*)
(*----------------COPY----------------------*)
(*------------------------------------------*)

let str = GData.clipboard Gdk.Atom.clipboard

let copy_function _ =
  str#clear ();
  str#set_text (text_output#get_text ())

let copy_button =
  let cb = GButton.tool_button
    ~stock:`COPY
    ~packing:toolbar#insert () in
    ignore(cb#connect#clicked ~callback:copy_function);
  cb 


(*------------------------------------------*)
(*-------------------END--------------------*)
(*------------------------------------------*)

let _ =
  ignore(window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.main ()

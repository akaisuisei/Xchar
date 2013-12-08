let _= GMain.init ()
(*place the items*)
let w= 600
let h=300
let m = 20
	let window = GWindow.window 
		~title:"X-Char" 
		~height:h
		~width:w
		~border_width:5 ()

	let vbox = GPack.vbox
		~width:w
		~packing:window#add ()

	let table = GPack.table
		~columns:3
		~rows:1
		~row_spacings:5
		~height:(h-m)
		~homogeneous:true
		~packing:vbox#add ()

	let c_1 = GPack.vbox
		~spacing:5
		~packing:(table#attach ~left:0 ~top:0) ()

	let b_open = GButton.button
		~label:"open"
		~packing:c_1#pack ()

	let b_full = GButton.button
		~label:"full"
		~packing:c_1#pack()

	let b_step =GButton.button
		~label:"operate step by step"
		~packing:c_1#pack ()

	let b_save = GButton.button
		~label:"save"
		~packing:c_1#pack ()

	let b_quit = GButton.button
		~label:"quit"
		~packing:c_1#pack ()

	let img = GMisc.image 
		~file:".x.jpeg"
		~packing:(table#attach ~left:1 ~top:0) ()

	let text = GText.view
		~editable:false
		~height:200
		~width:200
		~border_width:3
		~packing:(table#attach ~left:2 ~top:0) ()

(*create some function*)
	
let main () =
	window#connect#destroy ~callback:GMain.Main.quit;
	b_quit#connect#clicked ~callback:GMain.Main.quit;
 	window #show ();
  	GMain.Main.main ()

let _ = main ()

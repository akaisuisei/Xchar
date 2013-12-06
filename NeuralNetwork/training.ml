class training () = 
object (self)

  val mutable dbias = Array.make 2 0.;

(* renvoie les ouputs du reseau de neurones pour chaque cas donné par les inputs *)
method get_result inputs (anna: ANN.ann) =
  let result = Array.make_matrix (Array.length inputs) 1 1. in
  for i = 0 to (Array.length inputs)-1 do
    (anna#forward_propagation inputs.(i) (anna#get 1) (anna#get 2));
    result.(i)<- anna#get_output 1;
  done;
    result

 (* fait la transposée d'une matrice *)
method t (mat:float array array) =
  let width = Array.length mat.(0) 
  and heigth = Array.length mat in
  let sol = Array.make_matrix width heigth mat.(0).(0) in
  for i = 0 to (Array.length mat)-1 do
    for j = 0 to (Array.length mat.(0))-1 do
      sol.(j).(i) <- mat.(i).(j)
    done;
  done;
  sol

(* fait la multiplication d'une matrice et d'un vecteur *)
method mult mat1 mat2 =
  let result = Array.make (Array.length mat1) 1. in
  for i = 0 to (Array.length mat1)-1 do
    let sol = ref 0. in
    for z = 0 to (Array.length mat2)-1 do
      sol := !sol +. mat1.(i).(z)*.mat2.(z) 
    done;
    result.(i) <- !sol;
    sol := 0.;
  done;
  result
(* Calcul de l'erreur d'une hidden layer *)
method delta mw error (activation:float array) a = 
  let result = self#mult (self#t mw) error
  and sol = Array.make (Array.length activation) 1. in
  for i = 0 to Array.length sol-1 do
    sol.(i)<- result.(i+1) *. (activation.(i) *. (1. -. activation.(i)))
  done;
  dbias.(a) <- dbias.(a) +. result.(0);
  sol

(* fait une multiplacation de deux vecteur X,1 pour avoir une matrice X,X *)
method mat_mult ar1 ar2 = 
  let sol = Array.make_matrix (Array.length ar1) (Array.length ar2) 1. 
  in
  for i = 0 to Array.length ar1 - 1 do
    for j = 0 to Array.length ar2 - 1 do
      sol.(i).(j) <- ar1.(i) *. ar2.(j)
    done;
  done;
  sol

(* Calcule l'erreur de l'ouput layer *)
method first_error activation hopes  = 
  let result = Array.make (Array.length hopes) 1. in
  for i = 0 to Array.length result -1 do
    result.(i) <- (activation.(i) -. hopes.(i));
  done;
  result

(* met à jour les poids du NN *)
method update_weights mw1 mw2 d1 d2 lmw1 lmw2 = 
  for i = 0 to Array.length d1 -1 do
    for j = 0 to Array.length d1.(0)-1 do
      mw1.(i).(j+1) <- mw1.(i).(j+1) -. (0.3*.d1.(i).(j) +. 0.9*.(mw1.(i).(j+1) -. lmw1.(i).(j+1)));
    done;
    mw1.(i).(0) <- dbias.(0);
  done;
  for i = 0 to Array.length d2 -1 do
      for j = 0 to Array.length d2.(0) -1 do
      mw2.(i).(j+1) <- mw2.(i).(j+1) -. (0.3*.d2.(i).(j) +. 0.9*.(mw2.(i).(j+1)-.  lmw2.(i).(j+1)));
    done;
    mw2.(i).(0) <- dbias.(1);
  done;

(* somme membre à membre de deux vecteurs *)  
method sum ar1 ar2 = 
  let sol = Array.make (Array.length ar1) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) +. ar2.(i)
  done;
  sol
(* Somme membre à membre de deux matrics *)
method sum_mat m1 m2 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
  in
  for i = 0 to Array.length sol -1 do
    for j = 0 to Array.length sol.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) +. m2.(j).(i)
    done;
  done;
  sol
(* Multiplication membre à membre de deux vecteurs *)
method test ar1 ar2 = 
  let sol = Array.make (Array.length ar2) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) *. ar2.(i);
  done;
  sol


(* Divise l'erreur des couches internes par le nombre de cas d'entrainement *)
method div_delta m m1 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
in
  for i = 0 to Array.length m1 -1 do
    for j = 0 to Array.length m1.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) /. (float_of_int) m
    done;
  done;
  sol

method output_evolution a b = 
  for i = 0 to Array.length a-1 do 
    print_float (b.(i) -. a.(i));
    print_newline ()
  done;
  print_newline ();

method print_error a = 
  for i = 0 to Array.length a -1 do
    print_float a.(i);
    print_newline ()
  done;
  print_newline ();
		   

(* self explanatory *)
method train (p:ANN.ann) inputs outputs =
  let errors = Array.make_matrix 3 2 1.
  and delta1 =ref  (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.)
  and delta2 =ref  (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.)
  in let lastmw1 =ref  (p#get 1)
  and lastmw2 =  ref (p#get 2)
  and transitmw1 = ref (p#get 1)
  and transitmw2 = ref (p#get 2)
  and old_output =ref  (p#get_output 1)
     in
     errors.(0) <- Array.make 1 0.;
     errors.(1) <- Array.make 2 0.;
     errors.(2) <- Array.make 2 0.;
     for i = 0 to 10000 do
       for z = 0 to Array.length inputs -1 do
	 p#forward_propagation inputs.(z) (p#get 1) (p#get 2);
	 errors.(0) <- self#first_error (p#get_output 1) (outputs.(z));
	 errors.(1) <- (self#delta (p#get 2) errors.(0) (p#get_output 0) 0) ;
	 errors.(2) <- (self#delta (p#get 1) errors.(1) inputs.(z) 1);
	 delta1 := self#sum_mat (!delta1) (self#mat_mult ( inputs.(z)) errors.(1));
	 delta2 := self#sum_mat (!delta2) (self#mat_mult (p#get_output 0) errors.(0));
	 if i mod 100 = 0 then
	   begin
	     (*print_string "debut";
	     self#print_error errors.(0);
	     self#print_error errors.(1);
	     self#print_error errors.(2);
	     print_string "fin \n";*)
	     Printf.printf "%d eme couche  " z;
	     self#output_evolution !old_output (p#get_output 1);
	     old_output := (p#get_output 1);
	   end;
       done;
       delta1 := self#div_delta (Array.length inputs) !delta1;
       delta2 := self#div_delta (Array.length inputs) !delta2;
       dbias.(0) <- dbias.(0)/.float_of_int (Array.length inputs);
       dbias.(1) <- dbias.(1) /. float_of_int (Array.length inputs);
       transitmw1 := p#get 1 ;
       transitmw2 := p#get 2 ;
       self#update_weights (p#get 1) (p#get 2) !delta1 !delta2 !lastmw1 !lastmw2;
       dbias.(0) <- 0.;
       dbias.(1) <- 0.;
       lastmw1 := !transitmw1;
       lastmw2 := !transitmw2;
       delta1 := (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.);
       delta2 := (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.);
       errors.(0) <- Array.make 1 0.;
       errors.(1) <- Array.make 2 0.;
       errors.(2) <- Array.make 2 0.;
     done;
  

method all (p:ANN.ann) inputs (outputs:float array array) = 
  let sol = ref true
  and loop = ref true
  in
  while !sol do
    p#randominit;
    self#train p inputs outputs;
    for i = 0 to Array.length outputs -1 do
      for j = 0 to Array.length outputs.(0) -1 do
	p#forward_propagation inputs.(i) (p#get 1) (p#get 2);
	let a = p#get_output 1 in
	let b = a.(j) in
	match outputs.(i).(j) with
	  |0. -> if b > 0.1 then loop :=!loop & false else loop :=!loop & true
	  |1. -> if b < 0.9 then loop :=!loop & false else loop :=!loop & true
	  |_ -> ()
      done;
    done;
    sol := false;
    loop := true;
  done;

      
  

end;;

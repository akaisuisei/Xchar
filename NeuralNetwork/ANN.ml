class ann (nb1:int) (nb2:int) (nb3:int) =
object (self)
  val nb_entree = nb1
  val nb_hidden = nb2
  val nb_output = nb3
  val mutable mat_anu = Array.make_matrix 1 1 new anu
  val mutable mw1 = Array.make_matrix nb2 (nb1 + 1) 1.
  val mutable mw2 = Array.make_matrix nb3 (nb2 + 1) 1.
  (* puts random values between 0 and 1 on all weights *)
  method randominit = 
    let m1 = self#get 1 and m2 = self#get 2 in
    for i= 0 to Array.length m1-1 do
      for j = 0 to Array.length m1.(0) -1 do
	m1.(i).(j) <- Random.float 1.
      done;
    done;
    for i2 = 0 to Array.length m2-1 do
      for j2 = 0 to Array.length m2.(0)-1 do
	m2.(i2).(j2)<- Random.float 1.
      done;
    done;
(* creates the Neural Units matrix *)
    method matrix nb_hidden nb_output = 
    let result = Array.make 2 (Array.make 1 new anu) in
    result.(0)<- Array.make nb_hidden new anu;
    result.(1)<- Array.make nb_output new anu;
    result

(* initialize said Matrix *)
    method init_matrix mat = 
      for i = 0 to Array.length mat-1 do
	for j = 0 to Array.length mat.(i)-1 do
	  mat.(i).(j)<- new anu
	done
      done;
	mat

(* basic sigmoid function *)
    method sigmoid x = 
      let a =Pervasives.exp ((-1.)*.x) in
      1./.(1. +. a)


(* gere la somme membre à membre de deux vecteurs de float *)
    method sum array1 array2 =
      let sol = ref 0.  in
      for i = 0 to (Array.length array1)-2 do 
	sol := !sol +. (array1.(i+1) *. array2.(i))
      done;
      sol := !sol +. array1.(0);
      sol := self#sigmoid !sol;
      !sol
(*gere la somme membre à membre d'un vecteur de float et d'un vecteur d'anu*)
    method sum2 array1 array2 = 
      let sol = ref 0.  in
      for i = 0 to (Array.length array1)-2 do 
	sol := !sol +. (array1.(i+1) *. array2.(i)#get)
      done;
      sol := !sol +. array1.(0);
      sol := self#sigmoid !sol;
      !sol

	
    (* self explanatory *)
    method forward_propagation input mw1 mw2 =
      for i = 0 to Array.length mat_anu.(0)-1 do
	mat_anu.(0).(i)#chg (self#sum mw1.(i) input)
      done;
      for j = 0 to Array.length mat_anu.(1) - 1 do
	mat_anu.(1).(j)#chg (self#sum2 mw2.(j) mat_anu.(0))
      done

(* Gets the neural units matrix *)
    method gett = mat_anu

(* gets the weight's matrix *)
    method get a = 
      if a = 1 then mw1 else mw2

(* Gets the output layer as a float array *)
    method get_output a = 
      let result = Array.make (Array.length mat_anu.(a)) 1. in
      for i = 0 to Array.length result-1 do
	result.(i) <- mat_anu.(a).(i)#get
      done;
      result

(* prints the output *)
    method get_result = 
      for i = 0 to Array.length mat_anu.(1)-1 do
	print_float mat_anu.(1).(i)#get;
	print_newline();
      done


    initializer mat_anu <-  self#init_matrix (self#matrix nb_hidden nb_output); self#randominit
    
    
end ;;

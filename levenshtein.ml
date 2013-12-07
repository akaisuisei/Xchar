let distance x y=
  let (m,n)=(1+String.length x,1+String.length y) in
  let mat=Array.create_matrix (m) (n) 0 in
  for i=0 to m-1 do
    mat.(i).(0)<-i;
  done;
  for j=0 to n-1 do
    mat.(0).(j)<-j;
  done;
  for j=1 to n-1 do
    for i=1 to m-1 do
      begin
	if (String.get x (i-1))=(String.get y (j-1)) then
	  mat.(i).(j)<-mat.(i-1).(j-1)
	else 
	  mat.(i).(j)<- min (mat.(i-1).(j) +1) (min (mat.(i).(j-1)+1) (mat.(i-1).(j-1)+1));
      end
    done;
  done;
mat.(m-1).(n-1);;


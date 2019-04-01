type coordinate = int * int ;;

type importance = int;;

type puzzle = (coordinate * importance ) list;;

(*--------------------------*)
type bridge = { v : bool ; d : bool } ;;

type cell = | Nothing | Island of importance | Bridge of bridge ;;

type solution = cell list list ;;

(*--------------------------------------*)

let p1= (
    [(0,0),0;  (0,1),0;  (0,2),2; (0,3),0  ;(0,4),0;
     (1,0),0;  (1,1),0;  (1,2),0; (1,3),0  ;(1,4),0;
     (2,0),3;  (2,1),0;  (2,2),8; (2,3),0  ;(2,4),4;
     (3,0),0;  (3,1),0;  (3,2),0; (3,3),0  ;(3,4),0;
     (4,0),3;  (4,1),0;  (4,2),5; (4,3),0  ;(4,4),3
    ]:puzzle);;


let pf = (
    [
      [Nothing                  ;Nothing                  ;Island(2)               ;Nothing                   ;Nothing];
      [Nothing                  ;Nothing                  ;Bridge{v=true;d=true}   ;Nothing                   ;Nothing];
      [Island(3)                ;Bridge{v=false;d=true}   ;Island(8)               ;Bridge{v=false;d=true}    ;Island(4)];
      [Bridge{v=true;d=false}   ;Nothing                  ;Bridge{v=true;d=true}   ;Nothing                   ;Bridge{v=true;d=true}];
      [Island(3)                ;Bridge{v=false;d=true}   ;Island(5)               ;Bridge{v=false;d=false}   ;Island(3)]
    ]
    :solution);;


(* retourne la liste a partir du point x1,y1*)
let commencerA = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |[] -> failwith "x1,y1 n'est pas dans puzzle"
    |((x2,y2),i)::t-> if x2=x1 && y1=y2 then t else aux t
  in aux puzzle;;

(*Toutes les fonctions trouverIle cherchent les iles voisines a partir d'un point et retourne les iles si il y en a et None s'il n'y a pas de voisins ou si'il y a un pont entre deux iles
  Elle prend en parametre un puzzle tranformé
*)
let trouverIleVerticaleBas = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2<>x1 && y2=y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |((x2,y2),Bridge{v=x;d=y})::t when x1<>x2 && y2=y1 -> None
    |_::t -> aux t
  in aux (commencerA (x1,y1) puzzle);;

let trouverIleVerticaleHaut = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2<>x1 && y2=y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |((x2,y2),Bridge{v=x;d=y})::t when x1<>x2 && y1=y2 -> None                                  
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;


let trouverIleHorizontaleDroite = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2=x1 && y2<>y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |((x2,y2),Bridge{v=x;d=y})::t when y1<>y2 && x2=x1 -> None                                  
    |_::t ->aux t
  in aux (commencerA (x1,y1) (puzzle));;

let trouverIleHorizontaleGauche = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2=x1 && y2<>y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |((x2,y2),Bridge{v=x;d=y})::t when x2=x1 && y1<>y2 -> None                                  
    |_::t->aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;


(*Methode qui compte le nombre d'iles voisines d'une ile sans compter celle qui sont déjà reliées*)
let nbrIleVoisine = fun (x1,y1) p ->
  (if (trouverIleHorizontaleGauche (x1,y1) (p) )<>None then 1 else 0)
  +
    (if (trouverIleHorizontaleDroite (x1,y1) (p))<>None then 1 else 0)
  +
    (if (trouverIleVerticaleHaut (x1,y1) (p))<>None then 1 else 0)
  +
    (if (trouverIleVerticaleBas (x1,y1) (p))<>None then 1 else 0);;



(*Methode qui transforme un puzzle*)
let tranf = fun p ->
  let rec aux = fun  p l ->
    match p with
    |((x1,y1),0)::t -> aux t (l@[((x1,y1),Nothing)])
    |((x1,y1),i)::t -> aux t (l@[((x1,y1),Island(i))])
    | [] -> l
  in aux p [];;

let p2 =  [
    ((0, 0), Nothing);  ((0, 1), Nothing); ((0, 2), Island 2); ((0, 3), Nothing); ((0, 4), Nothing);
    ((1, 0), Nothing);  ((1, 1), Nothing); ((1, 2), Nothing);  ((1, 3), Nothing); ((1, 4), Nothing);
    ((2, 0), Island 3); ((2, 1), Nothing); ((2, 2), Island 8); ((2, 3), Nothing); ((2, 4), Island 4);
    ((3, 0), Nothing);  ((3, 1), Nothing); ((3, 2), Nothing);  ((3, 3), Nothing); ((3, 4), Nothing);
    ((4, 0), Island 3); ((4, 1), Nothing); ((4, 2), Island 5); ((4, 3), Nothing); ((4, 4), Island 3)
  ];;


(*
let remplacerValPar= fun  (x1,y1) repl p ->
  let rec aux = fun  p l->
    match p with
    |((x2,y2),smth)::t -> if x1=y1 && x2=y2 then l@[((x2,y2),repl)]@t else aux t (l@[((x2,y2),smth)]) 
    |[]->l
  in aux p [];;
 *)

(*Methode qui trace un pont entre deux points et diminue leur importance, elle prend en parametre un booleen pour savoir si le pont est double  *)
let tracerPont =fun (x1,y1) (x2,y2) estDouble puzl -> (* (0,2) (4,2) (Insland 10) puztranf *)
  let nbponts = if estDouble then 2 else 1 in
  let rec tracerVertical=fun (x1,y1) (x2,y2) p l ->
    match p with
    |((x3,y3),Nothing)::t-> if y3=y2 && x3>x1 && x3<x2 then tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=true;d=estDouble})::l)   else tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Nothing)::l)
    |[]->l
    |((x3,y3),Island(i))::t when (x3=x1 && y3=y1) || (x3=x2 && y3=y2) -> tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Island(i-nbponts))::l)
    |(x,y)::t -> tracerVertical (x1,y1) (x2,y2) t ((x,y)::l)
  in
  let rec tracerHorizontal=fun (x2,y1) (x2,y2)  p l ->
    match p with
    |((x3,y3),Nothing)::t-> if x3=x2 && y3>y1 && y3<y2 then tracerHorizontal (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=false;d=estDouble})::l)   else tracerHorizontal (x1,y1) (x2,y2) t (((x3,y3),Nothing)::l)
    |((x3,y3),Island(i))::t when (x3=x1 && y3=y1) || (x3=x2 && y3=y2) -> tracerHorizontal (x1,y1) (x2,y2) t (((x3,y3),Island(i-nbponts))::l)
    |[]->l
    |(x,y)::t -> tracerHorizontal (x1,y1) (x2,y2) t ((x,y)::l)
  in
  if y2=y1 then if x1<x2 then  List.rev (tracerVertical (x1,y1) (x2,y2) puzl []) else List.rev (tracerVertical (x2,y2) (x1,y1) puzl [])
  else  if y2>y1 then List.rev (tracerHorizontal (x1,y1) (x2,y2) puzl []) else List.rev (tracerHorizontal (x2,y2) (x1,y1) puzl []);;

(*Max*)


(*Toutes les methodes trouverPont cherchent s'il y a un pont a coté d'un point*)
let trouverPontVerticaleBas= fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Bridge({v=true;d=true}))::t-> if x2<>x1 && y1=y2  then  ((x2,y2),Bridge({v=true;d=true})) else aux t
    |((x2,y2),Bridge({v=true;d=false}))::t-> if x2<>x1 && y1=y2  then  ((x2,y2),Bridge({v=true;d=false})) else aux t
    |[] ->((x1+1,y1),Nothing)
    |_::t -> aux t
  in aux (commencerA (x1,y1) puzzle);;

let trouverPontVerticaleHaut = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=true;d=true}))::t-> if x2<>x1 && y1=y2  then  ((x2,y2),Bridge({v=true;d=true})) else aux t
    |((x2,y2),Bridge({v=true;d=false}))::t-> if x2<>x1 && y1=y2  then  ((x2,y2),Bridge({v=true;d=false})) else aux t
    |[] ->((x1-1,y1),Nothing)
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;

let trouverPontHorizontaleDroite = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=false;d=true}))::t -> if x2=x1 && y1<>y2 then  ((x2,y2),Bridge({v=false;d=true})) else aux t
    |((x2,y2),Bridge({v=false;d=false}))::t -> if x2=x1 && y1<>y2 then ((x2,y2),Bridge({v=false;d=false})) else aux t
    |[] ->((x1,y1+1),Nothing)
    |_::t -> aux t
  in aux (commencerA (x1,y1)( puzzle));;

let trouverPontHorizontaleGauche = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=false;d=true}))::t -> if x2=x1 && y1<>y2 then  ((x2,y2),Bridge({v=false;d=true})) else aux t
    |((x2,y2),Bridge({v=false;d=false}))::t -> if x2=x1 && y1<>y2 then  ((x2,y2),Bridge({v=false;d=false})) else aux t
    |[] ->((x1,y1-1),Nothing)
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;

(*Methode qui retourne le nombre de ponts d'une ile*)
let nbrPont = fun (x1,y1) puzzle ->
  (if (trouverPontVerticaleHaut (x1,y1) puzzle)=((x1-1,y1),Bridge({v=true;d=true})) then 2  else 0)
  +    
    (if (trouverPontVerticaleBas (x1,y1) puzzle)=((x1+1,y1),Bridge({v=true;d=true})) then 2 else 0)
  +    
    (if (trouverPontHorizontaleDroite (x1,y1) puzzle)=((x1,y1+1),Bridge({v=false;d=true})) then 2 else 0)
  +
    (if (trouverPontHorizontaleGauche (x1,y1) puzzle)=((x1,y1-1),Bridge({v=false;d=true})) then 2 else 0)

  +

    (if (trouverPontVerticaleHaut (x1,y1) puzzle)=((x1-1,y1),Bridge({v=true;d=false})) then 1  else 0)
  +    
    (if (trouverPontVerticaleBas (x1,y1) puzzle)=((x1+1,y1),Bridge({v=true;d=false})) then 1 else 0)
  +    
    (if (trouverPontHorizontaleDroite (x1,y1) puzzle)=((x1,y1+1),Bridge({v=false;d=false})) then 1  else 0)
  +
    (if (trouverPontHorizontaleGauche (x1,y1) puzzle)=((x1,y1-1),Bridge({v=false;d=false})) then 1 else 0);;
(*Methode qui prend des coordonees et retourne l'importance de cette ile*)
let importanceIle = fun (x1,y1) puzzle ->
  let rec aux= fun p ->
    match p with
    |((x2,y2),Island(i))::t when x2=x1 && y2=y1 ->i
    |[]->failwith "point pas trouvé"
    |h::t -> aux t
  in aux puzzle;;

(*Methode qui compte le nombre de ponts max a placer*)
let nbrPontRestant = fun (x1,y1) puzzle ->
  (importanceIle (x1,y1) puzzle) - (nbrPont (x1,y1) puzzle);;

(*Equivalent du fst mais avec des iles*)
let pr =fun a ->
  match a with
  | None -> (-10,-10)
  |Some ((x,y),Island(t)) -> (x,y)
 ;;


 (*Methode qui trace des ponts dans toutes les directions en partant d'un point*)
 let tracerPontToutesDir = fun (x1,y1) estDouble puzl ->
   let p =puzl in
   let x =(trouverIleVerticaleBas (x1,y1) p) in
   let p =( if (x<>None) then (tracerPont (pr(x)) (x1,y1) estDouble p) else p) in
   let x =(trouverIleVerticaleHaut (x1,y1) p) in
   let p =( if (x<>None) then (tracerPont (pr(x)) (x1,y1) estDouble p) else p) in
   let x=(trouverIleHorizontaleDroite (x1,y1) p) in
   let p= ( if (x<>None) then (tracerPont (pr(x)) (x1,y1) estDouble p) else p) in
   let x =(trouverIleHorizontaleGauche (x1,y1) p) in
   let p =( if (x<>None) then (tracerPont (pr(x)) (x1,y1) estDouble p) else p) in
   p;;

 (*Methode qui affiche le premier element de la liste*)
 let toString = fun p->
   if p<>[] then
     (if (snd(fst(List.hd p)))=0 then print_string("\n") else print_string(""))
   else
     print_string("");
   match p with
   |((x1,y1),Island(i))::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"): Ile "^string_of_int(i)^" ");
   |((x1,y1),Bridge({v=false;d=false}))::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"): ----- ");
   |((x1,y1),Bridge({v=false;d=true}))::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"): ===== ");
   |((x1,y1),Bridge({v=true;d=false}))::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"):   |   ");
   |((x1,y1),Bridge({v=true;d=true}))::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"):   ||  ");
   |((x1,y1),Nothing)::t ->print_string("("^(string_of_int(x1))^","^(string_of_int(y1))^"):       ");
   |[]-> print_string("\n")
 ;;

 (*Methode qui affiche tout le puzzle*)
 let toStringP = fun p ->
   let rec aux =fun p1->
     toString p1;
     match p1 with
     |h::t-> aux t
     |[]-> print_string("");
   in aux p;;

 (*methode qui crée des ponts si
  * Soit n l'importance et k le nombre de voisin d'une ile
  * si n%2=0 et k=n/2 alors on crée des ponts doubles entre cette ile et ses voisins
  * si n=1 et k=1 alors on crée un pont simple entre les deux iles
*)
 let solution_simple2= fun puzzle ->
   let rec aux = fun pdebut pfin ->
     match pdebut with
     |((x1,y1),Island(k))::t when
            let m = (importanceIle (x1,y1) pfin) in
            m=1 &&(nbrIleVoisine (x1,y1) pfin)=1 ->  aux t (tracerPontToutesDir (x1,y1) false pfin) 
     |((x1,y1),Island(n))::t when
            let m = (importanceIle (x1,y1) pfin) in
            (m<>0&&(m mod 2=0) && (nbrIleVoisine (x1,y1) pfin)=(m/2)) ->
       aux t (tracerPontToutesDir (x1,y1) true pfin)
     |h::t-> aux t pfin
     |[]-> pfin
   in aux puzzle puzzle ;;
 
 (*methode bis*)(*
 let solution_simple1= fun puzzle ->
   let rec aux = fun pdebut pfin ->
     toString pdebut;
     match pdebut with
    |((x1,y1),Island(1))::t when (nbrIleVoisine (x1,y1) pfin)=1 ->  aux t (tracerPontToutesDir (x1,y1) false pfin) 
    |((x1,y1),Island(n))::t when (n<>0&&(n mod 2=0) && (nbrIleVoisine (x1,y1) pfin)=(n/2)) ->  aux t (tracerPontToutesDir (x1,y1) true pfin)
(*    
    |((x1,y1),Island(2))::t when (nbrIleVoisine (x1,y1) pfin)=1 ->  aux t (tracerPontToutesDir (x1,y1) true pfin)
    |((x1,y1),Island(4))::t when (nbrIleVoisine (x1,y1) pfin)=2 ->  aux t (tracerPontToutesDir (x1,y1) true pfin)
    |((x1,y1),Island(6))::t when (nbrIleVoisine (x1,y1) pfin)=3 ->  aux t (tracerPontToutesDir (x1,y1) true pfin)
    |((x1,y1),Island(8))::t when (nbrIleVoisine (x1,y1) pfin)=4 ->  aux t (tracerPontToutesDir (x1,y1) true pfin)
 *)
    |h::t-> aux t pfin
    |[]-> pfin
  in aux puzzle puzzle ;;
                 *)
 p2;;
 
 let x=solution_simple2 p2;;
let z = solution_simple2 x;;
toStringP x;;
toStringP z;;

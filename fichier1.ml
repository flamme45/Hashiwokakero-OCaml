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

let puzzle1= (
    [(0,0),0;  (0,1),0;  (0,2),2; (0,3),0  ;(0,4),0;
     (1,0),0;  (1,1),0;  (1,2),0; (1,3),0  ;(1,4),0;
     (2,0),3;  (2,1),0;  (2,2),8; (2,3),0  ;(2,4),4;
     (3,0),0;  (3,1),0;  (3,2),0; (3,3),0  ;(3,4),0;
     (4,0),3;  (4,1),0;  (4,2),5; (4,3),0  ;(4,4),3
    ]:puzzle);;


let puzzle2 = (
  [
    (0,0),4;  (0,1),0;  (0,2),0;  (0,3),4;  (0,4),0;  (0,5),0;  (0,6),3;

    (1,0),0;  (1,1),0;  (1,2),0;  (1,3),0;  (1,4),0;  (1,5),0;  (1,6),0;
    
    (2,0),0;  (2,1),1;  (2,2),0;  (2,3),4;  (2,4),0;  (2,5),2;  (2,6),0;
     
    (3,0),4;  (3,1),0;  (3,2),0;  (3,3),0;  (3,4),0;  (3,5),0;  (3,6),5;
     
    (4,0),0;  (4,1),0;  (4,2),0;  (4,3),0;  (4,4),0;  (4,5),0;  (4,6),0;

    (5,0),2;  (5,1),0;  (5,2),0;  (5,3),0;  (5,4),0;  (5,5),1;  (5,6),0;
    
    (6,0),0;  (6,1),0;  (6,2),1;  (6,3),0;  (6,4),3;  (6,5),0;  (6,6),4

  ]:puzzle);;


let puzzle3 = (
  [
    (0,0),0;  (0,1),0;  (0,2),1;  (0,3),0;  (0,4),3;  (0,5),0;  (0,6),1;

    (1,0),2;  (1,1),0;  (1,2),0;  (1,3),0;  (1,4),0;  (1,5),1;  (1,6),0;
    
    (2,0),0;  (2,1),0;  (2,2),4;  (2,3),0;  (2,4),5;  (2,5),0;  (2,6),0;
     
    (3,0),4;  (3,1),0;  (3,2),0;  (3,3),0;  (3,4),0;  (3,5),0;  (3,6),0;
     
    (4,0),0;  (4,1),0;  (4,2),0;  (4,3),0;  (4,4),0;  (4,5),0;  (4,6),0;

    (5,0),0;  (5,1),0;  (5,2),0;  (5,3),0;  (5,4),1;  (5,5),0;  (5,6),0;
    
    (6,0),3;  (6,1),0;  (6,2),3;  (6,3),0;  (6,4),0;  (6,5),2;  (6,6),0

  ]:puzzle);;


let puzzle4 = (
  [
    (0,0),2;  (0,1),0;  (0,2),3;  (0,3),0;  (0,4),1;  (0,5),0;  (0,6),1;

    (1,0),0;  (1,1),2;  (1,2),0;  (1,3),1;  (1,4),0;  (1,5),0;  (1,6),0;
    
    (2,0),0;  (2,1),0;  (2,2),0;  (2,3),0;  (2,4),0;  (2,5),0;  (2,6),0;
     
    (3,0),0;  (3,1),0;  (3,2),1;  (3,3),0;  (3,4),0;  (3,5),0;  (3,6),0;
     
    (4,0),0;  (4,1),3;  (4,2),0;  (4,3),5;  (4,4),0;  (4,5),0;  (4,6),2;

    (5,0),0;  (5,1),0;  (5,2),0;  (5,3),0;  (5,4),0;  (5,5),0;  (5,6),0;
    
    (6,0),2;  (6,1),0;  (6,2),0;  (6,3),4;  (6,4),0;  (6,5),1;  (6,6),0

  ]:puzzle);;


let puzzle5 = (
  [
    (0,0),0;  (0,1),2;  (0,2),0;  (0,3),6;  (0,4),0;  (0,5),0;  (0,6),3;

    (1,0),0;  (1,1),0;  (1,2),0;  (1,3),0;  (1,4),0;  (1,5),0;  (1,6),0;
    
    (2,0),1;  (2,1),0;  (2,2),0;  (2,3),6;  (2,4),0;  (2,5),2;  (2,6),0;
     
    (3,0),0;  (3,1),0;  (3,2),0;  (3,3),0;  (3,4),1;  (3,5),0;  (3,6),3;
     
    (4,0),1;  (4,1),0;  (4,2),0;  (4,3),0;  (4,4),0;  (4,5),0;  (4,6),0;

    (5,0),0;  (5,1),0;  (5,2),0;  (5,3),0;  (5,4),1;  (5,5),0;  (5,6),2;
    
    (6,0),3;  (6,1),0;  (6,2),0;  (6,3),5;  (6,4),0;  (6,5),2;  (6,6),0

  ]:puzzle);;  


let puzzle6 = (
    [(0,0),4;  (0,1),0;  (0,2),4;  (0,3),0;  (0,4),0;  (0,5),2;  (0,6),0;  (0,7),0;  (0,8),3;

     (1,0),0;  (1,1),0;  (1,2),0;  (1,3),0;  (1,4),0;  (1,5),0;  (1,6),0;  (1,7),0;  (1,8),0;
     
     (2,0),6;  (2,1),0;  (2,2),8;  (2,3),0;  (2,4),4;  (2,5),0;  (2,6),0;  (2,7),1;  (2,8),0;
     
     (3,0),0;  (3,1),0;  (3,2),0;  (3,3),0;  (3,4),0;  (3,5),0;  (3,6),1;  (3,7),0;  (3,8),3;
     
     (4,0),0;  (4,1),0;  (4,2),2;  (4,3),0;  (4,4),2;  (4,5),0;  (4,6),0;  (4,7),1;  (4,8),0;

     (5,0),4;  (5,1),0;  (5,2),0;  (5,3),3;  (5,4),0;  (5,5),2;  (5,6),0;  (5,7),0;  (5,8),0;

     (6,0),0;  (6,1),0;  (6,2),0;  (6,3),0;  (6,4),0;  (6,5),0;  (6,6),2;  (6,7),0;  (6,8),3;
     
     (7,0),0;  (7,1),1;  (7,2),0;  (7,3),5;  (7,4),0;  (7,5),4;  (7,6),0;  (7,7),0;  (7,8),0;
     
     (8,0),3;  (8,1),0;  (8,2),3;  (8,3),0;  (8,4),2;  (8,5),0;  (8,6),3;  (8,7),0;  (8,8),2
     
    ]:puzzle);;


let p2 =  [
    ((0, 0), Nothing);  ((0, 1), Nothing); ((0, 2), Island 2); ((0, 3), Nothing); ((0, 4), Nothing);
    ((1, 0), Nothing);  ((1, 1), Nothing); ((1, 2), Nothing);  ((1, 3), Nothing); ((1, 4), Nothing);
    ((2, 0), Island 3); ((2, 1), Nothing); ((2, 2), Island 8); ((2, 3), Nothing); ((2, 4), Island 4);
    ((3, 0), Nothing);  ((3, 1), Nothing); ((3, 2), Nothing);  ((3, 3), Nothing); ((3, 4), Nothing);
    ((4, 0), Island 3); ((4, 1), Nothing); ((4, 2), Island 5); ((4, 3), Nothing); ((4, 4), Island 3)
  ];;


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


(*Max*)


(*Toutes les methodes trouverPont cherchent s'il y a un pont a coté d'un point*)
let trouverPontVerticaleBas= fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Bridge({v=true;d=true}))::t-> if x2<>x1 && y1=y2  then Some  ((x2,y2),Bridge({v=true;d=true})) else aux t
    |((x2,y2),Bridge({v=true;d=false}))::t-> if x2<>x1 && y1=y2  then Some  ((x2,y2),Bridge({v=true;d=false})) else aux t
    |[] ->None
    |_::t -> aux t
  in aux (commencerA (x1,y1) puzzle);;

let trouverPontVerticaleHaut = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=true;d=true}))::t-> if x2<>x1 && y1=y2  then Some ((x2,y2),Bridge({v=true;d=true})) else aux t
    |((x2,y2),Bridge({v=true;d=false}))::t-> if x2<>x1 && y1=y2  then Some  ((x2,y2),Bridge({v=true;d=false})) else aux t
    |[] ->None
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;

let trouverPontHorizontaleDroite = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=false;d=true}))::t -> if x2=x1 && y1<>y2 then Some  ((x2,y2),Bridge({v=false;d=true})) else aux t
    |((x2,y2),Bridge({v=false;d=false}))::t -> if x2=x1 && y1<>y2 then Some ((x2,y2),Bridge({v=false;d=false})) else aux t
    |[] ->None
    |_::t -> aux t
  in aux (commencerA (x1,y1)( puzzle));;

let trouverPontHorizontaleGauche = fun  (x1,y1) puzzle ->
  let rec aux =  fun l ->
    match l with
    |((x2,y2),Bridge({v=false;d=true}))::t -> if x2=x1 && y1<>y2 then Some ((x2,y2),Bridge({v=false;d=true})) else aux t
    |((x2,y2),Bridge({v=false;d=false}))::t -> if x2=x1 && y1<>y2 then Some  ((x2,y2),Bridge({v=false;d=false})) else aux t
    |[] ->None
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;

(*Methode qui retourne le nombre de ponts d'une ile*)
let nbrPont = fun (x1,y1) puzzle ->
  (if (trouverPontVerticaleHaut (x1,y1) puzzle)=Some((x1-1,y1),Bridge({v=true;d=true})) then 2  else 0)
  +    
    (if (trouverPontVerticaleBas (x1,y1) puzzle)=Some((x1+1,y1),Bridge({v=true;d=true})) then 2 else 0)
  +    
    (if (trouverPontHorizontaleDroite (x1,y1) puzzle)=Some((x1,y1+1),Bridge({v=false;d=true})) then 2 else 0)
  +
    (if (trouverPontHorizontaleGauche (x1,y1) puzzle)=Some((x1,y1-1),Bridge({v=false;d=true})) then 2 else 0)

  +

    (if (trouverPontVerticaleHaut (x1,y1) puzzle)=Some((x1-1,y1),Bridge({v=true;d=false})) then 1  else 0)
  +    
    (if (trouverPontVerticaleBas (x1,y1) puzzle)=Some((x1+1,y1),Bridge({v=true;d=false})) then 1 else 0)
  +    
    (if (trouverPontHorizontaleDroite (x1,y1) puzzle)=Some((x1,y1+1),Bridge({v=false;d=false})) then 1  else 0)
  +
    (if (trouverPontHorizontaleGauche (x1,y1) puzzle)=Some((x1,y1-1),Bridge({v=false;d=false})) then 1 else 0);;
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


(*Toutes les fonctions trouverIle cherchent les iles voisines a partir d'un point et retourne les iles si il y en a et None s'il n'y a pas de voisins ou si'il y a un pont entre deux iles
  Elle prend en parametre un puzzle tranformé
 *)

let trouverIleVerticaleBas = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island 0)::t when x2<>x1 && y2=y1 -> None
    |((x2,y2),Island 1)::t when x2<>x1 && y2=y1 && (importanceIle (x1,y1) puzzle)=1 && (nbrPont (x2,y2) puzzle)=0->None
    |((x2,y2),Island i)::t when x2<>x1 && y2=y1 -> Some ((x2,y2),Island i) 
    |[]-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x1<>x2 && y2=y1 &&( not estVer || estDou )-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x1<>x2 && y2=y1 && ( estVer) && (not estDou) -> aux t
    |_::t -> aux t
  in if (importanceIle (x1,y1) puzzle)=0 then None else (aux (commencerA (x1,y1) puzzle));;

let trouverIleVerticaleHaut = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island 0)::t when x2<>x1 && y2=y1 -> None
    |((x2,y2),Island 1)::t when x2<>x1 && y2=y1 && (importanceIle (x1,y1) puzzle)=1 && (nbrPont (x2,y2) puzzle)=0->None
    |((x2,y2),Island i)::t when x2<>x1 && y2=y1 -> Some ((x2,y2),Island i) 
    |[]-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x1<>x2 && y2=y1 &&( not estVer || estDou )-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x1<>x2 && y2=y1 && ( estVer) && (not estDou) -> aux t
    |_::t -> aux t
  in if (importanceIle (x1,y1) puzzle)=0 then None else (aux (commencerA (x1,y1) (List.rev puzzle)));;

let trouverIleHorizontaleDroite = fun (x1,y1) puzzle ->  
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island 0)::t when x2=x1 && y2<>y1 -> None
    |((x2,y2),Island 1)::t when x2=x1 && y2<>y1 && (importanceIle (x1,y1) puzzle)=1 && (nbrPont (x2,y2) puzzle)=0->None
    |((x2,y2),Island i)::t when x2=x1 && y2<>y1 -> Some ((x2,y2),Island i) 
    |[]-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x2=x1 && y2<>y1 &&( estVer || estDou )-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x2=x1 && y2<>y1 && (not estVer) && (not estDou) -> aux t
    |_::t -> aux t
  in if (importanceIle (x1,y1) puzzle)=0 then None else (aux (commencerA (x1,y1) puzzle));;

let trouverIleHorizontaleGauche = fun (x1,y1) puzzle ->  
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island 0)::t when x2=x1 && y2<>y1 -> None
    |((x2,y2),Island 1)::t when x2=x1 && y2<>y1 && (importanceIle (x1,y1) puzzle)=1 && (nbrPont (x2,y2) puzzle)=0->None
    |((x2,y2),Island i)::t when x2=x1 && y2<>y1 -> Some ((x2,y2),Island i) 
    |[]-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x2=x1 && y2<>y1 &&( estVer || estDou )-> None
    |((x2,y2),Bridge{v=estVer;d=estDou})::t when x2=x1 && y2<>y1 && (not estVer) && (not estDou) -> aux t
    |_::t -> aux t
  in if (importanceIle (x1,y1) puzzle)=0 then None else (aux (commencerA (x1,y1) (List.rev puzzle)));;


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



let tracerPont =fun (x1,y1) (x2,y2) estDouble puzl -> (* (0,2) (4,2) (Insland 10) puztranf *)
  let nbponts = if estDouble then 2 else 1 in
  let rec tracerVertical=fun (x1,y1) (x2,y2) p l ->
    match p with
    |((x3,y3),Nothing)::t when y3=y2 && x3>x1 && x3<x2 -> tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=true;d=estDouble})::l)
    |((x3,y3),Bridge{v=a;d=b})::t when y3=y2 && x3>x1 && x3<x2 && not b && not estDouble -> tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=a;d=true})::l)
    |((x3,y3),Island(i))::t when (x3=x1 && y3=y1) || (x3=x2 && y3=y2) -> tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Island(i-nbponts))::l)
    |[]->l
    |(x,y)::t -> tracerVertical (x1,y1) (x2,y2) t ((x,y)::l)
  in
  let rec tracerHorizontal=fun (x2,y1) (x2,y2)  p l ->
    match p with
    |((x3,y3),Nothing)::t when  x3=x2 && y3>y1 && y3<y2 -> tracerHorizontal (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=false;d=estDouble})::l)
    |((x3,y3),Island(i))::t when (x3=x1 && y3=y1) || (x3=x2 && y3=y2) -> tracerHorizontal (x1,y1) (x2,y2) t (((x3,y3),Island(i-nbponts))::l)
    |((x3,y3),Bridge{v=a;d=b})::t when x3=x2 && y3>y1 && y3<y2 && not b && not estDouble -> tracerVertical (x1,y1) (x2,y2) t (((x3,y3),Bridge{v=a;d=true})::l)                                                              
    |[]->l
    |(x,y)::t -> tracerHorizontal (x1,y1) (x2,y2) t ((x,y)::l)
  in
  if y2=y1 then if x1<x2 then  List.rev (tracerVertical (x1,y1) (x2,y2) puzl []) else List.rev (tracerVertical (x2,y2) (x1,y1) puzl [])
  else  if y2>y1 then List.rev (tracerHorizontal (x1,y1) (x2,y2) puzl []) else List.rev (tracerHorizontal (x2,y2) (x1,y1) puzl []);;


(*Equivalent du fst mais avec des iles*)
let pr =fun a ->
  match a with
  | None -> (-10,-10)
  | Some ((x,y),Island(t)) -> (x,y)   
  | Some ((x,y),_)->(x,y)
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


 (*methode qui crée des ponts si
  * Soit n l'importance et k le nombre de voisin d'une ile
  * si n%2=0 et k=n/2 alors on crée des ponts doubles entre cette ile et ses voisins
  * si n=1 et k=1 alors on crée un pont simple entre les deux iles
*)
 
 let a = fun (x,y) p ->
   if (trouverPontHorizontaleGauche (x,y) p) <> None then 1 else 2;;
 let b = fun (x,y) p ->
   if (trouverPontHorizontaleDroite (x,y) p) <> None then 1 else 2;;
 let c = fun (x,y) p ->
   if (trouverPontVerticaleBas (x,y) p) <> None then 1 else 2;;
 let d = fun (x,y) p ->
   if (trouverPontVerticaleBas (x,y) p) <> None then 1 else 2;;
 let pontsMaxRestants = fun (x,y) p ->
   let a1 = if ((trouverIleHorizontaleGauche (x,y) p)<>None) then (a (x,y) p) else 0 
   in  let b1 = if ((trouverIleHorizontaleDroite (x,y) p)<>None) then (b (x,y) p) else 0
       in  let c1 = if ((trouverIleVerticaleHaut (x,y) p)<>None) then (c (x,y) p) else 0
           in  let d1 = if ((trouverIleVerticaleBas (x,y) p)<>None) then (d (x,y) p) else 0  in  
               a1+b1+c1+d1;;
 
 

 
 let solution_simple4 = fun puzzle ->
   let rec aux = fun pdebut pfin ->
     match pdebut with
     |((x1,y1),Island(n))::t when
            let m = (importanceIle (x1,y1) pfin) in
            (m<>0&&((m+1) mod 2=0) && (nbrIleVoisine (x1,y1) pfin)=(((m+1)/2))) ->
       aux t (tracerPontToutesDir (x1,y1) false pfin)
     |((x1,y1),Island(n))::t when
            let m = (importanceIle (x1,y1) pfin) in
            (m<>0&&(m mod 2=0) && (nbrIleVoisine (x1,y1) pfin)=(m/2)) ->
       aux t (tracerPontToutesDir (x1,y1) true pfin)
     |((x1,y1),Island(n))::t when
            let m = (importanceIle (x1,y1) pfin) in
            let pontrest= (pontsMaxRestants (x1,y1) pfin) in
            (m<>0 && (m=pontrest)) ->
       aux t (tracerPontToutesDir (x1,y1) false pfin)
     |h::t-> aux t pfin
     |[]-> pfin
   in aux puzzle puzzle ;;
 
let solTout0 = fun puzzle ->
  let rec aux= fun p ->
    match p with
    |((x,y),Island(i))::t when i<>0 -> false
    |[]->true
    |h::t -> aux t
  in aux puzzle;;

let sontEgaux = fun p1 p2 ->
  let rec aux= fun  p1 p2 ->
    match (p1,p2) with
    | ((x1,y1)::t1,(x2,y2)::t2) when x1<>x2 || y1<>y2 -> false
    | ([],_) | (_,[])->true
    | (h1::t1,h2::t2) -> aux t1 t2
  in aux p1 p2 ;;

let resoudre =  fun p ->
  let rec aux = fun pdebut ->
    let pfin = solution_simple4 pdebut in
    if (solTout0 pfin) then(  print_string("\nPuzzle résolu\n"); pfin)
       else(
         if (sontEgaux (pfin) (pdebut)) then( print_string("Puzzle non soluble et non fini\n"); pfin)
         else aux pfin)
  in aux p;;



let remplacer0ParVal= fun pfini pdebut ->
  let rec aux=fun p1 p2 pfinal ->
    match (p1,p2) with
    |(_::t1,((x1,y1),Island(i))::t2) -> aux t1 t2 (pfinal@[((x1,y1),Island(i))])
    |(((x1,y1),Bridge{v=a;d=b})::t1),_::t2->aux t1 t2 (pfinal@[((x1,y1),Bridge{v=a;d=b})])
    |(h1::t1,h2::t2)->aux t1 t2 (pfinal@[h1])
    |([],[])->pfinal
    | _ -> failwith "Les puzzle ne sont pas le memes"
  in aux pfini pdebut [];;                                     

let creerListeListe = fun p ->
  let rec aux=fun pdebut ligne ppfin ->
    match pdebut with
    |((x,y),z)::t when y=0 &&(List.length ligne)<>0 -> aux t ([z]) (ppfin@[ligne])
    |((x,y),z)::t -> aux t (ligne@[z]) (ppfin)
    | [] -> ppfin@[ligne]
  in aux p [] [];;



let resultatFinal = fun p ->
  let p1=tranf p in
  let p2 = resoudre p1 in
  let p3 = remplacer0ParVal p2 p1 in
  (*toStringP p3 ;*)
  let p4 = creerListeListe p3 in
  p4;;


let toStringLigne = fun p ->
  let rec aux = fun  p s ->
    match p with
   |Island(i)::t ->aux t s^(" Ile "^string_of_int(i)^" ");
   |Bridge({v=false;d=false})::t ->aux t s^(" ----- ");
   |Bridge({v=false;d=true})::t ->aux t s^(" ===== ");
   |Bridge({v=true;d=false})::t ->aux t s^("   |   ");
   |Bridge({v=true;d=true})::t ->aux t s^("   ||  ");
   |Nothing::t ->aux t s^("       ");
   |[]-> s^"\n"
  in aux p ""

let toString = fun p ->
  let rec aux = fun p1 s->
    match p1 with
    |[] -> s^"\n"
    |a::t -> aux t (s^(toStringLigne (List.rev a)))
  in aux p "";;

let main()=
  print_string (toString (resultatFinal puzzle4));;


main ();;

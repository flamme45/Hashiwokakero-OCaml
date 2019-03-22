type coordinate = int * int ;;

type importance = int;;

type puzzle = (coordinate * importance ) list;;

(*--------------------------*)
type bridge = { v : bool ; d : bool } ;;

type cell = | Nothing | Island of importance | Bridge of bridge ;;

type solution = cell list list ;;

(*--------------------------------------*)

let puzzle= (
    [(0,0),0;  (0,1),0;  (0,2),2; (0,3),0  ;(0,4),0;
     (1,0),0;  (1,1),0;  (1,2),0; (1,3),0  ;(1,4),0;
     (2,0),3;  (2,1),0;  (2,2),8; (2,3),0  ;(2,4),4;
     (3,0),0;  (3,1),0;  (3,2),0; (3,3),0  ;(3,4),0;
     (4,0),3;  (4,1),0;  (4,2),5; (4,3),0  ;(4,4),3
    ]:puzzle);;


let solpuzzle = (
    [
      [Nothing                  ;Nothing                  ;Island(2)               ;Nothing                   ;Nothing];
      [Nothing                  ;Nothing                  ;Bridge{v=true;d=true}   ;Nothing                   ;Nothing];
      [Island(3)                ;Bridge{v=false;d=true}   ;Island(8)               ;Bridge{v=false;d=true}    ;Island(4)];
      [Bridge{v=true;d=false}   ;Nothing                  ;Bridge{v=true;d=true}   ;Nothing                   ;Bridge{v=true;d=true}];
      [Island(3)                ;Bridge{v=false;d=true}   ;Island(5)               ;Bridge{v=false;d=false}   ;Island(3)]
    ]
    :solution);;



let commencerA = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |[] -> failwith "x1,y1 n'est pas dans puzzle"
    |((x2,y2),i)::t-> if x2=x1 && y1=y2 then t else aux t
  in aux puzzle;;

let trouverIleVerticaleBas = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2<>x1 && y2=y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |_::t -> aux t
  in aux (commencerA (x1,y1) puzzle);;

let trouverIleVerticaleHaut = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2<>x1 && y2=y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |_::t -> aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;


let trouverIleHorizontaleDroite = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2=x1 && y2<>y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |_::t ->aux t
  in aux (commencerA (x1,y1) (puzzle));;

let trouverIleHorizontaleGauche = fun (x1,y1) puzzle ->
  let rec aux = fun l ->
    match l with
    |((x2,y2),Island i)::t-> if x2=x1 && y2<>y1 then Some ((x2,y2),Island i) else aux t
    |[]-> None
    |_::t->aux t
  in aux (commencerA (x1,y1) (List.rev (puzzle)));;

let nbrIleVoisine = fun (x1,y1) p ->
  (if (trouverIleHorizontaleGauche (x1,y1) (p) )<>None then 1 else 0)
  +
    (if (trouverIleHorizontaleDroite (x1,y1) (p))<>None then 1 else 0)
  +
    (if (trouverIleVerticaleHaut (x1,y1) (p))<>None then 1 else 0)
  +
    (if (trouverIleVerticaleBas (x1,y1) (p))<>None then 1 else 0);;


let tranf = fun p ->
  let rec aux = fun  p l ->
    match p with
    |((x1,y1),0)::t -> aux t (l@[((x1,y1),Nothing)])
    |((x1,y1),i)::t -> aux t (l@[((x1,y1),Island(i))])
    | [] -> l
  in aux p [];;

let puztranf =  [
    ((0, 0), Nothing);  ((0, 1), Nothing); ((0, 2), Island 2); ((0, 3), Nothing); ((0, 4), Nothing);
    ((1, 0), Nothing);  ((1, 1), Nothing); ((1, 2), Nothing);  ((1, 3), Nothing); ((1, 4), Nothing);
    ((2, 0), Island 3); ((2, 1), Nothing); ((2, 2), Island 8); ((2, 3), Nothing); ((2, 4), Island 4);
    ((3, 0), Nothing);  ((3, 1), Nothing); ((3, 2), Nothing);  ((3, 3), Nothing); ((3, 4), Nothing);
    ((4, 0), Island 3); ((4, 1), Nothing); ((4, 2), Island 5); ((4, 3), Nothing); ((4, 4), Island 3)
  ];;




let remplacerValPar= fun  (x1,y1) repl p ->
  let rec aux = fun  p l->
    match p with
    |((x2,y2),smth)::t -> if x1=y1 && x2=y2 then l@[((x2,y2),repl)]@t else aux t (l@[((x2,y2),smth)]) 
    |[]->l
  in aux p [];;

(*supposons que le premier point soit le point le plus a gauche ou le plus en haut des deux*)
let tracerPont =fun (x1,y1) (x2,y2) pont puzl -> (* (0,2) (4,2) (Insland 10) puztranf *)
  let rec tracerVertical=fun p l ->
    match p with
    |((x3,y3),Nothing)::t-> if y3=y2 && x3>x1 && x3<x2 then tracerVertical t (((x3,y3),pont)::l)   else tracerVertical t (((x3,y3),Nothing)::l)
    |[]->l
    |(x,y)::t -> tracerVertical t ((x,y)::l)
  in
  let rec tracerHorizontal=fun p l ->
        match p with
    |((x3,y3),Nothing)::t-> if x3=x2 && y3>y1 && y3<y2 then tracerHorizontal t (((x3,y3),pont)::l)   else tracerHorizontal t (((x3,y3),Nothing)::l)
    |[]->l
    |(x,y)::t -> tracerHorizontal t ((x,y)::l)
  in
  if y2=y1 then List.rev (tracerVertical puzl [])
  else List.rev (tracerHorizontal puzl []);;


tracerPont (0,0) (0,4) (Island 10) puztranf;;


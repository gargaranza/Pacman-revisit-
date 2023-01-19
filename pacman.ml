#load "graphics.cma";;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;
Graphics.open_graph " 900x600";;

(* Déclaration des variables globales *)

let case_pacman = ref 0;;
let case_fantome = ref 0;;
let upleftx = ref 0;;
let uplefty = ref 0;;
let taille_case = ref 0;;
let l = ref 0;;
let h = ref 0;;


(* Union find simplement avec un tableau et sans notion de rang *)
let create n = Array.init n (fun i -> i);;

  (* On cherche le plus "ancien" parent, celui à la racine de l'arbre *)
let rec find uf n = match uf.(n) with
  | x when x = n -> n
  | x -> find uf x
;;

  (* On colle le deuxieme arbre au bout du premier, afin qu'ils ne fassent qu'un *)
let union uf n m =
  let x = find uf n in
  let y = find uf m in
  if x != y then uf.(x) <- y
;;


(* Partie pour générer le labyrinthe *)
  (* Pour générer le labyrinthe on a créé un tableau à trois entrée de taille asymétryque (on n'a pas pris en compte les murs extérieurs) :
    0 : vertical, (l-1)*h
    1 : horizontal l*(h-1) *)

let cases_adjacents (d,x,y) = match (d,x,y) with
  | (0,x,y) when x > (!l-1) || x < 0 || y > !h || y < 0 -> raise (Failure "Mur non existant")
  | (1,x,y) when x > !l || x < 0 || y > (!h-1) || y < 0 -> raise (Failure "Mur non existant")
  | (0,x,y) -> let n = x + y * !l in (n, n + 1)
  | (1,x,y) -> let n = x + y * !l in (n, n + !l)
  | _ -> raise (Failure "Direction non existante")
;;

  (* La fonction donnée dans l'énoncé ajustée *)
let generate_lab () =
  let voisines = Array.make (!l* !h) [] in
  let mur_present = Array.init 2 (fun x -> if x=0 then (Array.init (!l-1) (fun _ -> Array.make !h true)) else Array.init !l (fun _ -> Array.make (!h-1) true)) in
  let uf = create (!l* !h) in
  let k = ref 0 in
  while !k < (!l* !h)-1 do
    let m = ref (0,0,0) in
    begin
    if Random.int 2 = 0 then
      m := (0, Random.int (!l-1), Random.int !h)
    else m := (1, Random.int !l, Random.int (!h-1));
    end;
    let (i,j) = cases_adjacents !m in
    if find uf i != find uf j then
    begin
      union uf i j;
      let (d,x,y) = !m in
      mur_present.(d).(x).(y) <- false;
      k := !k +1;
      voisines.(i) <- j::voisines.(i);
      voisines.(j) <- i::voisines.(j)
    end
    else ();
  done;
  (mur_present,voisines)
;;


(* Toute la partie graphique *)

let trace_pourtour () =
  Graphics.moveto !upleftx !uplefty;
  Graphics.lineto (!upleftx + !taille_case * !l) !uplefty;
  Graphics.moveto (!upleftx + !taille_case * !l) !uplefty;
  Graphics.lineto (!upleftx + !taille_case * !l) (!uplefty - !taille_case * (!h-1));
  Graphics.moveto !upleftx (!uplefty - !taille_case);
  Graphics.lineto !upleftx (!uplefty - !taille_case * !h);
  Graphics.moveto !upleftx (!uplefty - !taille_case * !h);
  Graphics.lineto (!upleftx + !taille_case * !l) (!uplefty - !taille_case * !h)
;;

let trace_mur (d,x,y) =
  if d = 0 then
  begin
    Graphics.moveto (!upleftx + !taille_case * (x+1)) (!uplefty - !taille_case * y);
    Graphics.lineto (!upleftx + !taille_case * (x+1)) (!uplefty - !taille_case * (y+1))
  end
  else
  begin
    Graphics.moveto (!upleftx + !taille_case * x) (!uplefty - !taille_case * (y+1));
    Graphics.lineto (!upleftx + !taille_case * (x+1)) (!uplefty - !taille_case * (y+1))
  end
;;

let trace_lab mur_present =
  for i = 0 to (!l-2) do
    for j = 0 to (!h-1) do
      if mur_present.(0).(i).(j) then trace_mur (0,i,j)
    done
  done;
  for i = 0 to (!l-1) do
    for j = 0 to (!h-2) do
      if mur_present.(1).(i).(j) then trace_mur (1,i,j)
    done
  done
;;

let deplacement ancienne_case nouvelle_case couleur =
  let rayon_cercle = int_of_float (0.4 *. float_of_int !taille_case) in
  let ecart = int_of_float (0.1 *. float_of_int !taille_case) in
  Graphics.set_color Graphics.white;
  Graphics.fill_circle (ancienne_case mod !l * !taille_case + !upleftx + rayon_cercle + ecart) (!uplefty - ancienne_case / !l * !taille_case - rayon_cercle - ecart) rayon_cercle;
  Graphics.set_color couleur;
  Graphics.fill_circle (nouvelle_case mod !l * !taille_case + !upleftx + rayon_cercle + ecart) (!uplefty - nouvelle_case / !l * !taille_case - rayon_cercle - ecart) rayon_cercle
;;


(* Différentes fins *)

let finWin () =
  Graphics.clear_graph (); Graphics.set_color Graphics.black;
  Graphics.moveto (!upleftx + (!taille_case * !l) /2 - 100) (!uplefty - (!taille_case * !h) /2);
  Graphics.draw_string "Bien joue, tu as batu le fantome qui te traquait"
;;

let finLose () =
  Graphics.clear_graph (); Graphics.set_color Graphics.black;
  Graphics.moveto (!upleftx + (!taille_case * !l) /2 - 80) (!uplefty - (!taille_case * !h) /2);
  Graphics.draw_string "C'est dommage, tu y etais presque"
;;


(* Partie controle pacman *)

let main_pacman mur_present =
  while (!case_pacman != !case_fantome) && (!case_pacman <> (!l * !h - 1)) do
    let bouton = Graphics.read_key () in
    if (!case_pacman = !case_fantome) || (!case_pacman = (!l * !h - 1)) then raise Exit; (* fix bug comme quoi on pouvait continuer de jouer après avoir predu *)
    match bouton with
      |'z' when (!case_pacman / !l) <> 0 && mur_present.(1).(!case_pacman mod !l).(!case_pacman / !l - 1) = false -> deplacement !case_pacman (!case_pacman - !l) Graphics.blue; case_pacman := !case_pacman - !l
      |'q' when (!case_pacman mod !l) <> 0 && mur_present.(0).(!case_pacman mod !l - 1).(!case_pacman / !l) = false -> deplacement !case_pacman (!case_pacman - 1) Graphics.blue; case_pacman := !case_pacman - 1
      |'s' when (!case_pacman / !l) <> (!h - 1) && mur_present.(1).(!case_pacman mod !l).(!case_pacman / !l) = false -> deplacement !case_pacman (!case_pacman + !l) Graphics.blue; case_pacman := !case_pacman + !l
      |'d' when (!case_pacman mod !l) <> (!l - 1) && mur_present.(0).(!case_pacman mod !l).(!case_pacman / !l) = false -> deplacement !case_pacman (!case_pacman + 1) Graphics.blue; case_pacman := !case_pacman + 1
      | _ -> ()
  done;
  if !case_fantome == !case_pacman then finLose() else finWin()
;;


(* IA du fantome *)

  (* fonction donnée dans l'énoncé *)
let rec est_relie src dst evite voisines =
  let rec aux l = match l with
    | c::_ when c != evite && est_relie c dst src voisines -> true
    | hd::tl -> aux tl
    | _ -> false
  in
  if src = dst then true
  else aux voisines.(src)
;;

let case_suivante_fantome voisines =
  let rec aux l = match l with
    | case::tl when est_relie !case_fantome !case_pacman case voisines -> aux tl
    | case::_ -> case
    | [] -> raise (Failure "Le fantome ne peut plus avancer, il n'as pas trouvé de chemin correct")
  in aux voisines.(!case_fantome)
;;

let main_ghost voisines =
  while (!case_fantome != !case_pacman) && (!case_pacman != (!l* !h - 1)) do
    Unix.sleepf 0.6;
    let case_suivante = case_suivante_fantome voisines in
    deplacement !case_fantome case_suivante Graphics.red;
    case_fantome := case_suivante
  done;
  if !case_fantome == !case_pacman then finLose() else finWin()
;;


(* Main *)

let init uplx uply tc (tll,tlh) =
  (* initialisation des variables globales *)
  upleftx := uplx; uplefty := uply; taille_case := tc; l := tll; h := tlh; case_fantome := (tll-1);

  (* partie labyrinthe *)
  let (mur_present, voisines) = generate_lab() in
  trace_lab mur_present;
  trace_pourtour();

  (* positionnement du fantome et du pacman *)
  deplacement !case_fantome !case_fantome Graphics.red;
  deplacement !case_pacman !case_pacman Graphics.blue;

  (* lancement de la partie *)
  Thread.create main_ghost voisines;
  main_pacman mur_present
;;

init 20 420 25 (15,10);;

Graphics.read_key();;

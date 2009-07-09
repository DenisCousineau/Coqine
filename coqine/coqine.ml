(*****************************************************************************************************)
(*****************************************   CoqInE    ***********************************************)
(*****************************************************************************************************)

open Environ
open Pp
open Util
open System
open Flags
open Names
open Term
open Check
open Typeops

exception RuleDoesNotExist
exception Typehasnotype
exception NotASort
exception NotACoqVar
exception AnonymousCoqVar
exception NotImplementedYet
exception ShouldNotAppear
exception EmptyArrayInApp


(********** EUROPA Syntax ***************)

type qid =
  | Id of string
  | Qid of string * string

type euterm =
| EType
| EKind
| EVar of qid
| EPi of qid * euterm * euterm
| EFun of qid * euterm * euterm
| EApp of euterm * euterm

type line =
| Declaration of qid * euterm
| Rule of (qid * euterm) list * euterm * euterm
| End


(*********** Translation constr to euterm  *************)

let which_dotpi r = match r with
  | Prop Null,Prop Null  -> "dotpipp"
  | Prop Null,Prop Pos   -> "dotpips"
  | Prop Null,Type _     -> "dotpipt"
  | Prop Pos,Prop Null   -> "dotpisp"
  | Type _,Prop Null     -> "dotpitp"
  | Prop Pos,Type _      -> "dotpist"
  | Type _,Prop Pos      -> "dotpits"
  | Prop Pos,Prop Pos    -> "dotpiss"
  | Type _,Type _        -> "dotpitt"


(*let get_sort e t = match (infer_type e t) with
  | Sort s -> s
  | _ -> raise NotASort
*)

let get_dotpi e n t1 t2 =
  let e1 =  push_rel (n,None,t1) e in
    which_dotpi (infer_type e t1, infer_type e1 t2)

let which_e s = match s with
  | Prop Pos  -> "eset"
  | Prop Null -> "eprop"
  | Type _    -> "etype"

let get_e e t = which_e (infer_type e t)

let name_to_string n = match n with
  | Anonymous -> "_"
  | Name s -> s


let get_identifier n = match n with
  | Anonymous -> "_"
  | Name s -> s

let name_to_qid n = Id (string_of_id (get_identifier n))


(*** as a term ***)

let rec term_trans_aux e i t = match t with
  | Rel n -> EVar (Id (try List.nth i (n-1)
		       with Failure("nth") ->
			 failwith (Printf.sprintf "var %d of %d" n (List.length i))
))

  | Var v  -> EVar(Id v)

  | Meta _ -> raise ShouldNotAppear

  | Evar _ -> raise ShouldNotAppear

  | Sort s -> (match s with
                 | Prop Null -> EVar (Qid ("Coq1univ","dotprop"))
                 | Prop Pos ->  EVar (Qid ("Coq1univ","dotset"))
                 | Type _ ->    EVar (Qid ("Coq1univ","dottype")))  (*** !!! Attention a Type 0 ***)

  | Cast (_,_,_)  -> raise NotImplementedYet

  | Prod (n,t1,t2)  -> let t_tt1 = term_trans_aux e i t1 and e1 = push_rel (n,None,t1) e in
                              EApp ((EApp (EVar(Qid("Coq1univ",get_dotpi e n t1 t2)), t_tt1)),
                                 (EFun ((name_to_qid n),
                                        (EApp(EVar(Qid ("Coq1univ",get_e e t1)),t_tt1)),
                                        (term_trans_aux e1 ((name_to_string n)::i) t2))))

  | Lambda (n,t1,t2)  ->  (EFun ((Id (string_of_id (name_to_string n))),
                                        (EApp(EVar(Qid ("Coq1univ",get_e e t1)),term_trans_aux e i t1)),
                                        (term_trans_aux (push_rel (n,None,t1) e) ((name_to_string n)::i) t2)))

  | LetIn (_,_,_,_)  -> raise NotImplementedYet

  | App (t1,a)  -> Array.fold_left (fun u1 u2 -> EApp(u1,term_trans_aux e i u2))
      (term_trans_aux e i t1) a

  | Const(mp,dp,l)  -> (* TODO: treat the module path and the dir path *)
      EVar(Id l)

  | Ind _  -> raise NotImplementedYet

  | Construct  _  -> raise NotImplementedYet

  | Case (_,_,_,_)  -> raise NotImplementedYet

  | Fix  _  -> raise NotImplementedYet

  | CoFix   _  -> raise NotImplementedYet



let term_trans t = term_trans_aux empty_env [] t

(*** as a type ***)

let rec type_trans_aux e i t = match t with
  | Sort s -> (match s with
                   | Prop Pos  -> EVar(Qid("Coq1univ.eu","Uset"))
                   | Prop Null -> EVar(Qid("Coq1univ.eu","Uprop"))
                   | Type _    -> EVar(Qid("Coq1univ.eu","Utype")))

  | Prod(n,t1,t2) ->  let t_tt1 = type_trans_aux e i t1 and e1 = push_rel (n,None,t1) e in
      EPi((name_to_qid n),t_tt1,(type_trans_aux e1 ((name_to_string n)::i) t2))

  | t -> EApp(EVar(Qid("Coq1univ.eu",get_e e t)),(term_trans_aux e i t))


let type_trans t = type_trans_aux empty_env [] t




(************* Translation module_body to EUline list *********************)

(*
type structure_field_body =
  | SFBconst of constant_body
  | SFBmind of mutual_inductive_body
  | SFBmodule of module_body
  | SFBalias of module_path * struct_expr_body option * Univ.constraints option
  | SFBmodtype of module_type_body

and structure_body = (label * structure_field_body) list

and struct_expr_body =
  | SEBident of module_path
  | SEBfunctor of mod_bound_id * module_type_body * struct_expr_body
  | SEBstruct of mod_self_id * structure_body
  | SEBapply of struct_expr_body * struct_expr_body
      * Univ.constraints
  | SEBwith of struct_expr_body * with_declaration_body

and with_declaration_body =
    With_module_body of identifier list * module_path *
      struct_expr_body option * Univ.constraints
  | With_definition_body of  identifier list * constant_body

and module_body =
    { mod_expr : struct_expr_body option;
      mod_type : struct_expr_body option;
      mod_constraints : Univ.constraints;
      mod_alias : substitution;
      mod_retroknowledge : action list}

and module_type_body =
    { typ_expr : struct_expr_body;
      typ_strength : module_path option;
      typ_alias : substitution}


type constant_body = {
    const_hyps : section_context; (* New: younger hyp at top *)
    const_body : constr_substituted option;
    const_type : constant_type;
    const_body_code : to_patch_substituted;
   (* const_type_code : Cemitcodes.to_patch; *)
    const_constraints : Univ.constraints;
    const_opaque : bool;
    const_inline : bool}

*)



(******** PRETTYPRINT ********)

let get_euname n = match n with
  | Id s -> s
  | Qid (_,s) -> s

let rec ast_to_str t = match t with
  | EType -> " Type "
  | EKind -> " Kind "
  | EVar n -> get_euname n
  | EPi (n,t1,t2) -> " (" ^ (get_euname n) ^ " : " ^ (ast_to_str t1) ^ " -> " ^ (ast_to_str t2) ^ ") "
  | EFun (n,t1,t2) -> " (" ^ (get_euname n) ^ " : " ^ (ast_to_str t1) ^ " => " ^ (ast_to_str t2) ^ ") "
  | EApp (t1,t2) -> " (" ^ (ast_to_str t1) ^ " " ^ (ast_to_str t2) ^ ") "

let pprint expres = Printf.printf "%s" (ast_to_str expres)

let output_term out_chan t = output_string out_chan (ast_to_str t)

let output_decl out_chan (i,t) =
  output_string out_chan (get_euname i);
  output_string out_chan " : ";
  output_term out_chan t

let output_line out_chan = function
  | Declaration(i,t) -> output_decl out_chan (i,t);
      output_string out_chan ".\n"
  | Rule(var_decls, t1, t2) ->
      output_string out_chan "[";
      List.iter (output_decl out_chan) var_decls;
      output_string out_chan "] ";
      output_term out_chan t1;
      output_string out_chan " --> ";
      output_term out_chan t2;
      output_string out_chan ".\n"
  | End -> output_string out_chan "\n"




(*** TESTS ***)

(*let idprop = (Lambda_a ((Name "x"),(Sort_a Prop_a), (Var_a (Name "x", Sort_a Prop_a)),
                  Prod_a ((Name "x"),(Var_a (Name "x", Sort_a Prop_a)), Sort_a Prop_a, Sort_a Prop_a)))

let idprop_te = term_translation idprop

let pipropprop = (Prod_a ((Name "x"),(Sort_a Prop_a),(Sort_a Prop_a),(Sort_a Type_a)))

let pipropprop_te = term_translation pipropprop
let pipropprop_ty = type_translation pipropprop

let _ =  print_endline " ";
         print_endline (ast_to_str idprop_te);
         print_endline " ";
         print_endline (ast_to_str pipropprop_te);
         print_endline " ";
         print_endline (ast_to_str pipropprop_ty);
         print_endline " ";
*)

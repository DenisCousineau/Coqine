exception RuleDoesNotExist
exception Typehasnotype
exception NotASort
exception NotACoqVar
exception AnonymousCoqVar
exception NotImplementedYet
exception ShouldNotAppear
exception EmptyArrayInApp
type qid = Id of string | Qid of string * string
type euterm =
    EType
  | EKind
  | EVar of qid
  | EPi of qid * euterm * euterm
  | EFun of qid * euterm * euterm
  | EApp of euterm * euterm
type line =
    Declaration of qid * euterm
  | Rule of (qid * euterm) list * euterm * euterm
  | End
val which_dotpi : Term.sorts * Term.sorts -> string
val get_dotpi : Environ.env -> Term.constr -> Term.constr -> string
val which_e : Term.sorts -> string
val get_e : Environ.env -> Term.constr -> string
val name_to_string : Names.name -> Names.identifier
val get_identifier : Names.name -> Names.identifier
val name_to_qid : Names.name -> qid
val term_trans_aux : Environ.env -> 'a -> Term.constr -> euterm
val term_trans : Term.constr -> euterm
val type_trans_aux : Environ.env -> 'a -> Term.constr -> euterm
val type_trans : Term.constr -> euterm
val get_euname : qid -> string
val ast_to_str : euterm -> string
val pprint : euterm -> unit

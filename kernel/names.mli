type identifier = string
val id_ord : 'a -> 'a -> int
val id_of_string : string -> string
val string_of_id : string -> string
module Hident :
  sig
    type t = string
    type u = string -> string
    val f : unit -> u -> t -> t
  end
module IdOrdered : sig type t = identifier val compare : 'a -> 'a -> int end
module Idset :
  sig
    type elt = IdOrdered.t
    type t = Set.Make(IdOrdered).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module Idmap :
  sig
    type key = IdOrdered.t
    type 'a t = 'a Map.Make(IdOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module Idpred :
  sig
    type elt = IdOrdered.t
    type t = Predicate.Make(IdOrdered).t
    val empty : t
    val full : t
    val is_empty : t -> bool
    val is_full : t -> bool
    val mem : elt -> t -> bool
    val singleton : elt -> t
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val complement : t -> t
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val elements : t -> bool * elt list
  end
type name = Name of identifier | Anonymous
type module_ident = identifier
type dir_path = module_ident list
module ModIdmap :
  sig
    type key = IdOrdered.t
    type 'a t = 'a Map.Make(IdOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val make_dirpath : 'a -> 'a
val repr_dirpath : 'a -> 'a
val empty_dirpath : 'a list
val string_of_dirpath : string list -> string
val u_number : int ref
type uniq_ident = int * string * dir_path
val make_uid : 'a -> string -> int * string * 'a
val debug_string_of_uid : int * string * 'a -> string
val string_of_uid : 'a * string * string list -> string
module Umap :
  sig
    type key = uniq_ident
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type label = string
type mod_self_id = uniq_ident
val make_msid : 'a -> string -> int * string * 'a
val repr_msid : 'a * 'b * 'c -> 'a * 'b * 'c
val debug_string_of_msid : int * string * 'a -> string
val refresh_msid : 'a * string * 'b -> int * string * 'b
val string_of_msid : 'a * string * string list -> string
val id_of_msid : 'a * 'b * 'c -> 'b
val label_of_msid : 'a * 'b * 'c -> 'b
type mod_bound_id = uniq_ident
val make_mbid : 'a -> string -> int * string * 'a
val repr_mbid : 'a * 'b * 'c -> 'a * 'b * 'c
val debug_string_of_mbid : int * string * 'a -> string
val string_of_mbid : 'a * string * string list -> string
val id_of_mbid : 'a * 'b * 'c -> 'b
val label_of_mbid : 'a * 'b * 'c -> 'b
val mk_label : 'a -> 'a
val string_of_label : string -> string
val id_of_label : 'a -> 'a
val label_of_id : 'a -> 'a
module Labset :
  sig
    type elt = IdOrdered.t
    type t = Set.Make(IdOrdered).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module Labmap :
  sig
    type key = IdOrdered.t
    type 'a t = 'a Map.Make(IdOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type module_path =
    MPfile of dir_path
  | MPbound of mod_bound_id
  | MPself of mod_self_id
  | MPdot of module_path * label
val check_bound_mp : module_path -> bool
val string_of_mp : module_path -> string
val mp_ord : module_path -> module_path -> int
module MPord :
  sig
    type t = module_path
    val compare : module_path -> module_path -> int
  end
module MPset :
  sig
    type elt = MPord.t
    type t = Set.Make(MPord).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module MPmap :
  sig
    type key = MPord.t
    type 'a t = 'a Map.Make(MPord).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type kernel_name = module_path * dir_path * label
val make_kn : 'a -> 'b -> 'c -> 'a * 'b * 'c
val repr_kn : 'a -> 'a
val modpath : 'a * 'b * 'c -> 'a
val label : 'a * 'b * 'c -> 'c
val string_of_kn : module_path * string list * string -> string
val pr_kn : module_path * string list * string -> Pp.std_ppcmds
val kn_ord : module_path * 'a * 'b -> module_path * 'a * 'b -> int
module KNord :
  sig
    type t = kernel_name
    val compare : module_path * 'a * 'b -> module_path * 'a * 'b -> int
  end
module KNmap :
  sig
    type key = KNord.t
    type 'a t = 'a Map.Make(KNord).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module KNpred :
  sig
    type elt = KNord.t
    type t = Predicate.Make(KNord).t
    val empty : t
    val full : t
    val is_empty : t -> bool
    val is_full : t -> bool
    val mem : elt -> t -> bool
    val singleton : elt -> t
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val complement : t -> t
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val elements : t -> bool * elt list
  end
module KNset :
  sig
    type elt = KNord.t
    type t = Set.Make(KNord).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module Cmap :
  sig
    type key = KNord.t
    type 'a t = 'a Map.Make(KNord).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module Cpred :
  sig
    type elt = KNord.t
    type t = Predicate.Make(KNord).t
    val empty : t
    val full : t
    val is_empty : t -> bool
    val is_full : t -> bool
    val mem : elt -> t -> bool
    val singleton : elt -> t
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val complement : t -> t
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val elements : t -> bool * elt list
  end
module Cset :
  sig
    type elt = KNord.t
    type t = Set.Make(KNord).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val default_module_name : string
val initial_dir : string list
val initial_msid : int * string * string list
val initial_path : module_path
type variable = identifier
type constant = kernel_name
type mutual_inductive = kernel_name
type inductive = mutual_inductive * int
type constructor = inductive * int
val constant_of_kn : 'a -> 'a
val make_con : 'a -> 'b -> 'c -> 'a * 'b * 'c
val repr_con : 'a -> 'a
val string_of_con : module_path * string list * string -> string
val con_label : 'a * 'b * 'c -> 'c
val pr_con : module_path * string list * string -> Pp.std_ppcmds
val con_modpath : 'a * 'b * 'c -> 'a
val mind_modpath : 'a * 'b * 'c -> 'a
val ind_modpath : ('a * 'b * 'c) * 'd -> 'a
val constr_modpath : (('a * 'b * 'c) * 'd) * 'e -> 'a
val ith_mutual_inductive : 'a * 'b -> 'c -> 'a * 'c
val ith_constructor_of_inductive : 'a -> 'b -> 'a * 'b
val inductive_of_constructor : 'a * 'b -> 'a
val index_of_constructor : 'a * 'b -> 'b
module InductiveOrdered :
  sig
    type t = inductive
    val compare :
      (module_path * 'a * 'b) * int -> (module_path * 'a * 'b) * int -> int
  end
module Indmap :
  sig
    type key = InductiveOrdered.t
    type 'a t = 'a Map.Make(InductiveOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module ConstructorOrdered :
  sig
    type t = constructor
    val compare :
      ((module_path * 'a * 'b) * int) * int ->
      ((module_path * 'a * 'b) * int) * int -> int
  end
module Constrmap :
  sig
    type key = ConstructorOrdered.t
    type 'a t = 'a Map.Make(ConstructorOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type evaluable_global_reference =
    EvalVarRef of identifier
  | EvalConstRef of constant
module Hname :
  sig
    type t = name
    type u = identifier -> identifier
    val f : unit -> u -> t -> t
  end
module Hdir :
  sig
    type t = dir_path
    type u = identifier -> identifier
    val f : unit -> u -> t -> t
  end
module Huniqid :
  sig
    type t = uniq_ident
    type u = (string -> string) * (dir_path -> dir_path)
    val f : unit -> u -> t -> t
  end
module Hmod :
  sig
    type t = module_path
    type u =
        (dir_path -> dir_path) * (uniq_ident -> uniq_ident) *
        (string -> string)
    val f : unit -> u -> t -> t
  end
module Hkn :
  sig
    type t = kernel_name
    type u =
        (module_path -> module_path) * (dir_path -> dir_path) *
        (string -> string)
    val f : unit -> u -> t -> t
  end
val hcons_names :
  unit ->
  (Hkn.t -> Hkn.t) * (Hkn.t -> Hkn.t) * (Hdir.t -> Hdir.t) *
  (Hname.t -> Hname.t) * (Hident.t -> Hident.t) *
  (Hashcons.Hstring.t -> Hashcons.Hstring.t)
type transparent_state = Idpred.t * Cpred.t
val empty_transparent_state : Idpred.t * Cpred.t
val full_transparent_state : Idpred.t * Cpred.t
val var_full_transparent_state : Idpred.t * Cpred.t
val cst_full_transparent_state : Idpred.t * Cpred.t
type 'a tableKey = ConstKey of constant | VarKey of identifier | RelKey of 'a
type inv_rel_key = int
type id_key = inv_rel_key tableKey

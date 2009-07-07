open Coqine
open Declarations
open Check

let translate_structure_body (name, decl) = match decl with
    SFBconst sbfc -> begin
      let tterm = match sbfc.const_body with
	  Some cb -> begin
	    match !cb with
		LSval c -> term_trans c 
	      | LSlazy(s,c) -> failwith "not implemented: lazy subst"
	  end
	| None -> failwith "no term given"
      and ttype = match sbfc.const_type with 
	  NonPolymorphicType t -> type_trans t
	| _ -> failwith "not implemented: polymorphic types"
      in
	output_line stdout (Declaration(Id name, ttype));
	output_line stdout (Rule([],EVar(Id name), tterm));
    end
  | _ -> failwith "not a const"


let translate filename =
  let channel = open_in_bin filename in
    ignore (input_binary_int channel); (* magic number *)
    let (md:Check.library_disk) = Marshal.from_channel channel in
      close_in channel;
      let (_,mb,_,_) = md.md_compiled in
	match mb.mod_expr with 
	    Some (SEBstruct (label, declarations)) ->
	      List.iter translate_structure_body declarations;
	      print_endline (";Finished module " ^ match label with _,l,_ -> l)
	  | _ -> ()
	      
let _ =  Arg.parse [] translate
  "CoqInE\nUsage: coqine filenames\n\tfilenames: coq binary files (.vo)" 

module ZFS_Tools.ASTLens

module AST   = FStar.Parser.AST
module Id    = FStar.Ident
module Const = FStar.Const

type Comment = string * FStar.Range.range
type AST     = AST.modul * list<Comment>

type constdata =
    | CD_bool      of bool
    | CD_int       of string * option<(Const.signedness * Const.width)> (* When None, means "mathematical integer", i.e. Prims.int. *)
    | CD_char      of char                                              (* unicode code point: char in F#, int in OCaml *)
    | CD_string    of string                                            (* UTF-8 encoded *)
    | CD_invalid

let modify_term' (newdata : constdata) (t : AST.term') : AST.term' =
    match t with
    | AST.Const(Const.Const_bool(_))     ->
        match newdata with
        | CD_bool(b)               -> AST.Const(Const.Const_bool(b))
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_int(_,t))    ->
        match newdata with
        | CD_int(s,t') when t = t' -> AST.Const(Const.Const_int(s,t)) 
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_char(_))     ->
        match newdata with
        | CD_char(c)               -> AST.Const(Const.Const_char(c))
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_string(_,r)) ->
        match newdata with
        | CD_string(s)             -> AST.Const(Const.Const_string(s,r))
        | _                        -> failwith "TODO"
    | _                                  -> failwith "TODO"

let modify_term (newdata : constdata) (t : AST.term) : AST.term =
    { t with tm = modify_term' newdata (t.tm) }

let check_name (name : string) (p : AST.pattern) : bool =
    match p.pat with
    | AST.PatVar(v,_) -> v.idText = name
    | _               -> false

let modify_pattern_term (name : string) (newdata : constdata) ((p,t) : AST.pattern * AST.term) : AST.pattern * AST.term =
    if check_name name p
        then (p, modify_term newdata t)
        else (p, t)

let modify_decl' (name : string) (newdata : constdata) (dec' : AST.decl') : AST.decl' =
    match dec' with
    | AST.TopLevelLet(q, [pt]) -> AST.TopLevelLet(q, [modify_pattern_term name newdata pt])
    | _ -> dec'

let modify_decl (name : string) (newdata : constdata) (dec : AST.decl) : AST.decl =
    { dec with d = modify_decl' name newdata dec.d }

let modify_modul (name : string) (newdata : constdata) (m : AST.modul) : AST.modul =
    match m with
    | AST.Module(lid, decs)       -> AST.Module(lid, List.map (modify_decl name newdata) decs) 
    | AST.Interface(lid, decs, b) -> AST.Interface(lid, List.map (modify_decl name newdata) decs, b)

let modify_AST (name : string) (newdata : constdata) ((m, cmts) : AST) : AST =
    (modify_modul name newdata m, cmts)

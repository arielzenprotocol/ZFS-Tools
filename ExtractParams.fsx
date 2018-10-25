
#r "bin/Debug/basic.dll"
#r "bin/Debug/parser.dll"
#r "bin/Debug/ZFS_Tools.dll"

module AST = FStar.Parser.AST
module Id = FStar.Ident
module Const = FStar.Const

let FILENAME = "../../zfc/contracts/Bet/Bet.fst"

//for arg in fsi.CommandLineArgs do
//    printfn "%s" arg

type constdata =
    | CD_bool      of bool
    | CD_int       of string * option<(Const.signedness * Const.width)> (* When None, means "mathematical integer", i.e. Prims.int. *)
    | CD_char      of char (* unicode code point: char in F#, int in OCaml *)
    | CD_string    of string (* UTF-8 encoded *)
    | CD_invalid

//type sconst =
//  | Const_effect
//  | Const_unit
//  | Const_bool        of bool
//  | Const_int         of string * option<(signedness * width)> (* When None, means "mathematical integer", i.e. Prims.int. *)
//  | Const_char        of char (* unicode code point: char in F#, int in OCaml *)
//  | Const_float       of double
//  | Const_bytearray   of array<byte> * Range.range
//  | Const_string      of string * Range.range                (* UTF-8 encoded *)
//  | Const_range_of                                           (* `range_of` primitive *)
//  | Const_set_range_of                                       (* `set_range_of` primitive *)
//  | Const_range       of Range.range                         (* not denotable by the programmer *)
//  | Const_reify                                              (* a coercion from a computation to a Tot term *)
//  | Const_reflect     of Ident.lid                           (* a coercion from a Tot term to an l-computation type *)

let getDeclerations ast =
    match fst ast with
    | AST.Module(_, decls)       -> decls
    | AST.Interface(_, decls, _) -> decls

let filterTLLs: list<AST.decl> -> list<list<AST.pattern * AST.term>> =
    List.choose (
        function
        | {AST.decl.d = AST.decl'.TopLevelLet(_, pts)} -> Some pts
        | _                                            -> None
    )

let filterSingletons : list<list<AST.pattern * AST.term>> -> list<AST.pattern' * AST.term'> =
    List.choose (
        function
        | [(p,t)] -> Some (p.pat, t.tm)
        | _       -> None
    )

let filterConstants : list<AST.pattern' * AST.term'> -> list<Id.ident * Const.sconst> =
    List.choose (
        function
        | (AST.PatVar (v, _), AST.Const c) -> Some (v, c)
        | _                                -> None 
    )

let getIdText : list<Id.ident * Const.sconst> -> list<string * Const.sconst> =
    List.map ( fun (id, e) -> (id.idText, e) )

let cvtConstData : Const.sconst -> constdata =
    function
    | Const.Const_bool(b)         -> CD_bool(b)
    | Const.Const_int(s,t)        -> CD_int(s,t)
    | Const.Const_char(c)         -> CD_char(c)
    | Const.Const_string(s,_)     -> CD_string(s)
    | Const.Const_unit
    | Const.Const_float(_)
    | Const.Const_bytearray(_,_)
    | Const.Const_effect
    | Const.Const_range_of
    | Const.Const_set_range_of
    | Const.Const_range(_)
    | Const.Const_reify
    | Const.Const_reflect(_)      -> CD_invalid

let cvtConstDatas : list<string * Const.sconst> -> list<string * constdata> =
    List.map ( fun (s, e) -> (s, cvtConstData e) ) 

let ast = ASTUtils.parse_file FILENAME

printfn "%A" (
    ast |> getDeclerations
        |> filterTLLs
        |> filterSingletons
        |> filterConstants
        //|> getIdText
        //|> cvtConstDatas
    )

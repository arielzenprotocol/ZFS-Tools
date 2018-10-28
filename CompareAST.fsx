
#r "bin/Debug/basic.dll"
#r "bin/Debug/parser.dll"
#r "bin/Debug/ZFS_Tools.dll"

module AST = FStar.Parser.AST

type Comment = string * FStar.Range.range
type AST = AST.modul * list<Comment>

let compareASTs ((m1,_) : AST) ((m2,_) : AST) : bool =
     WitheredAST.cvt_modul m1 = WitheredAST.cvt_modul m2

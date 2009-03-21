-- Functions to render encoded Javascript statements and expressions
-- into a interpretable Javascript

module JS.Show where

import JS.Jcode
import Data.List
import Data.Char
import Numeric
import Prim

-- A rendering function for Jcode (not pretty, w/o indents)

strJcode (JComment s) = "// " ++ s ++ "\n"
strJcode (JMlcomm []) = ""
strJcode (JMlcomm ss) = "/*\n" ++ concat (intersperse "\n" ss) ++ "\n*/\n"
strJcode (JStat e) = strJexp e ++ ";\n"
strJcode (JRaw s) = s
strJcode (JAssign lhs rhs) = strJexp lhs ++ "=" ++ strJexp rhs ++ ";\n"
strJcode (JAssignV lhs rhs) = "var " ++ strJexp lhs ++ "=" ++ strJexp rhs ++ ";\n"
strJcode (JTry jtry cv jcatch jfin) = 
  "try {\n" ++ concat (map strJcode jtry) ++ "}\n" ++
  "catch (" ++ cv ++ ") {\n" ++ concat (map strJcode jcatch) ++ "}\n" ++
  "finally {\n" ++ concat (map strJcode jfin) ++ "}\n"
strJcode (JReturn (JCall (JFunc [] exp) [])) = strJcode (JReturn exp)
strJcode (JReturn (JCall (JProc [] jss) [])) = concat (map strJcode jss)
strJcode (JReturn ret) = "return " ++ strJexp ret ++ ";\n"
strJcode (JSwitch exp cases) = "switch(" ++ strJexp exp ++ ") {\n" ++ 
  concat (map onecase cases) ++ "}\n"
    where onecase (Just cexp, ccode, brk) = "case " ++ strJexp cexp ++ " : " ++
            concat (map strJcode ccode) ++ 
            if brk then "break;\n" else ""
          onecase (Nothing, ccode, _) = "default : " ++ concat (map strJcode ccode)
strJcode (JFunction name args body) = "function " ++ name ++ "(" ++ 
                                       concat (intersperse ", " args) ++ ")" ++
                                      "{\n" ++ concat (map strJcode body) ++ "}\n"


-- A function to render Javascript expressions

strJexp JEmpty = "undefined"
strJexp (JStr s)  = s
strJexp (JNum s)  = s
strJexp (JObject flds) = "{" ++ 
                       concat (intersperse ",\n" (map strfield flds)) ++ "}"
  where strfield (f, e) = f ++ ":" ++ strJexp e

strJexp (JCall (JStr "new HSData") ((JStr con):_)) | con == "true" || con == "false" = con

strJexp (JCall (JFunc fargs (JStr fret)) _) | not (fret `elem` fargs) = fret

strJexp (JCall fun args) = (strJexp fun) ++ "(" ++ concat (intersperse ", " (map strJexp args)) ++ ")"

strJexp (JFunc args (JCall (JFunc [] exp) [])) = strJexp (JFunc args exp)

strJexp (JFunc args (JCall (JProc [] jcs) [])) = strJexp (JProc args jcs)

strJexp (JFunc args exp) = "function(" ++ concat (intersperse ", " args) ++ 
                           "){return " ++ strJexp exp ++ ";}"

strJexp (JProc args jcs) = "function(" ++ concat (intersperse ", " args) ++
                           "){\n" ++ concat (map strJcode jcs) ++ "}"


strJexp (JComma l r) = "(" ++ strJexp l ++ "," ++ strJexp r ++ ")"

strJexp (JPrim (pn) args@(l:r:_)) =  
  let pref o = "(" ++ o ++ "(" ++ strJexp l ++ "))"
      jxpn (JNum n) = n
      jxpn e = strJexp (JCall (JStr "Number") [e])
      inf1 o = "(" ++ strJexp l ++ ")" ++ o ++ "(" ++ strJexp r ++ ")" 
      inf2 o = "(" ++ jxpn    l ++ ")" ++ o ++ "(" ++ jxpn    r ++ ")" in
  case pn of
    "SEQ"   -> "(exprEval(" ++ strJexp l ++ ")," ++ strJexp r ++ ")"
    "QUOT"  -> "function(x,y){return (x - (x % y))/y;}(" ++ jxpn l ++ "," ++ jxpn r ++ ")"
    "ADD_W" -> inf1 "+"
    "ADD_F" -> inf1 "+"
    "ADD_D" -> inf1 "+"
    "SUB_W" -> inf1 "-"
    "SUB_F" -> inf1 "-"
    "SUB_D" -> inf1 "-"
    "MUL_W" -> inf1 "*"
    "MUL_F" -> inf1 "*"
    "MUL_D" -> inf1 "*"
    "NEG_W" -> pref "-"
    "NEG_F" -> pref "-"
    "NEG_D" -> pref "-"
    "EQ_W"  -> inf2 "==="
    "EQ_F"  -> inf2 "==="
    "EQ_D"  -> inf2 "==="
    "NE_W"  -> inf2 "!=="
    "NE_F"  -> inf2 "!=="
    "NE_D"  -> inf2 "!=="
    "GT_W"  -> inf2 ">"
    "GT_F"  -> inf2 ">"
    "GT_D"  -> inf2 ">"
    "LT_W"  -> inf2 "<"
    "LT_F"  -> inf2 "<"
    "LT_D"  -> inf2 "<"
    "GE_W"  -> inf2 ">="
    "GE_F"  -> inf2 ">="
    "GE_D"  -> inf2 ">="
    "LE_W"  -> inf2 "<="
    "LE_F"  -> inf2 "<="
    "LE_D"  -> inf2 "<="
    "SLASH_D" -> inf2 "/"
    "SLASH_F" -> inf2 "/"
    "REM"   -> inf1 "%"
--    "SEQ"   -> strJexp (JCall (JStr "exprEval") [l]
    other -> strJexp (JCall (JStr (" /*???*/ " ++ pn)) args)

strJexp (JPrim prim args) = prim ++ "(" ++
                         concat (intersperse "," (map strJexp args)) ++ ")"

strJexp (JMethod meth obj args) = "(" ++ strJexp obj ++ ")." ++ meth ++ "(" ++
                                  concat (intersperse ", " (map strJexp args)) ++ ")"

strJexp (JMember memb obj) = "(" ++ strJexp obj ++ ")." ++ memb

strJexp (JArray es) = "[" ++ concat (intersperse ", " (map strJexp es)) ++ "]"

strJexp (JCond e1 e2 e3) = "(" ++ strJexp e1 ++ ")?(" ++ 
                           strJexp e2 ++ "):(" ++ strJexp e3 ++ ")"

strJexp (JInfix oper e1 e2) = "(" ++ strJexp e1 ++ ") " ++ oper ++ " (" ++ strJexp e2  ++ ")"

strJexp (JIndex arr idx) = strJexp arr ++ "[" ++ strJexp idx ++ "]"

instance Show Prim where
  show = strPrim

-- A function to render Javascript string literals.

showJstr "" = ""
showJstr (c:cs) = (showjsc c) ++ (showJstr cs) where
  showjsc c | isAlphaNum c || c `elem` "{}[]() <=>_.,/;:!|+-@#$%^&*()~`"  = [c]
  showjsc c | c `elem` "\'\"\\" = '\\':[c]
  showjsc c | (ord c) > 127 && (ord c) < 256 = "\\x" ++ lpad '0' 2 (showHex (ord c) "")
  showjsc c = "\\u" ++ lpad '0' 4 (showHex (ord c) "")
  lpad p n s = let npad = n - length s in
    case npad > 0 of
      True -> (replicate npad p) ++ s
      False -> s


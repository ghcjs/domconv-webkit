module Prim where

data PrimOp = OpWord | OpFloat | OpDouble deriving (Eq,Show,Ord)

data Prim =
      ADD       PrimOp
    | SUB       PrimOp
    | MUL       PrimOp
    | ABS       PrimOp
    | SIGNUM    PrimOp
    | EXP       PrimOp
    | POW       PrimOp
    | LOG       PrimOp
    | SQRT      PrimOp
    | SIN       PrimOp
    | COS       PrimOp
    | TAN       PrimOp
    | ASIN      PrimOp
    | ACOS      PrimOp
    | ATAN      PrimOp
    | SLASH     PrimOp
    | CMP_EQ    PrimOp  -- EQ is used by 1.3 in in Ordering
    | CMP_NE    PrimOp
    | CMP_LT    PrimOp  -- LT is used by 1.3 in in Ordering
    | CMP_LE    PrimOp
    | CMP_GT    PrimOp  -- GT is used by 1.3 in in Ordering
    | CMP_GE    PrimOp
    | NEG       PrimOp
    | QUOT
    | REM
    | AND
    | OR
    | NOT
    | ORD
    | CHR
    | SEQ
    | STRING   -- NR
    | CATCH
    | HGETS    -- MW
    | HGETC    -- NR
    | HPUTC    -- NR
    -- -- | QUOTREM     -- WITHDRAWN
    -- -- | DIVMOD      -- WITHDRAWN
    -- -- | DIV         -- WITHDRAWN
    -- -- | MOD         -- WITHDRAWN
    deriving (Eq)

strPrim :: Prim -> String
strPrim (ADD    op)     = strPrimOp "ADD"    op
strPrim (SUB    op)     = strPrimOp "SUB"    op
strPrim (MUL    op)     = strPrimOp "MUL"    op
strPrim (ABS    op)     = strPrimOp "ABS"    op
strPrim (SIGNUM op)     = strPrimOp "SIGNUM" op
strPrim (EXP    op)     = strPrimOp "EXP"    op
strPrim (POW    op)     = strPrimOp "POW"    op
strPrim (LOG    op)     = strPrimOp "LOG"    op
strPrim (SQRT   op)     = strPrimOp "SQRT"   op
strPrim (SIN    op)     = strPrimOp "SIN"    op
strPrim (COS    op)     = strPrimOp "COS"    op
strPrim (TAN    op)     = strPrimOp "TAN"    op
strPrim (ASIN   op)     = strPrimOp "ASIN"   op
strPrim (ACOS   op)     = strPrimOp "ACOS"   op
strPrim (ATAN   op)     = strPrimOp "ATAN"   op
strPrim (SLASH  op)     = strPrimOp "SLASH"  op
strPrim (CMP_EQ op)     = strPrimOp "EQ"     op
strPrim (CMP_NE op)     = strPrimOp "NE"     op
strPrim (CMP_LT op)     = strPrimOp "LT"     op
strPrim (CMP_LE op)     = strPrimOp "LE"     op
strPrim (CMP_GT op)     = strPrimOp "GT"     op
strPrim (CMP_GE op)     = strPrimOp "GE"     op
strPrim (NEG    op)     = strPrimOp "NEG"    op
strPrim QUOT            = "QUOT"
strPrim REM             = "REM"
strPrim AND             = "AND"
strPrim OR              = "OR"
strPrim NOT             = "NOT"
strPrim ORD             = "ORD"
strPrim CHR             = "CHR"
strPrim SEQ             = "SEQ"
strPrim STRING          = "STRING"
strPrim CATCH           = "CATCH"
strPrim HGETS           = "HGETS"
strPrim HGETC           = "HGETC"
strPrim HPUTC           = "HPUTC"

strPrimOp :: String -> PrimOp -> String
strPrimOp mnem OpWord   = mnem ++ "_W"
strPrimOp mnem OpFloat  = mnem ++ "_F"
strPrimOp mnem OpDouble = mnem ++ "_D"

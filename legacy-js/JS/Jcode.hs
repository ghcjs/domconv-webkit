-- Jcode datatype: to represent Javascript on per-statement basis.

module JS.Jcode where

data Jcode  
  = JComment String                           -- renders as '// anycomment'
  | JMlcomm [String]                          -- renders as multiline comment within /* */
  | JStat Jexp                                -- renders as a non-assignment statement
  | JRaw String                               -- renders as raw Javascript
  | JAssign  Jexp Jexp                        -- renders as assignment (LHS = RHS;)
  | JAssignV Jexp Jexp                        -- renders as variable declaration with assignment
  | JSwitch  Jexp [(Maybe Jexp,[Jcode],Bool)] -- renders as a switch statements
  | JTry     [Jcode] String [Jcode] [Jcode]   -- try ... catch ... finally
  | JFunction String [String] [Jcode]         -- toplevel-declared multistatement funciton
  | JReturn  Jexp                             -- renders as a return statement
  deriving (Show)

data Jexp
  = JEmpty                                    -- empty expression
  | JStr String                               -- variable/function identifier/immediate value
  | JNum String                               -- specifically for numeric values
  | JObject [(String, Jexp)]                  -- object initializer
  | JPrim String [Jexp]                       -- primitive call on arguments
  | JCall Jexp [Jexp]                         -- function call on arguments
  | JFunc [String] Jexp                       -- nested function declaration
  | JProc [String] [Jcode]                    -- procedure
  | JMethod String Jexp [Jexp]                -- method invocation
  | JMember String Jexp                       -- member access
  | JArray [Jexp]                             -- array
  | JIndex Jexp Jexp                          -- array indexed access
  | JCond Jexp Jexp Jexp                      -- conditional exp1 ? exp2 : exp3
  | JInfix String Jexp Jexp                   -- infix operator
  | JComma Jexp Jexp                          -- comma operator
  deriving (Show)


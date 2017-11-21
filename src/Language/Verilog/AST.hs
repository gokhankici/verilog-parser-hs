module Language.Verilog.AST where

data Module = Module { moduleStmts  :: [Stmt] }

data Id = Id String


data SingleAsgn = SingleAsgn { saLval          :: Lval
                             , saExp           :: Exp
                             , saDriveStrength :: DriveStrength
                             , saDelay         :: Delay
                             }

data TimingControl = TCDelayParam     { delayParam :: Id  }
                   | TCDelaySpecParam { delayParam :: Id  }
                   | TCDelayNo        { delayNo :: Number }
                   | TCDelayRange     { mintypmax :: Delay }
                   | TCNoneEvent      
                   | TCAnyEvent       
                   | TCTriggerEvent   { events :: [EventExp] }
                   | TCEventRepeat    { repeatExpr :: Exp }

data EventExp = PosEdge { timingExp :: Exp }
              | NegEdge { timingExp :: Exp }
              | NoEdge  { timingExp :: Exp }

data Lval = SpecParam { lvalId :: Id }
          | Param     { lvalId :: Id }
          | Net       { lvalId :: Id }
          | Var       { lvalId :: Id }
          | Genvar    { lvalId :: Id }
          | NetConcat { lvalRep :: Exp
                      , lvalItems :: [Lval]
                      }
          | VarConcat { lvalRep :: Exp,
                        lvalItems :: [Lval]
                      }

-- attributes ?
-- module path expressions ?
data Exp = NumberExp   { numberExp :: Number }
         | IdExp       { idExp :: Id }
         | ConcatExp   { expRepeat :: Exp
                       , exps :: [Exp]
                       }
         | FuncCallExp { funcName :: Id
                       , funcArgs :: [Exp]
                       }
         | StringExp   { strExp :: String }
         | UnaryExp    { unaryOp  :: UnaryOp
                       , unaryExp :: Exp
                       }
         | BinaryExp   { binaryOp :: BinaryOp
                       , lhs :: Exp
                       , rhs :: Exp
                       }
         | TernaryExp  { cond :: Exp     -- c ? e1 : e2
                       , thenExp :: Exp
                       , elseExp :: Exp
                       }
         | RangeExp    { lhs :: Exp
                       , rhs :: Exp
                       }
         | DelayExp    { delayExp :: Delay }
         | IndexExp    { arrayExp :: Exp
                       , indexExp :: Exp
                       }

data Delay = Delay { minDelayExp     :: Exp
                   , typicalDelayExp :: Exp
                   , maxDelayExp     :: Exp
                   }

data UnaryOp = Op_LogicNeg
             | Op_BitNeg

data BinaryOp = Op_Plus
              | Op_Minus   
              | Op_Times
              | Op_ArithShiftLeft
              | Op_ArithShiftRight
              | Op_LogicShiftLeft
              | Op_LogicShiftRight
              | Op_Div
              | Op_Pow
              | Op_Mod
              | Op_Gte
              | Op_Lte
              | Op_Gt
              | Op_Lt
              | Op_LogicAnd
              | Op_LogicOr
              | Op_LogicEq
              | Op_LogicNeq
              | Op_CaseEq
              | Op_CaseNeq
              | Op_BitAnd
              | Op_BitNand
              | Op_BitOr
              | Op_BitXor
              | Op_BitNor
              | Op_BitEq

data Number = Number  

data DriveStrength = StrengthHighZ0
                   | StrengthHighZ1
                   | StrengthSupply0
                   | StrengthSupply1
                   | StrengthStrong0
                   | StrengthStrong1
                   | StrengthPull0
                   | StrengthPull1
                   | StrengthWeak0
                   | StrengthWeak1

data CaseType = Case | CaseX | CaseZ

data Block = SeqBlock      { blockId    :: Id
                           , blockStmts :: [Stmt]
                           }
           | InitialBlock  { blockId    :: Id
                           , blockStmts :: [Stmt]
                           }
           | AlwaysBlock   { blockId      :: Id
                           , blockStmts   :: [Stmt]
                           , blockTrigger :: TimingControl
                           }
           | FunctionBlock { blockId    :: Id
                           , blockStmts :: [Stmt]
                           }
           | ParallelBlock { blockId    :: Id
                           , blockStmts :: [Stmt]
                           }

data Asgn = ContinuousAsgn  { assignments :: [SingleAsgn] }
          | BlockingAsgn    { procLval    :: Lval
                            , procExp     :: Exp
                            , procTCS     :: TimingControl
                            }
          | NonBlockingAsgn { procLval :: Lval
                            , procExp  :: Exp
                            , procTCS  :: TimingControl
                            }

data Stmt = WaitStmt        { waitDuration :: Exp
                            , waitStmt     :: Stmt
                            }
          | FuncCall        { funcCallExp :: Exp }
          | TCStmt          { tc     :: TimingControl
                            , tcStmt :: Stmt
                            } 
          | IfStmt          { condition :: Exp
                            , thenStmt  :: Stmt
                            , elseStmt  :: Stmt
                            }
          | CaseStmt        { caseType    :: CaseType
                            , caseExp     :: Exp
                            , cases       :: [(Exp, Stmt)]
                            , defaultStmt :: Stmt
                            }
          | BlockStmt       { blockStmt :: Block }
          | DeclStmt        { declStmt  :: Decl }
          | AsgnStmt        { asgnStmt  :: Asgn }
          | ModuleInst          -- HERE

data Decl = VarDecl
          | ParamDecl
          | PortDecl
          | NetDecl
          | RegDecl  

module SkelStretch where

-- Haskell module generated by the BNF converter

import AbsStretch
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  ProgramEntry stms -> failure x
transBoolean :: Boolean -> Result
transBoolean x = case x of
  Boolean_true -> failure x
  Boolean_false -> failure x
transLiteral :: Literal -> Result
transLiteral x = case x of
  LiteralUnit -> failure x
  LiteralBoolean boolean -> failure x
  LiteralInteger integer -> failure x
  LiteralString string -> failure x
transType :: Type -> Result
transType x = case x of
  TyIdent ident -> failure x
  TyUnit -> failure x
  TyBool -> failure x
  TyInt -> failure x
  TyString -> failure x
  TyFun types type_ -> failure x
transStm :: Stm -> Result
transStm x = case x of
  SFunc ident formalargs block -> failure x
  SFuncRet ident formalargs type_ block -> failure x
  SStruct ident formalargs -> failure x
  SLet ident exp -> failure x
  SLetType ident type_ exp -> failure x
  SBlockExp blockexp -> failure x
  SExp exp -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block1 stms -> failure x
  Block2 stms exp -> failure x
transBlockExp :: BlockExp -> Result
transBlockExp x = case x of
  EBlock block -> failure x
  EIf exp block -> failure x
  EIfElse exp block1 block2 -> failure x
  EWhile exp block -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EAssign ident exp -> failure x
  EOr exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  ENEq exp1 exp2 -> failure x
  ELess exp1 exp2 -> failure x
  ELEq exp1 exp2 -> failure x
  EGreat exp1 exp2 -> failure x
  EGEq exp1 exp2 -> failure x
  EPlus exp1 exp2 -> failure x
  EMinus exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  ENot exp -> failure x
  ENeg exp -> failure x
  ELit literal -> failure x
  EIdent ident -> failure x
  EStruct ident memberinits -> failure x
  ECall exp exps -> failure x
  EPrint exp -> failure x
  EField exp ident -> failure x
  EBlockExp blockexp -> failure x
  EAnonFun anonfunc -> failure x
  ETyped exp type_ -> failure x
transAnonFunc :: AnonFunc -> Result
transAnonFunc x = case x of
  AnonEmpty block -> failure x
  AnonArgs formalargs block -> failure x
transMemberInit :: MemberInit -> Result
transMemberInit x = case x of
  MemberExp ident exp -> failure x
transFormalArg :: FormalArg -> Result
transFormalArg x = case x of
  TypedIdent ident type_ -> failure x


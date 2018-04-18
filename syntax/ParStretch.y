-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParStretch where
import AbsStretch
import LexStretch
import ErrM

}

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%name pProgram Program
%name pItem Item
%name pListItem ListItem
%name pStm Stm
%name pListStm ListStm
%name pBoolean Boolean
%name pLiteral Literal
%name pBExp1 BExp1
%name pBExp2 BExp2
%name pBExp3 BExp3
%name pBExp4 BExp4
%name pBExp BExp
%name pAExp1 AExp1
%name pAExp2 AExp2
%name pAExp3 AExp3
%name pAExp4 AExp4
%name pAExp AExp
%name pExp Exp
%name pMemberInit MemberInit
%name pListMemberInit ListMemberInit
%name pActualArg ActualArg
%name pListActualArg ListActualArg
%name pFormalArg FormalArg
%name pListFormalArg ListFormalArg
%name pBlock Block
%name pType Type
%name pListType ListType
%name pDecl Decl
%name pListDecl ListDecl
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '(' { PT _ (TS _ 3) }
  ')' { PT _ (TS _ 4) }
  '*' { PT _ (TS _ 5) }
  '+' { PT _ (TS _ 6) }
  ',' { PT _ (TS _ 7) }
  '-' { PT _ (TS _ 8) }
  '->' { PT _ (TS _ 9) }
  '.' { PT _ (TS _ 10) }
  '/' { PT _ (TS _ 11) }
  ':' { PT _ (TS _ 12) }
  ';' { PT _ (TS _ 13) }
  '<' { PT _ (TS _ 14) }
  '<=' { PT _ (TS _ 15) }
  '=' { PT _ (TS _ 16) }
  '==' { PT _ (TS _ 17) }
  '>' { PT _ (TS _ 18) }
  '>=' { PT _ (TS _ 19) }
  'Fn' { PT _ (TS _ 20) }
  'bool' { PT _ (TS _ 21) }
  'drop' { PT _ (TS _ 22) }
  'else' { PT _ (TS _ 23) }
  'false' { PT _ (TS _ 24) }
  'fn' { PT _ (TS _ 25) }
  'if' { PT _ (TS _ 26) }
  'int' { PT _ (TS _ 27) }
  'let' { PT _ (TS _ 28) }
  'print' { PT _ (TS _ 29) }
  'string' { PT _ (TS _ 30) }
  'struct' { PT _ (TS _ 31) }
  'true' { PT _ (TS _ 32) }
  'while' { PT _ (TS _ 33) }
  '{' { PT _ (TS _ 34) }
  '|' { PT _ (TS _ 35) }
  '}' { PT _ (TS _ 36) }

  L_ident {PT _ (TV $$)}
  L_integ {PT _ (TI $$)}
  L_quoted {PT _ (TL $$)}

%%

Ident :: {
  Ident 
}
: L_ident {
  Ident $1 
}

Integer :: {
  Integer 
}
: L_integ {
  read $1 
}

String :: {
  String 
}
: L_quoted {
  $1 
}

Program :: {
  Program 
}
: ListItem {
  AbsStretch.Program1 (reverse $1)
}

Item :: {
  Item 
}
: Stm {
  AbsStretch.ItemStm $1 
}
| Decl {
  AbsStretch.ItemDecl $1 
}

ListItem :: {
  [Item]
}
: {
  [] 
}
| ListItem Item {
  flip (:) $1 $2 
}

Stm :: {
  Stm 
}
: 'let' Ident '=' Exp ';' {
  AbsStretch.SLet $2 $4 
}
| 'let' Ident ':' Type '=' Exp ';' {
  AbsStretch.SLetType $2 $4 $6 
}
| 'drop' Ident ';' {
  AbsStretch.SDrop $2 
}
| Exp ';' {
  AbsStretch.SExp $1 
}

ListStm :: {
  [Stm]
}
: {
  [] 
}
| ListStm Stm ';' {
  flip (:) $1 $2 
}

Boolean :: {
  Boolean 
}
: 'true' {
  AbsStretch.Boolean_true 
}
| 'false' {
  AbsStretch.Boolean_false 
}

Literal :: {
  Literal 
}
: Integer {
  AbsStretch.LiteralInteger $1 
}
| String {
  AbsStretch.LiteralString $1 
}
| Boolean {
  AbsStretch.LiteralBoolean $1 
}
| '(' ')' {
  AbsStretch.Literal1 
}

BExp1 :: {
  BExp 
}
: BExp1 '==' BExp2 {
  AbsStretch.BExp11 $1 $3 
}
| BExp1 '!=' BExp2 {
  AbsStretch.BExp12 $1 $3 
}
| BExp2 {
  $1 
}

BExp2 :: {
  BExp 
}
: BExp2 '<' BExp3 {
  AbsStretch.BExp21 $1 $3 
}
| BExp2 '<=' BExp3 {
  AbsStretch.BExp22 $1 $3 
}
| BExp2 '>' BExp3 {
  AbsStretch.BExp23 $1 $3 
}
| BExp2 '>=' BExp3 {
  AbsStretch.BExp24 $1 $3 
}
| BExp3 {
  $1 
}

BExp3 :: {
  BExp 
}
: '!' BExp3 {
  AbsStretch.BExp31 $2 
}
| BExp4 {
  $1 
}

BExp4 :: {
  BExp 
}
: AExp {
  AbsStretch.BExp4AExp $1 
}
| '(' BExp ')' {
  $2 
}

BExp :: {
  BExp 
}
: BExp1 {
  $1 
}

AExp1 :: {
  AExp 
}
: AExp1 '+' AExp2 {
  AbsStretch.AExp11 $1 $3 
}
| AExp1 '-' AExp2 {
  AbsStretch.AExp12 $1 $3 
}
| AExp2 {
  $1 
}

AExp2 :: {
  AExp 
}
: AExp2 '*' AExp3 {
  AbsStretch.AExp21 $1 $3 
}
| AExp2 '/' AExp3 {
  AbsStretch.AExp22 $1 $3 
}
| AExp3 {
  $1 
}

AExp3 :: {
  AExp 
}
: '-' AExp3 {
  AbsStretch.AExp31 $2 
}
| AExp4 {
  $1 
}

AExp4 :: {
  AExp 
}
: Exp {
  AbsStretch.AExp4Exp $1 
}
| '(' AExp ')' {
  $2 
}

AExp :: {
  AExp 
}
: AExp1 {
  $1 
}

Exp :: {
  Exp 
}
: Literal {
  AbsStretch.ELit $1 
}
| Ident {
  AbsStretch.EIdent $1 
}
| Exp '(' ListActualArg ')' {
  AbsStretch.ECall $1 $3 
}
| 'print' '(' ActualArg ')' {
  AbsStretch.ECallPrint $3 
}
| Exp '.' Ident {
  AbsStretch.EField $1 $3 
}
| Ident '{' ListMemberInit '}' {
  AbsStretch.EStruct $1 $3 
}
| Exp '=' BExp {
  AbsStretch.EAssign $1 $3 
}
| 'if' BExp Block {
  AbsStretch.EIf $2 $3 
}
| 'if' BExp Block 'else' Block {
  AbsStretch.EIfElse $2 $3 $5 
}
| 'while' BExp Block {
  AbsStretch.EWhile $2 $3 
}
| Block {
  AbsStretch.EBlock $1 
}
| '|' ListFormalArg '|' Block {
  AbsStretch.EAnonFunc $2 $4 
}

MemberInit :: {
  MemberInit 
}
: Ident ':' Exp {
  AbsStretch.MemberInit $1 $3 
}

ListMemberInit :: {
  [MemberInit]
}
: {
  [] 
}
| MemberInit {
  (:[]) $1 
}
| MemberInit ',' ListMemberInit {
  (:) $1 $3 
}

ActualArg :: {
  ActualArg 
}
: Exp {
  AbsStretch.ActualArg $1 
}

ListActualArg :: {
  [ActualArg]
}
: {
  [] 
}
| ActualArg {
  (:[]) $1 
}
| ActualArg ',' ListActualArg {
  (:) $1 $3 
}

FormalArg :: {
  FormalArg 
}
: Ident ':' Type {
  AbsStretch.FormalArg $1 $3 
}

ListFormalArg :: {
  [FormalArg]
}
: {
  [] 
}
| FormalArg {
  (:[]) $1 
}
| FormalArg ',' ListFormalArg {
  (:) $1 $3 
}

Block :: {
  Block 
}
: '{' ListItem '}' {
  AbsStretch.Block1 (reverse $2)
}
| '{' ListItem BExp '}' {
  AbsStretch.Block2 (reverse $2)$3 
}

Type :: {
  Type 
}
: Ident {
  AbsStretch.TypeIdent $1 
}
| '(' ')' {
  AbsStretch.Type1 
}
| 'bool' {
  AbsStretch.Type_bool 
}
| 'int' {
  AbsStretch.Type_int 
}
| 'string' {
  AbsStretch.Type_string 
}
| 'Fn' '(' ListType ')' '->' Type {
  AbsStretch.Type2 $3 $6 
}

ListType :: {
  [Type]
}
: {
  [] 
}
| Type {
  (:[]) $1 
}
| Type ',' ListType {
  (:) $1 $3 
}

Decl :: {
  Decl 
}
: 'fn' Ident '(' ListFormalArg ')' Block {
  AbsStretch.DFunc $2 $4 $6 
}
| 'fn' Ident '(' ListFormalArg ')' '->' Type Block {
  AbsStretch.DFuncRet $2 $4 $7 $8 
}
| 'struct' Ident '{' ListFormalArg '}' {
  AbsStretch.DStruct $2 $4 
}

ListDecl :: {
  [Decl]
}
: {
  [] 
}
| ListDecl Decl {
  flip (:) $1 $2 
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens


}


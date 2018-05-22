module Typeck
    ( typeck
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Monad.Except
import Control.Monad.State

import Util(traceShowM)
import BNFC.AbsStretch
import BNFC.PrintStretch(Print, printTree)

maybeToEither :: a -> Maybe a1 -> Either a a1
maybeToEither = flip maybe Right . Left

type Result a = Either String a

type TypeCheckMonad a = StateT TypeEnv (Either String) a
type TypeCheck a = a -> TypeCheckMonad a

type TypeEnv = Map Ident Type

errorIn :: Print a => a -> String -> String
errorIn inside msg =
    "In `" ++ (printTree inside) ++ "`:\n" ++
    "  " ++ msg

mismatch :: Print a => a -> String -> Type -> Type -> String
mismatch inside msg expected got =
    "In `" ++ (printTree inside) ++ "`:\n" ++
    "  " ++ msg ++ "\n" ++
    "  expected `" ++ (show expected) ++ "`, got `" ++ (show got) ++ "`"

-- Given an initial state, runs a StateT in a local variable binding environment
runInLocalEnv :: TypeCheckMonad a -> TypeEnv -> TypeCheckMonad a
runInLocalEnv st initial = liftEither $ evalStateT st initial

typeck :: Program -> Result Program
typeck prog @ (ProgramEntry stmts) =
    evalStateT (ProgramEntry <$> mapM typeckStm stmts) (Map.empty)

typeckEvalFuncArg :: TypeCheck FormalArg
typeckEvalFuncArg arg @ (TypedIdent ident typ) = do
    typ <- typeckType typ
    modify(\st -> Map.insert ident typ st) >> return arg

typeckStm :: TypeCheck Stm
typeckStm (SFunc ident args block) =
    -- Functions have implicit return value ()
    typeckStm (SFuncRet ident args TyUnit block)

typeckStm (SFuncRet ident args retType block) =
    let mapStateWithIdent ident = mapStateT (liftM (\(typ, s) -> ((TypedIdent ident typ),s))) in
        let typeckArgs x = (sequence $ map (\(TypedIdent i t) -> mapStateWithIdent i $ typeckType t) x) in do
            -- Check args and return type before introducing the actual function type itself
            args <- typeckArgs args
            retType <- typeckType retType

            -- Introduce type definition for the function so it's visible
            -- inside the type-checked block and after the func def itself
            let argTypes = map(\(TypedIdent _ typ) -> typ) args in
                modify(\env -> Map.insert ident (TyFun argTypes retType) env)

            block @ (Block2 _ (ETyped _ bodyType)) <-
                runInLocalEnv (mapM (typeckEvalFuncArg) args >> typeckBlock block) =<< get

            traceShowM =<< get

            if bodyType == retType then return (SFuncRet ident args retType block)
                                   else throwError $ mismatch ident "Mismatched types in function" retType bodyType

typeckStm stm @ (SStruct ident args) = do
    -- traceShowM =<< get
    checkedArgs <- mapM typeckType $ map (\(TypedIdent _ ty) -> ty) args

    modify(\env -> Map.insert ident (TyStruct ident args) env)

    -- traceShowM =<< get

    return stm

typeckStm (SLet ident expr) = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr

    modify(\env -> Map.insert ident typ env)

    return $ SLet ident typedExpr

typeckStm stm @ (SLetType ident declaredType expr) = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr

    modify(\env -> Map.insert ident typ env)

    if not $ declaredType == typ then throwError $ mismatch stm "Mismatched types" declaredType typ
                                 else return $ SLetType ident typ typedExpr

typeckStm (SBlockExp blockExpr) = SBlockExp <$> typeckBlockExpr blockExpr
typeckStm (SExp expr) = SExp <$> typeckExp expr

-- always returns ETyped variant
typeckExp :: TypeCheck Exp
typeckExp expr = do
    (typedExpr, typ) <- ti expr
    return (ETyped typedExpr typ)

-- if (else), while guard clauses need to be boolean
checkGuardClause :: TypeCheck Exp
checkGuardClause expr = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr

    if typ /= TyBool then throwError $ mismatch expr "guard clause is not a boolean" TyBool typ
                        else return typedExpr

typeckBlockExpr :: TypeCheck BlockExp
typeckBlockExpr (EBlock block) = EBlock <$> typeckBlock block
typeckBlockExpr (EIf expr block) = EIf <$> checkGuardClause expr <*> typeckBlock block
typeckBlockExpr blk @ (EIfElse expr trueBlock falseBlock) = do
    typedExpr <- checkGuardClause expr

    typedTrueBlk  @ (Block2 _ (ETyped _ trueType))  <- typeckBlock trueBlock
    typedFalseBlk @ (Block2 _ (ETyped _ falseType)) <- typeckBlock falseBlock

    if trueType == falseType then return $ (EIfElse typedExpr typedTrueBlk typedFalseBlk)
                             else throwError $ mismatch blk "Mismatched types in if-true-else block" trueType falseType

typeckBlockExpr (EWhile expr block) = EWhile <$> checkGuardClause expr <*> typeckBlock block

typeckBlock :: TypeCheck Block
-- Add implicit () return value for blocks without last expression
typeckBlock (Block1 stmts) = typeckBlock (Block2 stmts (ELit LiteralUnit))
typeckBlock (Block2 stmts expr) = do
    -- Evaluate trailing expression in a local env (can depend on a statement in an inner block)
    typedExpr <- runInLocalEnv (mapM (typeckStm) stmts >> typeckExp expr) =<< get
    return (Block2 stmts typedExpr)

typeckType :: TypeCheck Type
typeckType (TyIdent ident @ (Ident identStr)) = do
    env <- get
    case Map.lookup ident env of
        Just x -> return x
        Nothing -> throwError $ "No struct definition found for `" ++ identStr ++ "`"
typeckType ty = do return ty -- every other type is valid

typeckExpExpect :: Type -> TypeCheck Exp
typeckExpExpect expected expr = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr

    if typ /= expected then throwError $ mismatch expr "mismatched operand type" expected typ
                       else return typedExpr

binOp :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Type -> Type -> TypeCheckMonad (Exp, Type)
binOp constr left right inType outType = do
    expr <- constr <$> typeckExpExpect inType left <*> typeckExpExpect inType right
    return (expr, outType)
unOp :: (Exp -> Exp) -> Exp -> Type -> Type -> TypeCheckMonad (Exp, Type)
unOp constr expr inType outType = do
    expr <- constr <$> typeckExpExpect inType expr
    return (expr, outType)

ti :: Exp -> StateT TypeEnv (Either String) (Exp, Type)
ti (EAssign ident expr) = do
    expr <- typeckExp expr
    return (EAssign ident expr, TyUnit)

ti (EOr left right) = binOp EOr left right TyBool TyBool
ti (EAnd left right) = binOp EAnd left right TyBool TyBool
ti op @ (EEq left right) = do
    lExp @ (ETyped _ lType) <- typeckExp left
    rExp @ (ETyped _ rType) <- typeckExp right
    if lType /= rType then throwError $ mismatch op "`=` operands must be same type" lType rType
                      else return $ (EEq lExp rExp, TyBool)

ti (ENEq left right) = ti (ENot (EEq left right))

ti (ELess left right) = binOp ELess left right TyInt TyBool
ti (ELEq left right) = binOp ELEq left right TyInt TyBool
ti (EGreat left right) = binOp EGreat left right TyInt TyBool
ti (EGEq left right) = binOp EGEq left right TyInt TyBool

ti (EPlus left right) = binOp EPlus left right TyInt TyInt
ti (EMinus left right) = binOp EMinus left right TyInt TyInt
ti (EMul left right) = binOp EMul left right TyInt TyInt
ti (EDiv left right) = binOp EDiv left right TyInt TyInt

ti (ENot expr) = unOp ENot expr TyBool TyBool
ti (ENeg expr) = unOp ENeg expr TyInt TyInt

ti lit @ (ELit LiteralUnit) = return (lit, TyUnit)
ti lit @ (ELit (LiteralBoolean _)) = return (lit, TyBool)
ti lit @ (ELit (LiteralInteger _)) = return (lit, TyInt)
ti lit @ (ELit (LiteralString _)) = return (lit, TyString)

ti (EIdent ident @ (Ident var)) = do
    env <- get
    typ <- liftEither $ maybeToEither ("Unbound variable `" ++ var ++ "`") (Map.lookup ident env)
    return (EIdent ident, typ)

ti str @ (EStruct ident members) = do
    env <- get
    (TyStruct _ declaredMembers) <- let errMsg = ("`" ++ (printTree ident) ++ "` is not a struct") in
        liftEither $ maybeToEither errMsg (Map.lookup ident env)

    checkedMembers <- sequence $ map (\(MemberExp ident e) -> mapStateT (liftM (\(val,s) -> ((ident,val),s))) $ typeckExp e) members
    typedMembers <- return $ map (\(ident, ETyped expr ty) -> (ident, ty)) checkedMembers

    declaredMembers <- sequence $ map (\(TypedIdent ident ty) -> mapStateT (liftM (\(val,s) -> ((ident,val),s))) $ typeckType ty) declaredMembers

    let a = Map.fromList typedMembers in
        let b = Map.fromList declaredMembers in
            if a /= b then throwError (errorIn str "Mismatched struct args:\n" ++
                    "  expected `" ++ (show b) ++ "`\n  found `" ++ (show a) ++ "`")
                else let initMembers = map (\(i, e) -> MemberExp i e) checkedMembers in
                    let formalArgs = map (\(i, ty) -> TypedIdent i ty) typedMembers in
                        return $ (EStruct ident initMembers, TyStruct ident formalArgs)

ti (ECall expr callArgs) = do
    func @ (ETyped _ funcType) <- typeckExp expr

    TyFun declaredArgs retValue <- case funcType of
        fun @ (TyFun _ _)-> return fun
        _ -> throwError $ "`" ++ (printTree func) ++ "` is not a function"


    checkedCallArgs <- mapM (liftM (\x -> x) typeckExp) callArgs
    callTypedExps <- return $ map (\(ETyped _ ty) -> ty) checkedCallArgs

    when (declaredArgs /= callTypedExps) $ throwError "Mismatched types in function argument"

    return (ECall func checkedCallArgs, retValue)

ti (EPrint expr) = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr
    return $ (EPrint typedExpr, TyUnit)

ti e @ (EField expr ident) = do
    typedExpr @ (ETyped _ typ) <- typeckExp expr

    TypedIdent _ fieldType <- case typ of
        TyStruct _ args -> liftEither $ maybeToEither
            (errorIn e "No such field `" ++ (printTree ident) ++ "` on struct")
            (List.find (\(TypedIdent iden ty) -> iden == ident) args)
        _ -> throwError $ "`" ++ (printTree expr) ++ "` is not a struct"

    return (EField typedExpr ident, fieldType)

ti (EBlockExp blockExpr) = do
    typedBlockExpr <- typeckBlockExpr blockExpr
    typ <- case typedBlockExpr of
        EBlock (Block2 _ (ETyped _ typ)) -> return typ
        EIf _ (Block2 _ (ETyped _ typ)) -> return typ
        EIfElse _ (Block2 _ (ETyped _ typ)) _ -> return typ
        EWhile _ (Block2 _ (ETyped _ typ)) -> return typ
        _ -> fail $ "unreachable: internal typeck error"

    return $ (EBlockExp typedBlockExpr, typ)

ti (EAnonFun (AnonEmpty block)) = ti (EAnonFun (AnonArgs [] block))
ti (EAnonFun (AnonArgs args block)) = do
    let mapStateWithIdent ident = mapStateT (liftM (\(typ, s) -> ((TypedIdent ident typ),s))) in
        let typeckArgs x = (sequence $ map (\(TypedIdent i t) -> mapStateWithIdent i $ typeckType t) x) in do
            args <- typeckArgs args

            block @ (Block2 _ (ETyped _ bodyType)) <-
                runInLocalEnv (mapM (typeckEvalFuncArg) args >> typeckBlock block) =<< get

            let argTypes = map(\(TypedIdent ident typ) -> typ) args in
                return $ (EAnonFun (AnonArgs args block), TyFun argTypes bodyType)

ti (ETyped expr typ) = fail $ "unreachable: internal typeck error"

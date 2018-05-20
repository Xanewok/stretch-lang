import BNFC.AbsStretch

import Typeck(typeck)

unit :: Exp
unit = (ELit $ LiteralUnit)

true :: Exp
true = (ELit $ LiteralBoolean Boolean_true)

false :: Exp
false = (ELit $ LiteralBoolean Boolean_false)

int :: Integer -> Exp
int val = (ELit $ LiteralInteger val)

string :: String -> Exp
string val = (ELit $ LiteralString val)

testCases :: [Program]
testCases =
    map ProgramEntry
    [ [SLet (Ident "abc") (ELit LiteralUnit)] -- let abc = ();
    , [SBlockExp $ EBlock $ Block1 []] -- {}
    , [SStruct (Ident "MyStruct")  [TypedIdent (Ident "a") TyUnit]] -- struct MyStruct { a: () }
    , [SExp (ELit LiteralUnit)] -- ();
    , [SExp (ELit $ LiteralBoolean Boolean_false)] -- false;
    , [SExp (ELit $ LiteralInteger 42)] -- 42;
    , [SExp (ELit $ LiteralString "string")] -- "string";
    , [SFunc (Ident "myFunc") [] (Block1 [SExp (ELit $ LiteralInteger 42)])] -- fn myFunc() {42;}
    , [SLetType (Ident "dsa") TyUnit (ELit $ LiteralUnit)] -- let dsa: () = ();
    , [ SStruct (Ident "MyStruct") [TypedIdent (Ident "a") TyUnit] -- struct MyStruct { a: () }
      , SFunc (Ident "myFunc") [TypedIdent (Ident "arg") $ TyIdent (Ident "MyStruct")] (Block1 []) -- fn myFunc(arg: MyStruct) {}
      ]
    , [SFuncRet (Ident "myFunc") [] TyInt (Block2 [] (ELit $ LiteralInteger 42))] -- fn myFunc() -> int { 42 }
    , [SBlockExp (EIfElse true (Block1 []) (Block2 [] unit))] -- if true {} else { () }
    , [SBlockExp (EIfElse false (Block2 [] $ int 42) (Block2 [] $ int 28))] -- if false { 42 } else { 28 }
    , [SBlockExp (EWhile true (Block2 [] $ int 42))] -- while true { 42 }
    , [SExp (EPlus (int 2) (int 2))] -- 2 + 2
    , [ SLet (Ident "a") (int 4) -- let a = 4;
      , SLet (Ident "b") (EIdent (Ident "a")) -- let b = a;
      ]
    , [SLet (Ident "a") (EBlockExp (EBlock (Block1 [])))] -- let a = {};
    , [SLetType (Ident "a") TyInt (EBlockExp (EBlock (Block2 [] $ int 42)))] -- let a: int = {42};
    , [SLet (Ident "a") (EBlockExp (EIf true (Block1 [])))] -- let a = if true {};
    , [SLetType (Ident "a") TyInt (EBlockExp (EIfElse true (Block2 [] $ int 30) (Block2 [] $ int 42)))] -- let a: int = if true {30} else {42};
    , [ SLet (Ident "a") (EAnonFun (AnonEmpty (Block2 [] $ int 30))) -- let a = || {30};
      , SExp (ECall (EIdent (Ident "a")) []) -- a();
      ]
    , [ SLet (Ident "a") (EAnonFun (AnonArgs [TypedIdent (Ident "arg") TyString] (Block2 [] $ int 30))) -- let a = |arg: string| {30};
      , SLetType (Ident "b") TyInt (ECall (EIdent (Ident "a")) [string "test"]) -- let b: int = a("test");
      ]
    , [ SStruct (Ident "MyStruct")  [TypedIdent (Ident "a") TyUnit] -- struct MyStruct { a: () }
      , SLet (Ident "a") (EAnonFun (AnonArgs [TypedIdent (Ident "arg") $ TyIdent (Ident "MyStruct")] (Block2 [] $ int 30))) -- let a = |arg: MyStruct| {30};
      ]
    , [ SStruct (Ident "MyStruct")  [TypedIdent (Ident "a") TyInt] -- struct MyStruct { a: int }
      , SLet (Ident "a") (EStruct (Ident "MyStruct") [MemberExp (Ident "a") $ int 42]) -- let a = MyStruct { a: 42 };
      ]
    , [ SStruct (Ident "MyStruct")  [TypedIdent (Ident "a") TyInt] -- struct MyStruct { a: int }
      , SLet (Ident "a") (EField (EStruct (Ident "MyStruct") [MemberExp (Ident "a") $ int 42]) (Ident "a"))-- let a = MyStruct { a: 42 }.a;
      ]
    , [SExp (EPrint $ int 42)] -- print(42);
    , [SExp (EOr true false)] -- true || false
    , [SExp (ENeg (int 43))] -- let a: int = -43;
    ]

badCases :: [Program]
badCases =
  map ProgramEntry
  [ [SLetType (Ident "dsa") TyUnit (ELit $ LiteralInteger 42)] -- let dsa: () = 42;
  , [SLetType (Ident "a") TyInt unit] -- let a: int = ();
  , [SLetType (Ident "a") TyInt (EIdent (Ident "b"))] -- let a = b;
  , [SBlockExp (EIfElse unit (Block1 []) (Block1 []))] -- if () { } else { }
  , [SBlockExp (EIfElse true (Block2 [] $ int 43) (Block1 []))] -- if true { 43 } else { }
  , [SBlockExp (EWhile (int 5) (Block2 [] $ int 42))] -- while 5 { 42 }
  , [SExp (EPlus false (int 2))] -- false + 2
  , [ SFunc (Ident "abc") [] (Block1 [SStruct (Ident "Inner") []]) -- fn abc() {struct Inner{}}
    , SFunc (Ident "acceptsNested") [TypedIdent (Ident "arg") $ TyIdent (Ident "Inner")] (Block1 []) -- fn acceptsNested(arg: Inner) {}
    ]
  , [ SLet (Ident "fun") (int 4) -- let fun = 4;
    , SExp (ECall (EIdent (Ident "fun")) []) -- fun();
    ]
  , [ SLet (Ident "a") (EAnonFun (AnonArgs [TypedIdent (Ident "arg") $ TyIdent (Ident "MyStruct")] (Block2 [] $ int 30)))] -- let a = |arg: MyStruct| {30};
  ]

testProg :: Program -> IO ()
testProg x = case typeck x of
    Left msg -> putStrLn $ "Failure: " ++ msg
    Right tree -> print tree

main :: IO ()
main = putStrLn "" >> mapM_ testProg testCases >> putStrLn "Bad cases:" >> (mapM_ testProg badCases)
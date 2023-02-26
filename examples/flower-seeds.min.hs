import System.IO(stdin,hSetBuffering,BufferMode(NoBuffering));
import Control.Concurrent(threadDelay);
import System.Environment(getArgs);
a(j,k,l)=take l$iterate(\(m,n)->(m+pi*j/90,n+k/30))(0,0);
b(j,k)=(i(60+k*cos j),i(0.5*(50+k*sin j)));
c(j,k)=do{threadDelay 1000;h("\^[["++show k++";"++show j++"f❤")};
d j=do{h("\^[cflower-seeds "<>show j);traverse(c.b)(a j)};
e(j,k,l) m=let n|m=='j'=(-1)|m=='l'=1|g=0;o|m=='k'=(-1)|m=='i'=1|g=0;p|m=='c'=(-1)|m=='v'=1|g=0 in f(j+n/20,max 0.1(k+o/10),max 1(l+p*17));
f j=do{d j;input<-getChar;e j input};
main=do{hSetBuffering stdin NoBuffering;args<-getArgs;case args of{ [] ->f(29.6,3,400);(j:_)->f(read j)}};
g=True;
h=putStrLn;
i=round
{- AST:
Module
    [ Import "System.IO"
        [ "stdin"
        , "hSetBuffering"
        , "BufferMode(NoBuffering)"
        ]
    , Import "Control.Concurrent" [ "threadDelay" ]
    , Import "System.Environment" [ "getArgs" ]
    ]
    [ TypeDecl "type Flower = (Float, Float, Int)"
    , SigDecl "initialParams :: Flower"
    , ValueDecl
        ( Binding "initialParams"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ETuple
                        [ ELit "29.6"
                        , ELit "3"
                        , ELit "400"
                        ]
                    )
                ]
            ]
        )
    , SigDecl "genSeeds :: Flower -> [(Float, Float)]"
    , ValueDecl
        ( Binding "genSeeds"
            [ BindingMatch
                [ PTup
                    [ PVar "a"
                    , PVar "d"
                    , PVar "n"
                    ]
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EApp
                            ( EVar "take" )
                            ( EVar "n" )
                        )
                        ( EVar "$" )
                        ( EApp
                            ( EApp
                                ( EVar "iterate" )
                                ( EPar
                                    ( ELam
                                        ( BindingMatch
                                            [ PTup
                                                [ PVar "b"
                                                , PVar "e"
                                                ]
                                            ]
                                            [ GuardedExpr []
                                                ( ETuple
                                                    [ EOp
                                                        ( EOp
                                                            ( EOp
                                                                ( EVar "b" )
                                                                ( EVar "+" )
                                                                ( EVar "pi" )
                                                            )
                                                            ( EVar "*" )
                                                            ( EVar "a" )
                                                        )
                                                        ( EVar "/" )
                                                        ( ELit "90" )
                                                    , EOp
                                                        ( EOp
                                                            ( EVar "e" )
                                                            ( EVar "+" )
                                                            ( EVar "d" )
                                                        )
                                                        ( EVar "/" )
                                                        ( ELit "30" )
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
                            ( ETuple
                                [ ELit "0"
                                , ELit "0"
                                ]
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "seedCoord :: (Float, Float) -> (Int, Int)"
    , ValueDecl
        ( Binding "seedCoord"
            [ BindingMatch
                [ PTup
                    [ PVar "a"
                    , PVar "d"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EApp
                            ( EVar "round" )
                            ( EPar
                                ( EOp
                                    ( EOp
                                        ( ELit "60" )
                                        ( EVar "+" )
                                        ( EVar "d" )
                                    )
                                    ( EVar "*" )
                                    ( EApp
                                        ( EVar "cos" )
                                        ( EVar "a" )
                                    )
                                )
                            )
                        , EApp
                            ( EVar "round" )
                            ( EPar
                                ( EOp
                                    ( ELit "0.5" )
                                    ( EVar "*" )
                                    ( EPar
                                        ( EOp
                                            ( EOp
                                                ( ELit "50" )
                                                ( EVar "+" )
                                                ( EVar "d" )
                                            )
                                            ( EVar "*" )
                                            ( EApp
                                                ( EVar "sin" )
                                                ( EVar "a" )
                                            )
                                        )
                                    )
                                )
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "plantSeed :: (Int, Int) -> IO ()"
    , ValueDecl
        ( Binding "plantSeed"
            [ BindingMatch
                [ PTup
                    [ PVar "x"
                    , PVar "y"
                    ]
                ]
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EVar "threadDelay" )
                                ( ELit "1000" )
                            )
                        , SBody
                            ( EApp
                                ( EVar "putStrLn" )
                                ( EPar
                                    ( EOp
                                        ( EOp
                                            ( EOp
                                                ( EOp
                                                    ( ELit ""\^[["" )
                                                    ( EVar "++" )
                                                    ( EApp
                                                        ( EVar "show" )
                                                        ( EVar "y" )
                                                    )
                                                )
                                                ( EVar "++" )
                                                ( ELit "";"" )
                                            )
                                            ( EVar "++" )
                                            ( EApp
                                                ( EVar "show" )
                                                ( EVar "x" )
                                            )
                                        )
                                        ( EVar "++" )
                                        ( ELit ""f❤"" )
                                    )
                                )
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "renderFlower :: Flower -> IO [()]"
    , ValueDecl
        ( Binding "renderFlower"
            [ BindingMatch
                [ PVar "flower" ]
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EVar "putStrLn" )
                                ( EPar
                                    ( EOp
                                        ( ELit ""\^[cflower-seeds "" )
                                        ( EVar "<>" )
                                        ( EApp
                                            ( EVar "show" )
                                            ( EVar "flower" )
                                        )
                                    )
                                )
                            )
                        , SBody
                            ( EApp
                                ( EApp
                                    ( EVar "traverse" )
                                    ( EPar
                                        ( EOp
                                            ( EVar "plantSeed" )
                                            ( EVar "." )
                                            ( EVar "seedCoord" )
                                        )
                                    )
                                )
                                ( EPar
                                    ( EApp
                                        ( EVar "genSeeds" )
                                        ( EVar "flower" )
                                    )
                                )
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "evalInput :: Flower -> Char -> IO ()"
    , ValueDecl
        ( Binding "evalInput"
            [ BindingMatch
                [ PTup
                    [ PVar "a"
                    , PVar "d"
                    , PVar "n"
                    ]
                , PVar "i"
                ]
                [ GuardedExpr []
                    ( ELet
                        [ Binding "b"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'j'" )
                                    ]
                                    ( EPar
                                        ( ELit "-1" )
                                    )
                                , GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'l'" )
                                    ]
                                    ( ELit "1" )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "0" )
                                ]
                            ]
                        , Binding "e"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'k'" )
                                    ]
                                    ( EPar
                                        ( ELit "-1" )
                                    )
                                , GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'i'" )
                                    ]
                                    ( ELit "1" )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "0" )
                                ]
                            ]
                        , Binding "m"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'c'" )
                                    ]
                                    ( EPar
                                        ( ELit "-1" )
                                    )
                                , GuardedExpr
                                    [ EOp
                                        ( EVar "i" )
                                        ( EVar "==" )
                                        ( ELit "'v'" )
                                    ]
                                    ( ELit "1" )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "0" )
                                ]
                            ]
                        ]
                        ( EApp
                            ( EVar "go" )
                            ( ETuple
                                [ EOp
                                    ( EOp
                                        ( EVar "a" )
                                        ( EVar "+" )
                                        ( EVar "b" )
                                    )
                                    ( EVar "/" )
                                    ( ELit "20" )
                                , EApp
                                    ( EApp
                                        ( EVar "max" )
                                        ( ELit "0.1" )
                                    )
                                    ( EPar
                                        ( EOp
                                            ( EOp
                                                ( EVar "d" )
                                                ( EVar "+" )
                                                ( EVar "e" )
                                            )
                                            ( EVar "/" )
                                            ( ELit "10" )
                                        )
                                    )
                                , EApp
                                    ( EApp
                                        ( EVar "max" )
                                        ( ELit "1" )
                                    )
                                    ( EPar
                                        ( EOp
                                            ( EOp
                                                ( EVar "n" )
                                                ( EVar "+" )
                                                ( EVar "m" )
                                            )
                                            ( EVar "*" )
                                            ( ELit "17" )
                                        )
                                    )
                                ]
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "go :: Flower -> IO ()"
    , ValueDecl
        ( Binding "go"
            [ BindingMatch
                [ PVar "flower" ]
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EVar "renderFlower" )
                                ( EVar "flower" )
                            )
                        , SBind
                            ( PVar "input" )
                            ( EVar "getChar" )
                        , SBody
                            ( EApp
                                ( EApp
                                    ( EVar "evalInput" )
                                    ( EVar "flower" )
                                )
                                ( EVar "input" )
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "main :: IO ()"
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EApp
                                    ( EVar "hSetBuffering" )
                                    ( EVar "stdin" )
                                )
                                ( EVar "NoBuffering" )
                            )
                        , SBind
                            ( PVar "args" )
                            ( EVar "getArgs" )
                        , SBody
                            ( ECase
                                ( EVar "args" )
                                [ BindingMatch
                                    [ PCon "[]" [] ]
                                    [ GuardedExpr []
                                        ( EApp
                                            ( EVar "go" )
                                            ( EVar "initialParams" )
                                        )
                                    ]
                                , BindingMatch
                                    [ PPar
                                        ( PIco ":"
                                            ( PVar "x" )
                                            ( PLit "_" )
                                        )
                                    ]
                                    [ GuardedExpr []
                                        ( EApp
                                            ( EVar "go" )
                                            ( EPar
                                                ( EApp
                                                    ( EVar "read" )
                                                    ( EVar "x" )
                                                )
                                            )
                                        )
                                    ]
                                ]
                            )
                        ]
                    )
                ]
            ]
        )
    ]
-}

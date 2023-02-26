import GHC.Conc(threadDelay);
import System.IO;
a 0=d"game-over";
a e|e>=1,e<=2=pure()|c=d("score: "++show e);
b|c=[123];
main=a(21*let e=1;f=1 in e+f);
c=True;
d=putStrLn
{- AST:
Module
    [ Import "GHC.Conc" [ "threadDelay" ]
    , Import "System.IO" []
    ]
    [ ValueDecl
        ( Binding "render"
            [ BindingMatch
                [ PLit "0" ]
                [ GuardedExpr []
                    ( EApp
                        ( EVar "putStrLn" )
                        ( ELit ""game-over"" )
                    )
                ]
            , BindingMatch
                [ PVar "score" ]
                [ GuardedExpr
                    [ EOp
                        ( EVar "score" )
                        ( EVar ">=" )
                        ( ELit "1" )
                    , EOp
                        ( EVar "score" )
                        ( EVar "<=" )
                        ( ELit "2" )
                    ]
                    ( EApp
                        ( EVar "pure" )
                        ( EVar "()" )
                    )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( EApp
                        ( EVar "putStrLn" )
                        ( EPar
                            ( EOp
                                ( ELit ""score: "" )
                                ( EVar "++" )
                                ( EApp
                                    ( EVar "show" )
                                    ( EVar "score" )
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "list"
            [ BindingMatch []
                [ GuardedExpr
                    [ EVar "otherwise" ]
                    ( EList
                        [ ELit "1"
                        , ELit "2"
                        , ELit "3"
                        ]
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "two"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ELet
                        [ Binding "x"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( ELit "1" )
                                ]
                            ]
                        , Binding "y"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( ELit "1" )
                                ]
                            ]
                        ]
                        ( EOp
                            ( EVar "x" )
                            ( EVar "+" )
                            ( EVar "y" )
                        )
                    )
                ]
            ]
        )
    , SigDecl "main :: IO ()"
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EApp
                        ( EVar "render" )
                        ( EPar
                            ( EOp
                                ( ELit "21" )
                                ( EVar "*" )
                                ( EVar "two" )
                            )
                        )
                    )
                ]
            ]
        )
    ]
-}

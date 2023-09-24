a=[0,88..];
b d=[d,d];
c d=let (e,_)=(0,True);f h i=abs(h-i)<1;(g,_)=(f 0e,f 1e) in (d,g);
main|c 0==(0,True)=pure()|otherwise=putStrLn"Ooops"
{- AST:
Module []
    [ ValueDecl
        ( Binding "pat"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ERange
                        ( ELit "0" ) Nothing
                        ( Just
                            ( ELit "88" )
                        )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "tup"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ETuple
                        [ ELit "0"
                        , EVar "True"
                        ]
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "draw"
            [ BindingMatch
                [ PVar "p" ]
                [ GuardedExpr []
                    ( EList
                        [ EVar "p"
                        , EVar "p"
                        ]
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "step"
            [ BindingMatch
                [ PVar "pos" ]
                [ GuardedExpr []
                    ( ELet
                        [ BindingPattern
                            ( PTup
                                [ PVar "x"
                                , PLit "_"
                                ]
                            )
                            [ GuardedExpr []
                                ( EVar "tup" )
                            ]
                        , Binding "hit"
                            [ BindingMatch
                                [ PVar "m"
                                , PVar "n"
                                ]
                                [ GuardedExpr []
                                    ( EOp
                                        ( EApp
                                            ( EVar "abs" )
                                            ( EPar
                                                ( EOp
                                                    ( EVar "m" )
                                                    ( EVar "-" )
                                                    ( EVar "n" )
                                                )
                                            )
                                        )
                                        ( EVar "<" )
                                        ( ELit "1" )
                                    )
                                ]
                            ]
                        , BindingPattern
                            ( PTup
                                [ PVar "res"
                                , PLit "_"
                                ]
                            )
                            [ GuardedExpr []
                                ( ETuple
                                    [ EApp
                                        ( EApp
                                            ( EVar "hit" )
                                            ( ELit "0" )
                                        )
                                        ( EVar "x" )
                                    , EApp
                                        ( EApp
                                            ( EVar "hit" )
                                            ( ELit "1" )
                                        )
                                        ( EVar "x" )
                                    ]
                                )
                            ]
                        ]
                        ( ETuple
                            [ EVar "pos"
                            , EVar "res"
                            ]
                        )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr
                    [ EOp
                        ( EApp
                            ( EVar "step" )
                            ( ELit "0" )
                        )
                        ( EVar "==" )
                        ( ETuple
                            [ ELit "0"
                            , EVar "True"
                            ]
                        )
                    ]
                    ( EApp
                        ( EVar "pure" )
                        ( EVar "()" )
                    )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( EApp
                        ( EVar "putStrLn" )
                        ( ELit ""Ooops"" )
                    )
                ]
            ]
        )
    ]
-}

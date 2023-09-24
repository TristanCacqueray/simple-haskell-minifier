a=[0,88..];
b c=let (d,_)=(0,True);e g h=abs(g-h)<1;(f,_)=(e 0d,e 1d) in (c,f);
main|b 0==(0,True)=pure()|otherwise=putStrLn"Ooops"
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

a c d=(b c+b d);
main=print(a[][]);
b=length
{- AST:
Module []
    [ ValueDecl
        ( Binding "compute"
            [ BindingMatch
                [ PVar "xs"
                , PVar "ys"
                ]
                [ GuardedExpr []
                    ( EPar
                        ( EOp
                            ( EApp
                                ( EVar "length" )
                                ( EVar "xs" )
                            )
                            ( EVar "+" )
                            ( EApp
                                ( EVar "length" )
                                ( EVar "ys" )
                            )
                        )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EApp
                        ( EVar "print" )
                        ( EPar
                            ( EApp
                                ( EApp
                                    ( EVar "compute" )
                                    ( EVar "[]" )
                                )
                                ( EVar "[]" )
                            )
                        )
                    )
                ]
            ]
        )
    ]
-}

a=[0,88..];
main=pure()
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
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EApp
                        ( EVar "pure" )
                        ( EVar "()" )
                    )
                ]
            ]
        )
    ]
-}

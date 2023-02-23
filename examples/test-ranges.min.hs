a=print;
main=a(take 5[0..])>>a[0..10]>>a[0,5..10]
{- AST:
Module []
    [ ValueDecl
        ( Binding "from"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ERange
                        ( ELit "0" ) Nothing Nothing
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "fromThen"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ERange
                        ( ELit "0" )
                        ( Just
                            ( ELit "10" )
                        ) Nothing
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "fromThenTo"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ERange
                        ( ELit "0" )
                        ( Just
                            ( ELit "10" )
                        )
                        ( Just
                            ( ELit "5" )
                        )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EApp
                                ( EVar "print" )
                                ( EPar
                                    ( EApp
                                        ( EApp
                                            ( EVar "take" )
                                            ( ELit "5" )
                                        )
                                        ( EVar "from" )
                                    )
                                )
                            )
                            ( EVar ">>" )
                            ( EApp
                                ( EVar "print" )
                                ( EVar "fromThen" )
                            )
                        )
                        ( EVar ">>" )
                        ( EApp
                            ( EVar "print" )
                            ( EVar "fromThenTo" )
                        )
                    )
                ]
            ]
        )
    ]
-}

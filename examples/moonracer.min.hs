a=[0,88..];
b e=[e,e];
c(EventKey(SpecialKey e)f _ _)=(e,f);
d e=let (f,_)=(0,True);g i j=abs(i-j)<1;(h,_)=(g 0f,g 1f)in (e,h)
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
        ( Binding "handleEvent"
            [ BindingMatch
                [ PPar
                    ( PCon "EventKey"
                        [ PPar
                            ( PCon "SpecialKey"
                                [ PVar "key" ]
                            )
                        , PVar "dir"
                        , PLit "_"
                        , PLit "_"
                        ]
                    )
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EVar "key"
                        , EVar "dir"
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
    ]
-}

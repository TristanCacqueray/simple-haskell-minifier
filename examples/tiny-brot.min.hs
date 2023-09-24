a=80;
b=24;
c(j,k)(l,m)=(j+(l*l-m*m),k+2*l*m);
d j=4.18-4.179*(1-cos(j/10)**8);
e j(k,l)(m,n)=(k+j*(m-a/2)/a,(l+j*(n-b/2)/b)*(-0.5));
f(j,k)=let l=abs(j*k)in if isNaN l then 42else l ;
g j k=f.last.take j.iterate(c k)$(0,0);
h _(81,_)="\n";
h j k=if g 150(e j(-1.4844,0)k)>20then" "else"λ";
i=[0..80];
main=pure()
{- AST:
Module []
    [ SigDecl "width, height :: Float"
    , ValueDecl
        ( Binding "width"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ELit "80" )
                ]
            ]
        )
    , ValueDecl
        ( Binding "height"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ELit "24" )
                ]
            ]
        )
    , SigDecl "screenRatio :: Float"
    , ValueDecl
        ( Binding "screenRatio"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EPar
                        ( ELit "-0.5" )
                    )
                ]
            ]
        )
    , TypeDecl "type Complex = (Float, Float)"
    , SigDecl "z2 :: Complex -> Complex -> Complex"
    , ValueDecl
        ( Binding "z2"
            [ BindingMatch
                [ PTup
                    [ PVar "cx"
                    , PVar "cy"
                    ]
                , PTup
                    [ PVar "x"
                    , PVar "y"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EOp
                            ( EVar "cx" )
                            ( EVar "+" )
                            ( EPar
                                ( EOp
                                    ( EOp
                                        ( EOp
                                            ( EVar "x" )
                                            ( EVar "*" )
                                            ( EVar "x" )
                                        )
                                        ( EVar "-" )
                                        ( EVar "y" )
                                    )
                                    ( EVar "*" )
                                    ( EVar "y" )
                                )
                            )
                        , EOp
                            ( EOp
                                ( EOp
                                    ( EVar "cy" )
                                    ( EVar "+" )
                                    ( ELit "2" )
                                )
                                ( EVar "*" )
                                ( EVar "x" )
                            )
                            ( EVar "*" )
                            ( EVar "y" )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "mb :: Complex"
    , ValueDecl
        ( Binding "mb"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ETuple
                        [ ELit "-1.4844"
                        , ELit "0"
                        ]
                    )
                ]
            ]
        )
    , TypeDecl "type Time = Float"
    , TypeDecl "type Zoom = Float"
    , SigDecl "zoom :: Time -> Zoom"
    , ValueDecl
        ( Binding "zoom"
            [ BindingMatch
                [ PVar "x" ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( ELit "4.18" )
                            ( EVar "-" )
                            ( ELit "4.179" )
                        )
                        ( EVar "*" )
                        ( EPar
                            ( EOp
                                ( EOp
                                    ( ELit "1" )
                                    ( EVar "-" )
                                    ( EApp
                                        ( EVar "cos" )
                                        ( EPar
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "/" )
                                                ( ELit "10" )
                                            )
                                        )
                                    )
                                )
                                ( EVar "**" )
                                ( ELit "8" )
                            )
                        )
                    )
                ]
            ]
        )
    , TypeDecl "type Coord = (Float, Float)"
    , SigDecl "coord :: Zoom -> Complex -> Coord -> Complex"
    , ValueDecl
        ( Binding "coord"
            [ BindingMatch
                [ PVar "z"
                , PTup
                    [ PVar "c"
                    , PVar "d"
                    ]
                , PTup
                    [ PVar "x"
                    , PVar "y"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EOp
                            ( EOp
                                ( EOp
                                    ( EVar "c" )
                                    ( EVar "+" )
                                    ( EVar "z" )
                                )
                                ( EVar "*" )
                                ( EPar
                                    ( EOp
                                        ( EOp
                                            ( EVar "x" )
                                            ( EVar "-" )
                                            ( EVar "width" )
                                        )
                                        ( EVar "/" )
                                        ( ELit "2" )
                                    )
                                )
                            )
                            ( EVar "/" )
                            ( EVar "width" )
                        , EOp
                            ( EPar
                                ( EOp
                                    ( EOp
                                        ( EOp
                                            ( EVar "d" )
                                            ( EVar "+" )
                                            ( EVar "z" )
                                        )
                                        ( EVar "*" )
                                        ( EPar
                                            ( EOp
                                                ( EOp
                                                    ( EVar "y" )
                                                    ( EVar "-" )
                                                    ( EVar "height" )
                                                )
                                                ( EVar "/" )
                                                ( ELit "2" )
                                            )
                                        )
                                    )
                                    ( EVar "/" )
                                    ( EVar "height" )
                                )
                            )
                            ( EVar "*" )
                            ( EVar "screenRatio" )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "dot :: Complex -> Float"
    , ValueDecl
        ( Binding "dot"
            [ BindingMatch
                [ PTup
                    [ PVar "x"
                    , PVar "y"
                    ]
                ]
                [ GuardedExpr []
                    ( ELet
                        [ Binding "l"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EApp
                                        ( EVar "abs" )
                                        ( EPar
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "*" )
                                                ( EVar "y" )
                                            )
                                        )
                                    )
                                ]
                            ]
                        ]
                        ( EIf
                            ( EApp
                                ( EVar "isNaN" )
                                ( EVar "l" )
                            )
                            ( ELit "42" )
                            ( EVar "l" )
                        )
                    )
                ]
            ]
        )
    , SigDecl "brot :: Int -> Complex -> Float"
    , ValueDecl
        ( Binding "brot"
            [ BindingMatch
                [ PVar "max_iter"
                , PVar "p"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EOp
                                ( EOp
                                    ( EVar "dot" )
                                    ( EVar "." )
                                    ( EVar "last" )
                                )
                                ( EVar "." )
                                ( EApp
                                    ( EVar "take" )
                                    ( EVar "max_iter" )
                                )
                            )
                            ( EVar "." )
                            ( EApp
                                ( EVar "iterate" )
                                ( EPar
                                    ( EApp
                                        ( EVar "z2" )
                                        ( EVar "p" )
                                    )
                                )
                            )
                        )
                        ( EVar "$" )
                        ( ETuple
                            [ ELit "0"
                            , ELit "0"
                            ]
                        )
                    )
                ]
            ]
        )
    , SigDecl "d :: Zoom -> Coord -> String"
    , ValueDecl
        ( Binding "d"
            [ BindingMatch
                [ PLit "_"
                , PTup
                    [ PLit "81"
                    , PLit "_"
                    ]
                ]
                [ GuardedExpr []
                    ( ELit ""\n"" )
                ]
            , BindingMatch
                [ PVar "z"
                , PVar "c"
                ]
                [ GuardedExpr []
                    ( EIf
                        ( EOp
                            ( EApp
                                ( EApp
                                    ( EVar "brot" )
                                    ( ELit "150" )
                                )
                                ( EPar
                                    ( EApp
                                        ( EApp
                                            ( EApp
                                                ( EVar "coord" )
                                                ( EVar "z" )
                                            )
                                            ( EVar "mb" )
                                        )
                                        ( EVar "c" )
                                    )
                                )
                            )
                            ( EVar ">" )
                            ( ELit "20" )
                        )
                        ( ELit "" "" )
                        ( ELit ""λ"" )
                    )
                ]
            ]
        )
    , ValueDecl
        ( Binding "x"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ERange
                        ( ELit "0" )
                        ( Just
                            ( ELit "80" )
                        ) Nothing
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
                        ( EVar "pure" )
                        ( EVar "()" )
                    )
                ]
            ]
        )
    ]
-}

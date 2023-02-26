import Terminal.Game;
a=80;
b=20;
c(p,q,r)=sqrt(p*p+q*q+r*r);
d(p,q,r)=(max p 0,max q 0,max r 0);
e p q(r,s,t)=c.d$(k r-q,k s-p,k t-0.03);
f p(q,r,s)=(q*m p-r*o p,q*o p+r*m p,s);
g(p,q,r)=(p+0.05,q-0.4,r);
h p=min(e 0.8 0.05(f 1p))(e 0.4 0.04(g(f(-1)p)));
i _ 10 _=id;
i p q(r,s,(t,u,v))=let w=t*m p-v*o p;x=t*o p+v*m p;y=h(w,u,x) in if y<=0.01then(s,r)%cell '%' else i p(q+1)(r,s,(t,u,v+y));
j p=foldr(i(p/10)0)(blankPlane(n a)(n b))[(n x,n y,(x/a*2-1,(y/b*2-1)*0.2,-10))|y<-[0..b],x<-[0..a]];
main=playGame(Game 13 0(\_ p _->p+1)(l j)(l False));
k=abs;
l=const;
m=cos;
n=round;
o=sin
{- AST:
Module
    [ Import "Terminal.Game" [] ]
    [ TypeDecl "type V3 = (Float, Float, Float)"
    , TypeDecl "type Time = Float"
    , SigDecl "width, height, screenRatio :: Float"
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
                    ( ELit "20" )
                ]
            ]
        )
    , ValueDecl
        ( Binding "screenRatio"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ELit "0.2" )
                ]
            ]
        )
    , SigDecl "points :: [(Int, Int, V3)]"
    , ValueDecl
        ( Binding "points"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EComp
                        [ SBind
                            ( PVar "y" )
                            ( ERange
                                ( ELit "0" )
                                ( Just
                                    ( EVar "height" )
                                ) Nothing
                            )
                        , SBind
                            ( PVar "x" )
                            ( ERange
                                ( ELit "0" )
                                ( Just
                                    ( EVar "width" )
                                ) Nothing
                            )
                        , SBody
                            ( ETuple
                                [ EApp
                                    ( EVar "round" )
                                    ( EVar "x" )
                                , EApp
                                    ( EVar "round" )
                                    ( EVar "y" )
                                , ETuple
                                    [ EOp
                                        ( EOp
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "/" )
                                                ( EVar "width" )
                                            )
                                            ( EVar "*" )
                                            ( ELit "2" )
                                        )
                                        ( EVar "-" )
                                        ( ELit "1" )
                                    , EOp
                                        ( EPar
                                            ( EOp
                                                ( EOp
                                                    ( EOp
                                                        ( EVar "y" )
                                                        ( EVar "/" )
                                                        ( EVar "height" )
                                                    )
                                                    ( EVar "*" )
                                                    ( ELit "2" )
                                                )
                                                ( EVar "-" )
                                                ( ELit "1" )
                                            )
                                        )
                                        ( EVar "*" )
                                        ( EVar "screenRatio" )
                                    , ELit "-10"
                                    ]
                                ]
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "dot :: V3 -> Float"
    , ValueDecl
        ( Binding "dot"
            [ BindingMatch
                [ PTup
                    [ PVar "x"
                    , PVar "y"
                    , PVar "z"
                    ]
                ]
                [ GuardedExpr []
                    ( EApp
                        ( EVar "sqrt" )
                        ( EPar
                            ( EOp
                                ( EOp
                                    ( EOp
                                        ( EOp
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "*" )
                                                ( EVar "x" )
                                            )
                                            ( EVar "+" )
                                            ( EVar "y" )
                                        )
                                        ( EVar "*" )
                                        ( EVar "y" )
                                    )
                                    ( EVar "+" )
                                    ( EVar "z" )
                                )
                                ( EVar "*" )
                                ( EVar "z" )
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "clamp :: V3 -> V3"
    , ValueDecl
        ( Binding "clamp"
            [ BindingMatch
                [ PTup
                    [ PVar "x"
                    , PVar "y"
                    , PVar "z"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EApp
                            ( EApp
                                ( EVar "max" )
                                ( EVar "x" )
                            )
                            ( ELit "0" )
                        , EApp
                            ( EApp
                                ( EVar "max" )
                                ( EVar "y" )
                            )
                            ( ELit "0" )
                        , EApp
                            ( EApp
                                ( EVar "max" )
                                ( EVar "z" )
                            )
                            ( ELit "0" )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "sdBox :: Float -> Float -> V3 -> Float"
    , ValueDecl
        ( Binding "sdBox"
            [ BindingMatch
                [ PVar "height"
                , PVar "width"
                , PTup
                    [ PVar "x"
                    , PVar "y"
                    , PVar "z"
                    ]
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EVar "dot" )
                            ( EVar "." )
                            ( EVar "clamp" )
                        )
                        ( EVar "$" )
                        ( ETuple
                            [ EOp
                                ( EApp
                                    ( EVar "abs" )
                                    ( EVar "x" )
                                )
                                ( EVar "-" )
                                ( EVar "width" )
                            , EOp
                                ( EApp
                                    ( EVar "abs" )
                                    ( EVar "y" )
                                )
                                ( EVar "-" )
                                ( EVar "height" )
                            , EOp
                                ( EApp
                                    ( EVar "abs" )
                                    ( EVar "z" )
                                )
                                ( EVar "-" )
                                ( ELit "0.03" )
                            ]
                        )
                    )
                ]
            ]
        )
    , SigDecl "rot :: Float -> V3 -> V3"
    , ValueDecl
        ( Binding "rot"
            [ BindingMatch
                [ PVar "t"
                , PTup
                    [ PVar "x"
                    , PVar "y"
                    , PVar "z"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EOp
                            ( EOp
                                ( EOp
                                    ( EVar "x" )
                                    ( EVar "*" )
                                    ( EApp
                                        ( EVar "cos" )
                                        ( EVar "t" )
                                    )
                                )
                                ( EVar "-" )
                                ( EVar "y" )
                            )
                            ( EVar "*" )
                            ( EApp
                                ( EVar "sin" )
                                ( EVar "t" )
                            )
                        , EOp
                            ( EOp
                                ( EOp
                                    ( EVar "x" )
                                    ( EVar "*" )
                                    ( EApp
                                        ( EVar "sin" )
                                        ( EVar "t" )
                                    )
                                )
                                ( EVar "+" )
                                ( EVar "y" )
                            )
                            ( EVar "*" )
                            ( EApp
                                ( EVar "cos" )
                                ( EVar "t" )
                            )
                        , EVar "z"
                        ]
                    )
                ]
            ]
        )
    , SigDecl "offset :: V3 -> V3"
    , ValueDecl
        ( Binding "offset"
            [ BindingMatch
                [ PTup
                    [ PVar "x"
                    , PVar "y"
                    , PVar "z"
                    ]
                ]
                [ GuardedExpr []
                    ( ETuple
                        [ EOp
                            ( EVar "x" )
                            ( EVar "+" )
                            ( ELit "0.05" )
                        , EOp
                            ( EVar "y" )
                            ( EVar "-" )
                            ( ELit "0.4" )
                        , EVar "z"
                        ]
                    )
                ]
            ]
        )
    , SigDecl "scene :: V3 -> Float"
    , ValueDecl
        ( Binding "scene"
            [ BindingMatch
                [ PVar "p" ]
                [ GuardedExpr []
                    ( EApp
                        ( EApp
                            ( EVar "min" )
                            ( EPar
                                ( EApp
                                    ( EApp
                                        ( EApp
                                            ( EVar "sdBox" )
                                            ( ELit "0.8" )
                                        )
                                        ( ELit "0.05" )
                                    )
                                    ( EPar
                                        ( EApp
                                            ( EApp
                                                ( EVar "rot" )
                                                ( ELit "1" )
                                            )
                                            ( EVar "p" )
                                        )
                                    )
                                )
                            )
                        )
                        ( EPar
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EVar "sdBox" )
                                        ( ELit "0.4" )
                                    )
                                    ( ELit "0.04" )
                                )
                                ( EPar
                                    ( EApp
                                        ( EVar "offset" )
                                        ( EPar
                                            ( EApp
                                                ( EApp
                                                    ( EVar "rot" )
                                                    ( EPar
                                                        ( ELit "-1" )
                                                    )
                                                )
                                                ( EVar "p" )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "march :: Time -> Int -> (Int, Int, V3) -> (Plane -> Plane)"
    , ValueDecl
        ( Binding "march"
            [ BindingMatch
                [ PLit "_"
                , PLit "10"
                , PLit "_"
                ]
                [ GuardedExpr []
                    ( EVar "id" )
                ]
            , BindingMatch
                [ PVar "time"
                , PVar "step"
                , PTup
                    [ PVar "ix"
                    , PVar "iy"
                    , PTup
                        [ PVar "x"
                        , PVar "y"
                        , PVar "z"
                        ]
                    ]
                ]
                [ GuardedExpr []
                    ( ELet
                        [ Binding "rx"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EOp
                                        ( EOp
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "*" )
                                                ( EApp
                                                    ( EVar "cos" )
                                                    ( EVar "time" )
                                                )
                                            )
                                            ( EVar "-" )
                                            ( EVar "z" )
                                        )
                                        ( EVar "*" )
                                        ( EApp
                                            ( EVar "sin" )
                                            ( EVar "time" )
                                        )
                                    )
                                ]
                            ]
                        , Binding "rz"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EOp
                                        ( EOp
                                            ( EOp
                                                ( EVar "x" )
                                                ( EVar "*" )
                                                ( EApp
                                                    ( EVar "sin" )
                                                    ( EVar "time" )
                                                )
                                            )
                                            ( EVar "+" )
                                            ( EVar "z" )
                                        )
                                        ( EVar "*" )
                                        ( EApp
                                            ( EVar "cos" )
                                            ( EVar "time" )
                                        )
                                    )
                                ]
                            ]
                        , Binding "distance"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EApp
                                        ( EVar "scene" )
                                        ( ETuple
                                            [ EVar "rx"
                                            , EVar "y"
                                            , EVar "rz"
                                            ]
                                        )
                                    )
                                ]
                            ]
                        ]
                        ( EIf
                            ( EOp
                                ( EVar "distance" )
                                ( EVar "<=" )
                                ( ELit "0.01" )
                            )
                            ( EOp
                                ( ETuple
                                    [ EVar "iy"
                                    , EVar "ix"
                                    ]
                                )
                                ( EVar "%" )
                                ( EApp
                                    ( EVar "cell" )
                                    ( ELit "'%'" )
                                )
                            )
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EVar "march" )
                                        ( EVar "time" )
                                    )
                                    ( EPar
                                        ( EOp
                                            ( EVar "step" )
                                            ( EVar "+" )
                                            ( ELit "1" )
                                        )
                                    )
                                )
                                ( ETuple
                                    [ EVar "ix"
                                    , EVar "iy"
                                    , ETuple
                                        [ EVar "x"
                                        , EVar "y"
                                        , EOp
                                            ( EVar "z" )
                                            ( EVar "+" )
                                            ( EVar "distance" )
                                        ]
                                    ]
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "go :: Float -> Plane"
    , ValueDecl
        ( Binding "go"
            [ BindingMatch
                [ PVar "time" ]
                [ GuardedExpr []
                    ( EApp
                        ( EApp
                            ( EApp
                                ( EVar "foldr" )
                                ( EPar
                                    ( EApp
                                        ( EApp
                                            ( EVar "march" )
                                            ( EPar
                                                ( EOp
                                                    ( EVar "time" )
                                                    ( EVar "/" )
                                                    ( ELit "10" )
                                                )
                                            )
                                        )
                                        ( ELit "0" )
                                    )
                                )
                            )
                            ( EPar
                                ( EApp
                                    ( EApp
                                        ( EVar "blankPlane" )
                                        ( EPar
                                            ( EApp
                                                ( EVar "round" )
                                                ( EVar "width" )
                                            )
                                        )
                                    )
                                    ( EPar
                                        ( EApp
                                            ( EVar "round" )
                                            ( EVar "height" )
                                        )
                                    )
                                )
                            )
                        )
                        ( EVar "points" )
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
                        ( EVar "playGame" )
                        ( EPar
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EApp
                                            ( EApp
                                                ( EVar "Game" )
                                                ( ELit "13" )
                                            )
                                            ( ELit "0" )
                                        )
                                        ( EPar
                                            ( ELam
                                                ( BindingMatch
                                                    [ PLit "_"
                                                    , PVar "time"
                                                    , PLit "_"
                                                    ]
                                                    [ GuardedExpr []
                                                        ( EOp
                                                            ( EVar "time" )
                                                            ( EVar "+" )
                                                            ( ELit "1" )
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                    ( EPar
                                        ( EApp
                                            ( EVar "const" )
                                            ( EVar "go" )
                                        )
                                    )
                                )
                                ( EPar
                                    ( EApp
                                        ( EVar "const" )
                                        ( EVar "False" )
                                    )
                                )
                            )
                        )
                    )
                ]
            ]
        )
    ]
-}

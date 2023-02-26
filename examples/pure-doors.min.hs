a=1:2:zipWith(+)a(g a);
b h i j="|"<>f j h<>i<>f(7-j)h<>"|\n";
c 5(0,h) i j k|h/=i="Crash!"<>k[]|otherwise=c 5(e j)i(g j)k;
c 5(h,i) j k l=b ' '"^"j<>"[jkl]> "<>l((h-1,i):k);
c h i@(j,k) l m n=(if h==5-j then b '-'" "k else b ' '" "7)<>c(h+1)i l m n;
d h i j k="\ESCcpure-doors\n"<>c 0(e i)j(g i)(\l->case (l,k) of{(_:_,m:'\n':n)|m>'h'&&m<'n'->d(h+1)l(j+fromEnum m-107)n; _->(if h>5then" GG, your score is: "<>show(div h 5)else"")<>"\n"});
main=interact(d(-1)(map(\h->(5,mod h 8))a)3);
e=head;
f=replicate;
g=tail
{- AST:
Module []
    [ TypeDecl "type Position = Int"
    , SigDecl "nums :: [Position]"
    , ValueDecl
        ( Binding "nums"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( ELit "1" )
                            ( EVar ":" )
                            ( ELit "2" )
                        )
                        ( EVar ":" )
                        ( EApp
                            ( EApp
                                ( EApp
                                    ( EVar "zipWith" )
                                    ( EVar "+" )
                                )
                                ( EVar "nums" )
                            )
                            ( EPar
                                ( EApp
                                    ( EVar "tail" )
                                    ( EVar "nums" )
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , TypeDecl "type DoorDistance = Int"
    , TypeDecl "type Door = (DoorDistance, Position)"
    , TypeDecl "type World = [Door]"
    , SigDecl "world :: World"
    , ValueDecl
        ( Binding "world"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EApp
                        ( EApp
                            ( EVar "map" )
                            ( EPar
                                ( ELam
                                    ( BindingMatch
                                        [ PVar "n" ]
                                        [ GuardedExpr []
                                            ( ETuple
                                                [ ELit "5"
                                                , EApp
                                                    ( EApp
                                                        ( EVar "mod" )
                                                        ( EVar "n" )
                                                    )
                                                    ( ELit "8" )
                                                ]
                                            )
                                        ]
                                    )
                                )
                            )
                        )
                        ( EVar "nums" )
                    )
                ]
            ]
        )
    , SigDecl "draw :: Char -> String -> Position -> String"
    , ValueDecl
        ( Binding "draw"
            [ BindingMatch
                [ PVar "sep"
                , PVar "mid"
                , PVar "pos"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EOp
                                ( EOp
                                    ( ELit ""|"" )
                                    ( EVar "<>" )
                                    ( EApp
                                        ( EApp
                                            ( EVar "replicate" )
                                            ( EVar "pos" )
                                        )
                                        ( EVar "sep" )
                                    )
                                )
                                ( EVar "<>" )
                                ( EVar "mid" )
                            )
                            ( EVar "<>" )
                            ( EApp
                                ( EApp
                                    ( EVar "replicate" )
                                    ( EPar
                                        ( EOp
                                            ( ELit "7" )
                                            ( EVar "-" )
                                            ( EVar "pos" )
                                        )
                                    )
                                )
                                ( EVar "sep" )
                            )
                        )
                        ( EVar "<>" )
                        ( ELit ""|\n"" )
                    )
                ]
            ]
        )
    , SigDecl "emptyLine :: String"
    , ValueDecl
        ( Binding "emptyLine"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EApp
                        ( EApp
                            ( EApp
                                ( EVar "draw" )
                                ( ELit "' '" )
                            )
                            ( ELit "" "" )
                        )
                        ( ELit "7" )
                    )
                ]
            ]
        )
    , SigDecl "step ::
        Int -> Door -> Position -> World -> (World -> String) -> String"
    , ValueDecl
        ( Binding "step"
            [ BindingMatch
                [ PLit "5"
                , PTup
                    [ PLit "0"
                    , PVar "door"
                    ]
                , PVar "pos"
                , PVar "world"
                , PVar "cont"
                ]
                [ GuardedExpr
                    [ EOp
                        ( EVar "door" )
                        ( EVar "/=" )
                        ( EVar "pos" )
                    ]
                    ( EOp
                        ( ELit ""Crash!"" )
                        ( EVar "<>" )
                        ( EApp
                            ( EVar "cont" )
                            ( EVar "[]" )
                        )
                    )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( EApp
                        ( EApp
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EVar "step" )
                                        ( ELit "5" )
                                    )
                                    ( EPar
                                        ( EApp
                                            ( EVar "head" )
                                            ( EVar "world" )
                                        )
                                    )
                                )
                                ( EVar "pos" )
                            )
                            ( EPar
                                ( EApp
                                    ( EVar "tail" )
                                    ( EVar "world" )
                                )
                            )
                        )
                        ( EVar "cont" )
                    )
                ]
            , BindingMatch
                [ PLit "5"
                , PTup
                    [ PVar "distance"
                    , PVar "door"
                    ]
                , PVar "pos"
                , PVar "world"
                , PVar "cont"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EVar "draw" )
                                        ( ELit "' '" )
                                    )
                                    ( ELit ""^"" )
                                )
                                ( EVar "pos" )
                            )
                            ( EVar "<>" )
                            ( ELit ""[jkl]> "" )
                        )
                        ( EVar "<>" )
                        ( EApp
                            ( EVar "cont" )
                            ( EPar
                                ( EOp
                                    ( ETuple
                                        [ EOp
                                            ( EVar "distance" )
                                            ( EVar "-" )
                                            ( ELit "1" )
                                        , EVar "door"
                                        ]
                                    )
                                    ( EVar ":" )
                                    ( EVar "world" )
                                )
                            )
                        )
                    )
                ]
            , BindingMatch
                [ PVar "n"
                , PNam "doorDistance"
                    ( PTup
                        [ PVar "distance"
                        , PVar "door"
                        ]
                    )
                , PVar "pos"
                , PVar "rest"
                , PVar "cont"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EPar
                            ( EIf
                                ( EPar
                                    ( EOp
                                        ( EOp
                                            ( EVar "n" )
                                            ( EVar "==" )
                                            ( ELit "5" )
                                        )
                                        ( EVar "-" )
                                        ( EVar "distance" )
                                    )
                                )
                                ( EApp
                                    ( EApp
                                        ( EApp
                                            ( EVar "draw" )
                                            ( ELit "'-'" )
                                        )
                                        ( ELit "" "" )
                                    )
                                    ( EVar "door" )
                                )
                                ( EVar "emptyLine" )
                            )
                        )
                        ( EVar "<>" )
                        ( EApp
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EApp
                                            ( EVar "step" )
                                            ( EPar
                                                ( EOp
                                                    ( EVar "n" )
                                                    ( EVar "+" )
                                                    ( ELit "1" )
                                                )
                                            )
                                        )
                                        ( EVar "doorDistance" )
                                    )
                                    ( EVar "pos" )
                                )
                                ( EVar "rest" )
                            )
                            ( EVar "cont" )
                        )
                    )
                ]
            ]
        )
    , TypeDecl "type Score = Int"
    , SigDecl "go :: Score -> World -> Position -> String -> String"
    , ValueDecl
        ( Binding "go"
            [ BindingMatch
                [ PVar "score"
                , PVar "world"
                , PVar "pos"
                , PVar "input"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( ELit ""\ESCcpure-doors\n"" )
                        ( EVar "<>" )
                        ( EApp
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EApp
                                            ( EVar "step" )
                                            ( ELit "0" )
                                        )
                                        ( EPar
                                            ( EApp
                                                ( EVar "head" )
                                                ( EVar "world" )
                                            )
                                        )
                                    )
                                    ( EVar "pos" )
                                )
                                ( EPar
                                    ( EApp
                                        ( EVar "tail" )
                                        ( EVar "world" )
                                    )
                                )
                            )
                            ( EPar
                                ( ELam
                                    ( BindingMatch
                                        [ PVar "m" ]
                                        [ GuardedExpr []
                                            ( ECase
                                                ( ETuple
                                                    [ EVar "m"
                                                    , EVar "input"
                                                    ]
                                                )
                                                [ BindingMatch
                                                    [ PTup
                                                        [ PIco ":"
                                                            ( PLit "_" )
                                                            ( PLit "_" )
                                                        , PIco ":"
                                                            ( PIco ":"
                                                                ( PVar "c" )
                                                                ( PLit "'\n'" )
                                                            )
                                                            ( PVar "xs" )
                                                        ]
                                                    ]
                                                    [ GuardedExpr
                                                        [ EOp
                                                            ( EOp
                                                                ( EOp
                                                                    ( EVar "c" )
                                                                    ( EVar ">" )
                                                                    ( ELit "'h'" )
                                                                )
                                                                ( EVar "&&" )
                                                                ( EVar "c" )
                                                            )
                                                            ( EVar "<" )
                                                            ( ELit "'n'" )
                                                        ]
                                                        ( EApp
                                                            ( EApp
                                                                ( EApp
                                                                    ( EApp
                                                                        ( EVar "go" )
                                                                        ( EPar
                                                                            ( EOp
                                                                                ( EVar "score" )
                                                                                ( EVar "+" )
                                                                                ( ELit "1" )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( EVar "m" )
                                                                )
                                                                ( EPar
                                                                    ( EOp
                                                                        ( EOp
                                                                            ( EVar "pos" )
                                                                            ( EVar "+" )
                                                                            ( EApp
                                                                                ( EVar "fromEnum" )
                                                                                ( EVar "c" )
                                                                            )
                                                                        )
                                                                        ( EVar "-" )
                                                                        ( ELit "107" )
                                                                    )
                                                                )
                                                            )
                                                            ( EVar "xs" )
                                                        )
                                                    ]
                                                , BindingMatch
                                                    [ PLit "_" ]
                                                    [ GuardedExpr []
                                                        ( EOp
                                                            ( EPar
                                                                ( EIf
                                                                    ( EOp
                                                                        ( EVar "score" )
                                                                        ( EVar ">" )
                                                                        ( ELit "5" )
                                                                    )
                                                                    ( EOp
                                                                        ( ELit "" GG, your score is: "" )
                                                                        ( EVar "<>" )
                                                                        ( EApp
                                                                            ( EVar "show" )
                                                                            ( EPar
                                                                                ( EApp
                                                                                    ( EApp
                                                                                        ( EVar "div" )
                                                                                        ( EVar "score" )
                                                                                    )
                                                                                    ( ELit "5" )
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( ELit """" )
                                                                )
                                                            )
                                                            ( EVar "<>" )
                                                            ( ELit ""\n"" )
                                                        )
                                                    ]
                                                ]
                                            )
                                        ]
                                    )
                                )
                            )
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
                        ( EVar "interact" )
                        ( EPar
                            ( EApp
                                ( EApp
                                    ( EApp
                                        ( EVar "go" )
                                        ( EPar
                                            ( ELit "-1" )
                                        )
                                    )
                                    ( EVar "world" )
                                )
                                ( ELit "3" )
                            )
                        )
                    )
                ]
            ]
        )
    ]
-}

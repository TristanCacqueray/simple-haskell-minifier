import System.IO(stdin,stdout,hSetBuffering,hSetEcho,BufferMode(NoBuffering));
import Control.Concurrent(threadDelay);
import Data.ByteString(elemIndices,hGetNonBlocking,hPut);
import Data.ByteString.Char8(pack);
import Data.ByteString(ByteString);
i=True;
j=show;
k=stdin;
a l|l<0=j(l`div`10)|i="";
b=hPut stdout.pack;
c l|l>=0=">"|i="<";
d 0(_,l,m,_,_)="VEL "++j m++" | ALT "++j l;
d 1 _="--,"++[' '|_<-[0..69]]++"~|~";
d 2(l,m,n,_,o)=[' '|_<-[0..floor(m/40)]]++o:c n++a l;
e l=b$"\ESCc=<< TSP >>=   | "++unlines(map(flip d l)[0..2]);
f(l,m,n,o,p) q=let r=(/=[]).flip elemIndices q;s=r 102;t=r 114;u|s='*'|t='['|i=' ';v|s=5|t=(-5)|i=0;w=max 0(m+n-1);x|l<0=0|i=1 in if w==0&&o>2900then g l n else h(l+1,x*w,x*(n+v-1),max o m,u);
g l m|m>(-50)=print l|i=b"Lost\n";
h l=do {e l;threadDelay 100000;input<-hGetNonBlocking k 42;f l input};
main=do {hSetBuffering k NoBuffering;hSetEcho k False;h(-30,0,0,0,' ')}
{- AST:
Module
    [ Import "System.IO"
        [ "stdin"
        , "stdout"
        , "hSetBuffering"
        , "hSetEcho"
        , "BufferMode(NoBuffering)"
        ]
    , Import "Control.Concurrent" [ "threadDelay" ]
    , Import "Data.ByteString"
        [ "elemIndices"
        , "hGetNonBlocking"
        , "hPut"
        ]
    , Import "Data.ByteString.Char8" [ "pack" ]
    , Import "Data.ByteString" [ "ByteString" ]
    ]
    [ TypeDecl "type Time = Int"
    , TypeDecl "type Velocity = Float"
    , TypeDecl "type World = (Time, Float, Velocity, Float, Char)"
    , SigDecl "world :: World"
    , ValueDecl
        ( Binding "world"
            [ BindingMatch []
                [ GuardedExpr []
                    ( ETuple
                        [ ELit "-30"
                        , ELit "0"
                        , ELit "0"
                        , ELit "0"
                        , ELit "' '"
                        ]
                    )
                ]
            ]
        )
    , SigDecl "countDown :: Time -> String"
    , ValueDecl
        ( Binding "countDown"
            [ BindingMatch
                [ PVar "time" ]
                [ GuardedExpr
                    [ EOp
                        ( EVar "time" )
                        ( EVar "<" )
                        ( ELit "0" )
                    ]
                    ( EApp
                        ( EVar "show" )
                        ( EPar
                            ( EOp
                                ( EVar "time" )
                                ( EVar "div" )
                                ( ELit "10" )
                            )
                        )
                    )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( ELit """" )
                ]
            ]
        )
    , SigDecl "q :: String -> IO ()"
    , ValueDecl
        ( Binding "q"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EOp
                        ( EApp
                            ( EVar "hPut" )
                            ( EVar "stdout" )
                        )
                        ( EVar "." )
                        ( EVar "pack" )
                    )
                ]
            ]
        )
    , SigDecl "ship :: Velocity -> String"
    , ValueDecl
        ( Binding "ship"
            [ BindingMatch
                [ PVar "velocity" ]
                [ GuardedExpr
                    [ EOp
                        ( EVar "velocity" )
                        ( EVar ">=" )
                        ( ELit "0" )
                    ]
                    ( ELit "">"" )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( ELit ""<"" )
                ]
            ]
        )
    , SigDecl "renderWorld :: Int -> World -> String"
    , ValueDecl
        ( Binding "renderWorld"
            [ BindingMatch
                [ PLit "0"
                , PTup
                    [ PLit "_"
                    , PVar "pos"
                    , PVar "vel"
                    , PLit "_"
                    , PLit "_"
                    ]
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EOp
                                ( ELit ""VEL "" )
                                ( EVar "++" )
                                ( EApp
                                    ( EVar "show" )
                                    ( EVar "vel" )
                                )
                            )
                            ( EVar "++" )
                            ( ELit "" | ALT "" )
                        )
                        ( EVar "++" )
                        ( EApp
                            ( EVar "show" )
                            ( EVar "pos" )
                        )
                    )
                ]
            , BindingMatch
                [ PLit "1"
                , PLit "_"
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( ELit ""--,"" )
                            ( EVar "++" )
                            ( EComp
                                [ SBind
                                    ( PLit "_" )
                                    ( ERange
                                        ( ELit "0" )
                                        ( Just
                                            ( ELit "69" )
                                        ) Nothing
                                    )
                                , SBody
                                    ( ELit "' '" )
                                ]
                            )
                        )
                        ( EVar "++" )
                        ( ELit ""~|~"" )
                    )
                ]
            , BindingMatch
                [ PLit "2"
                , PTup
                    [ PVar "t"
                    , PVar "p"
                    , PVar "v"
                    , PLit "_"
                    , PVar "plume"
                    ]
                ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EOp
                                ( EComp
                                    [ SBind
                                        ( PLit "_" )
                                        ( ERange
                                            ( ELit "0" )
                                            ( Just
                                                ( EApp
                                                    ( EVar "floor" )
                                                    ( EPar
                                                        ( EOp
                                                            ( EVar "p" )
                                                            ( EVar "/" )
                                                            ( ELit "40" )
                                                        )
                                                    )
                                                )
                                            ) Nothing
                                        )
                                    , SBody
                                        ( ELit "' '" )
                                    ]
                                )
                                ( EVar "++" )
                                ( EVar "plume" )
                            )
                            ( EVar ":" )
                            ( EApp
                                ( EVar "ship" )
                                ( EVar "v" )
                            )
                        )
                        ( EVar "++" )
                        ( EApp
                            ( EVar "countDown" )
                            ( EVar "t" )
                        )
                    )
                ]
            ]
        )
    , SigDecl "printWorld :: World -> IO ()"
    , ValueDecl
        ( Binding "printWorld"
            [ BindingMatch
                [ PVar "world" ]
                [ GuardedExpr []
                    ( EOp
                        ( EOp
                            ( EVar "q" )
                            ( EVar "$" )
                            ( ELit ""\ESCc=<< TSP >>=   | "" )
                        )
                        ( EVar "++" )
                        ( EApp
                            ( EVar "unlines" )
                            ( EPar
                                ( EApp
                                    ( EApp
                                        ( EVar "map" )
                                        ( EPar
                                            ( EApp
                                                ( EApp
                                                    ( EVar "flip" )
                                                    ( EVar "renderWorld" )
                                                )
                                                ( EVar "world" )
                                            )
                                        )
                                    )
                                    ( ERange
                                        ( ELit "0" )
                                        ( Just
                                            ( ELit "2" )
                                        ) Nothing
                                    )
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "eval :: World -> ByteString -> IO ()"
    , ValueDecl
        ( Binding "eval"
            [ BindingMatch
                [ PTup
                    [ PVar "time"
                    , PVar "position"
                    , PVar "velocity"
                    , PVar "height"
                    , PVar "_plume"
                    ]
                , PVar "input"
                ]
                [ GuardedExpr []
                    ( ELet
                        [ Binding "has"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EOp
                                        ( EPar
                                            ( EApp
                                                ( EVar "/=" )
                                                ( EVar "[]" )
                                            )
                                        )
                                        ( EVar "." )
                                        ( EApp
                                            ( EApp
                                                ( EVar "flip" )
                                                ( EVar "elemIndices" )
                                            )
                                            ( EVar "input" )
                                        )
                                    )
                                ]
                            ]
                        , Binding "f"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EApp
                                        ( EVar "has" )
                                        ( ELit "102" )
                                    )
                                ]
                            ]
                        , Binding "r"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EApp
                                        ( EVar "has" )
                                        ( ELit "114" )
                                    )
                                ]
                            ]
                        , Binding "plume"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EVar "f" ]
                                    ( ELit "'*'" )
                                , GuardedExpr
                                    [ EVar "r" ]
                                    ( ELit "'['" )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "' '" )
                                ]
                            ]
                        , Binding "n"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EVar "f" ]
                                    ( ELit "5" )
                                , GuardedExpr
                                    [ EVar "r" ]
                                    ( EPar
                                        ( ELit "-5" )
                                    )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "0" )
                                ]
                            ]
                        , Binding "newPosition"
                            [ BindingMatch []
                                [ GuardedExpr []
                                    ( EApp
                                        ( EApp
                                            ( EVar "max" )
                                            ( ELit "0" )
                                        )
                                        ( EPar
                                            ( EOp
                                                ( EOp
                                                    ( EVar "position" )
                                                    ( EVar "+" )
                                                    ( EVar "velocity" )
                                                )
                                                ( EVar "-" )
                                                ( ELit "1" )
                                            )
                                        )
                                    )
                                ]
                            ]
                        , Binding "cd"
                            [ BindingMatch []
                                [ GuardedExpr
                                    [ EOp
                                        ( EVar "time" )
                                        ( EVar "<" )
                                        ( ELit "0" )
                                    ]
                                    ( ELit "0" )
                                , GuardedExpr
                                    [ EVar "otherwise" ]
                                    ( ELit "1" )
                                ]
                            ]
                        ]
                        ( EIf
                            ( EPar
                                ( EOp
                                    ( EOp
                                        ( EOp
                                            ( EVar "newPosition" )
                                            ( EVar "==" )
                                            ( ELit "0" )
                                        )
                                        ( EVar "&&" )
                                        ( EVar "height" )
                                    )
                                    ( EVar ">" )
                                    ( ELit "2900" )
                                )
                            )
                            ( EApp
                                ( EApp
                                    ( EVar "gameOver" )
                                    ( EVar "time" )
                                )
                                ( EVar "velocity" )
                            )
                            ( EApp
                                ( EVar "go" )
                                ( ETuple
                                    [ EOp
                                        ( EVar "time" )
                                        ( EVar "+" )
                                        ( ELit "1" )
                                    , EOp
                                        ( EVar "cd" )
                                        ( EVar "*" )
                                        ( EVar "newPosition" )
                                    , EOp
                                        ( EVar "cd" )
                                        ( EVar "*" )
                                        ( EPar
                                            ( EOp
                                                ( EOp
                                                    ( EVar "velocity" )
                                                    ( EVar "+" )
                                                    ( EVar "n" )
                                                )
                                                ( EVar "-" )
                                                ( ELit "1" )
                                            )
                                        )
                                    , EApp
                                        ( EApp
                                            ( EVar "max" )
                                            ( EVar "height" )
                                        )
                                        ( EVar "position" )
                                    , EVar "plume"
                                    ]
                                )
                            )
                        )
                    )
                ]
            ]
        )
    , SigDecl "gameOver :: Time -> Velocity -> IO ()"
    , ValueDecl
        ( Binding "gameOver"
            [ BindingMatch
                [ PVar "t"
                , PVar "v"
                ]
                [ GuardedExpr
                    [ EOp
                        ( EVar "v" )
                        ( EVar ">" )
                        ( EPar
                            ( ELit "-50" )
                        )
                    ]
                    ( EApp
                        ( EVar "print" )
                        ( EVar "t" )
                    )
                , GuardedExpr
                    [ EVar "otherwise" ]
                    ( EApp
                        ( EVar "q" )
                        ( ELit ""Lost\n"" )
                    )
                ]
            ]
        )
    , SigDecl "go :: World -> IO ()"
    , ValueDecl
        ( Binding "go"
            [ BindingMatch
                [ PVar "world" ]
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EVar "printWorld" )
                                ( EVar "world" )
                            )
                        , SBody
                            ( EApp
                                ( EVar "threadDelay" )
                                ( ELit "100000" )
                            )
                        , SBind
                            ( PVar "input" )
                            ( EApp
                                ( EApp
                                    ( EVar "hGetNonBlocking" )
                                    ( EVar "stdin" )
                                )
                                ( ELit "42" )
                            )
                        , SBody
                            ( EApp
                                ( EApp
                                    ( EVar "eval" )
                                    ( EVar "world" )
                                )
                                ( EVar "input" )
                            )
                        ]
                    )
                ]
            ]
        )
    , SigDecl "main :: IO ()"
    , ValueDecl
        ( Binding "main"
            [ BindingMatch []
                [ GuardedExpr []
                    ( EDo
                        [ SBody
                            ( EApp
                                ( EApp
                                    ( EVar "hSetBuffering" )
                                    ( EVar "stdin" )
                                )
                                ( EVar "NoBuffering" )
                            )
                        , SBody
                            ( EApp
                                ( EApp
                                    ( EVar "hSetEcho" )
                                    ( EVar "stdin" )
                                )
                                ( EVar "False" )
                            )
                        , SBody
                            ( EApp
                                ( EVar "go" )
                                ( EVar "world" )
                            )
                        ]
                    )
                ]
            ]
        )
    ]
-}

% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(HsName(..), hsnWild, hsnArrow, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[1 export(AssocL, hdAndTl, ppAssocL)
%%]

%%[1 import(UU.Pretty, List) export(PP_DocL, ppListSep, ppCommaList, ppListSepFill, ppSpaced, ppAppTop, ppCon, ppCmt)
%%]

%%[1 import(GetOpt) export(EHCOpts(..), defaultEHCOpts, cmdLineOpts)
%%]

%%[1 export(MkConAppAlg, mkApp, mkConApp, mkArrow)
%%]

%%[1.mkProdApp.exp export(mkProdApp)
%%]

%%[7.mkProdApp.exp -1.mkProdApp.exp
%%]

%%[1 export(ParNeed(..), ParNeedL, parNeedApp, ppParNeed)
%%]

%%[2 export(UID, mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, uidNext, mkNewUID, mkNewUIDL, uidStart)
%%]

%%[2 export(assocLMapSnd)
%%]

%%[2 import(List) export(unionL)
%%]

%%[3 export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[4 export(listCombineUniq)
%%]

%%[4 export(CoContraVariance(..), cocoOpp)
%%]

%%[6 export(hsnStar)
%%]

%%[7 export(hsnRow,hsnRec,hsnSum,hsnRowEmpty,hsnIsRec,hsnIsSum)
%%]

%%[7 export(hsnORow,hsnCRow,hsnORec,hsnCRec,hsnOSum,hsnCSum)
%%]

%%[7 export(positionalFldNames,ppFld,mkExtAppPP,mkPPAppFun)
%%]

%%[7 export(uidHNm)
%%]

%%[8 import (FPath) export(hsnUndefined,putPPLn,Verbosity(..),putCompileMsg)
%%]

%%[8 export(hsnPrefix,hsnSuffix)
%%]

%%[8 export(groupSortOn)
%%]

%%[8_1 export(EHCCodeType(..),ehcoptCodeJava,ehcoptCode)
%%]

%%[9 export(hsnOImpl,hsnCImpl)
%%]

%%[9 export(showPP)
%%]

%%[9 export(assocLValues)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.HsName.type
data HsName                         =   HNm String
                                    deriving (Eq,Ord)

instance Show HsName where
  show (HNm s) = s
%%]

%%[1
instance PP HsName where
  pp h = pp (show h)
%%]

%%[7.HsName.type -1.HsName.type
data HsName                         =   HNm String
                                    |   HNPos Int
                                    deriving (Eq,Ord)

instance Show HsName where
  show (HNm s    )  = s
  show (HNPos p  )  = show p
%%]

%%[1.HsName.Base
hsnArrow, hsnUnknown, hsnInt, hsnChar, hsnWild :: HsName
hsnArrow                            =   HNm "->"
hsnUnknown                          =   HNm "??"
hsnInt                              =   HNm "Int"
hsnChar                             =   HNm "Char"
hsnWild                             =   HNm "_"

hsnProd                             ::  Int -> HsName
hsnProd         i                   =   HNm  (',' : show i)

hsnIsArrow, hsnIsProd               ::  HsName -> Bool
hsnIsArrow      hsn                 =   hsn == hsnArrow
hsnIsProd       (HNm (',':_))       =   True
hsnIsProd       _                   =   False

hsnProdArity                        ::  HsName -> Int
hsnProdArity    (HNm (_:ar))        =   read ar
%%]

%%[3
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   HNm ("un" ++ show nm)

hsnIsUn         (HNm ('u':'n':_))   =   True
hsnIsUn         _                   =   False

hsnUnUn         (HNm ('u':'n':nm))  =   HNm nm
%%]

%%[6
hsnStar                             =   HNm "*"
%%]

%%[7
hsnORow                             =   HNm "(|"
hsnCRow                             =   HNm "|)"
hsnOSum                             =   HNm "(<"
hsnCSum                             =   HNm ">)"
hsnORec                             =   HNm "("
hsnCRec                             =   HNm ")"

hsnRow                              =   HNm "Row"
hsnRec                              =   HNm "Rec"
hsnSum                              =   HNm "Var"
hsnRowEmpty                         =   HNm (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow

positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map HNPos [1..]
%%]

%%[8
hsnPrefix                           ::  String -> HsName -> HsName
hsnPrefix   p   hsn                 =   HNm (p ++ show hsn)

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p             =   HNm (show hsn ++ p)
%%]

%%[8
hsnUndefined                        =   HNm "undefined"
%%]

%%[9
hsnOImpl                            =   HNm "(#"
hsnCImpl                            =   HNm "#)"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.UID.Base
newtype UID= UID [Int] deriving (Eq,Ord)
%%]

%%[2.UID.Rest
type UIDL = [UID]

instance Show UID where
  show (UID (l:ls)) = foldl (\ls l -> show l ++ "_" ++ ls) (show l) ls
%%]

%%[2.UID.mkNewLevUID
uidNext :: UID -> UID
uidNext (UID (l:ls)) = UID (l+1:ls)

mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u@(UID ls) = (uidNext u,UID (0:ls))
%%]

%%[2.UID.Utils
mkNewLevUID2 u = let { (u',u1)     = mkNewLevUID   u; (u'',u2)        = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3)        = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3,u4)     = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3,u4,u5)  = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)

uidStart :: UID
uidStart = UID [0]

mkNewUID :: UID -> (UID,UID)
mkNewUID   uid = (uidNext uid,uid)

mkNewUIDL :: Int -> UID -> [UID] -- assume sz > 0
mkNewUIDL sz uid
  =  let  l = take sz . iterate (\(nxt,uid) -> mkNewUID nxt) . mkNewUID $ uid
     in   map snd l

instance PP UID where
  pp uid = text (show uid)
%%]

%%[7
uidHNm :: UID -> HsName
uidHNm = HNm . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Building specific structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.mkApp.Base
type MkConAppAlg t = (HsName -> t,t -> t -> t,t -> t,t -> t)

mkApp :: MkConAppAlg t -> [t] -> t
mkApp (_,app,top,_) ts
  =  case ts of
       [t]  ->  t
       _    ->  top (foldl1 app ts)
%%]

%%[1.mkApp.mkConApp
mkConApp :: MkConAppAlg t -> HsName -> [t] -> t
mkConApp alg@(con,_,_,_) c ts = mkApp alg (con c : ts)
%%]

%%[1.mkApp.mkProdApp
mkProdApp :: MkConAppAlg t -> [t] -> t
mkProdApp alg ts = mkConApp alg (hsnProd (length ts)) ts
%%]

%%[7 -1.mkApp.mkProdApp
%%]

%%[1.mkApp.Rest
mkArrow :: MkConAppAlg t -> t -> t -> t
mkArrow alg@(con,_,_,_) a r = mkApp alg [con hsnArrow,a,r]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[ppAppTop.1
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) args dflt
  =  if       hsnIsArrow conNm  then     ppListSep "" "" (" " ++ show hsnArrow ++ " ") args
     else if  hsnIsProd conNm   then     ppListSep "(" ")" "," args
%%]

%%[1.PP.ppAppTop
%%@ppAppTop.1
                                else     dflt
%%]

%%[7.PP.ppAppTop -1.PP.ppAppTop
%%@ppAppTop.1
     else if  hsnIsRec  conNm   then     ppListSep (hsnORec >|< con) hsnCRec "," args
     else if  hsnIsSum  conNm   then     ppListSep (hsnOSum >|< con) hsnCSum "," args
     else if  hsnIsRow  conNm   then     ppListSep (hsnORow >|< con) hsnCRow "," args
                                else     dflt
%%]

%%[1.PP.NeededByExpr
type PP_DocL = [PP_Doc]

ppListSep :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSep o c s pps = o >|< hlist (intersperse (pp s) (map pp pps)) >|< c

ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  pp_parens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"
%%]

%%[1.PP.Rest
ppCommaList :: PP a => [a] -> PP_Doc
ppCommaList = ppListSep "[" "]" ","

ppSpaced :: PP a => [a] -> PP_Doc
ppSpaced = ppListSep "" "" " "

ppListSepFill :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepFill o c s pps
  = l pps
  where l []      = o >|< c
        l [p]     = o >|< pp p >|< c
        l (p:ps)  = fill ((o >|< pp p) : map (s >|<) ps) >|< c
%%]

%%[7
ppFld :: String -> HsName -> HsName -> PP_Doc -> PP_Doc
ppFld sep positionalNm nm f
  = if nm == positionalNm then f else nm >#< sep >#< f

mkPPAppFun :: HsName -> PP_Doc -> PP_Doc
mkPPAppFun c p = if c == hsnRowEmpty then empty else p >|< "|"

mkExtAppPP :: (HsName,PP_Doc,PP_DocL) -> (HsName,PP_Doc,PP_DocL,PP_Doc) -> (PP_Doc,PP_DocL)
mkExtAppPP (funNm,funNmPP,funPPL) (argNm,argNmPP,argPPL,argPP)
  =  if hsnIsRec funNm || hsnIsSum funNm
     then (mkPPAppFun argNm argNmPP,argPPL)
     else (funNmPP,funPPL ++ [argPP])
%%]

%%[9
showPP :: PP a => a -> String
showPP x = disp (pp x) 100 ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Putting stuff on output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
putPPLn :: PP_Doc -> IO ()
putPPLn pp = putStrLn (disp pp 4000 "")

putCompileMsg :: EHCOpts -> String -> HsName -> FPath -> IO ()
putCompileMsg opts msg modNm fNm
  = if ehcoptVerbosity opts >= VerboseNormal
    then putStrLn (strBlankPad 25 msg ++ " " ++ strBlankPad 15 (show modNm) ++ " (" ++ fpathToStr fNm ++ ")")
    else return ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prio computation for need of parenthesis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ParNeed
data ParNeed =  ParNotNeeded | ParNeededLow | ParNeeded | ParNeededHigh | ParOverrideNeeded
                deriving (Eq,Ord)

type ParNeedL = [ParNeed]

parNeedApp :: HsName -> (ParNeed,ParNeed,ParNeedL)
parNeedApp conNm
  =  let  pr  | hsnIsArrow  conNm   =  (ParNeededLow,ParNotNeeded,[ParNotNeeded,ParNeeded])
              | hsnIsProd   conNm   =  (ParOverrideNeeded,ParNotNeeded,repeat ParNotNeeded)
              | otherwise           =  (ParNeeded,ParNeededHigh,repeat ParNeededHigh)
     in   pr

ppParNeed :: PP p => ParNeed -> ParNeed -> p -> PP_Doc
ppParNeed locNeed globNeed p
  = par (pp p)
  where par = if globNeed > locNeed then pp_parens else id
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Co/Contra variance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.CoContraVariance
data CoContraVariance =  CoVariant | ContraVariant | CoContraVariant deriving (Show,Eq)
%%]

%%[4.cocoOpp
cocoOpp :: CoContraVariance -> CoContraVariance
cocoOpp  CoVariant      =   ContraVariant
cocoOpp  ContraVariant  =   CoVariant
cocoOpp  _              =   CoContraVariant
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Verbosity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data Verbosity
  = VerboseQuiet | VerboseNormal | VerboseALot
  deriving (Eq,Ord)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[EHCCodeOpts.8.1
-- functions to keep the version .8 parts working
ehcoptCode     o = case ehcoptCodeType o of { EHCCoreCode -> True; _ -> False }
ehcoptCodeJava o = case ehcoptCodeType o of { JavaCode    -> True; _ -> False }
	
data EHCCodeType = EHCCoreCode | JavaCode | GHCCoreCode 
%%]

%%[EHCOpts.1
data EHCOpts    = EHCOptions    {  ehcoptDumpPP         ::  Maybe String
                                ,  ehcoptShowTopTyPP    ::  Bool
                                ,  ehcoptHelp           ::  Bool
                                ,  ehcoptDebug          ::  Bool
%%]

%%[EHCOpts.8
                                ,  ehcoptCode           ::  Bool
                                ,  ehcoptCodeJava       ::  Bool
                                ,  ehcoptSearchPath     ::  [String]
                                ,  ehcoptVerbosity      ::  Verbosity
%%]


%%[EHCOpts.8.1
                                ,  ehcoptCodeType       ::  EHCCodeType
                                ,  ehcoptSearchPath     ::  [String]
                                ,  ehcoptVerbosity      ::  Verbosity
%%]

%%[defaultEHCOpts.1
defaultEHCOpts  = EHCOptions    {  ehcoptDumpPP         =   Just "pp"
                                ,  ehcoptShowTopTyPP    =   False
                                ,  ehcoptHelp           =   False
                                ,  ehcoptDebug          =   False
%%]

%%[defaultEHCOpts.8
                                ,  ehcoptCode           =   True
                                ,  ehcoptCodeJava       =   False
                                ,  ehcoptSearchPath     =   []
                                ,  ehcoptVerbosity      =   VerboseQuiet
%%]
%%[defaultEHCOpts.8.1
                                ,  ehcoptCodeType       =   GHCCoreCode
                                ,  ehcoptSearchPath     =   []
                                ,  ehcoptVerbosity      =   VerboseQuiet
%%]

%%[cmdLineOptsA.1
cmdLineOpts  
  =  [  Option "p"  ["pretty"]        (OptArg oPretty "pp|ast|no")
          "do output pretty printed version of src, default=pp"
     ,  Option "d"  ["debug"]         (NoArg oDebug)
          "include debug info, for now: dump extra info in ast pretty print"
     ,  Option ""   ["show-top-ty"]   (OptArg oShowTopTy "yes|no")
          "show top ty, default=no"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
%%]

%%[cmdLineOptsA.8
     ,  Option "c"  ["code"]   (OptArg oCode "java")
          "dump code (java -> .java) on file, default=code (-> .code)"
%%]

%%[cmdLineOptsA.8.1
     ,  Option "c"  ["code"]   (OptArg oCode "ghc-core")
          "dump code (java -> .java, ghc-core -> .hcr) on file, default=ghc-core (-> .hcr)"
%%]

%%[cmdLineOptsB.1
  where  oPretty     ms  o =  case ms of
                                Just "no"   -> o { ehcoptDumpPP        = Nothing   }
                                Just p      -> o { ehcoptDumpPP        = Just p    }
                                _           -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcoptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcoptHelp          = True    }
         oDebug          o =  o { ehcoptDebug         = True    }
%%]

%%[cmdLineOptsB.8
         oCode       ms  o =  case ms of
                                Just "java"  -> o { ehcoptCodeJava     = True      }
                                _            -> o { ehcoptCode         = True      }
%%]

%%[cmdLineOptsB.8.1
         oCode       ms  o =  case ms of
                                Just "java"      -> o { ehcoptCodeType = JavaCode    }
                                Just "ghc-core"  -> o { ehcoptCodeType = GHCCoreCode }
                                _                -> o { ehcoptCodeType = EHCCoreCode }
%%]

%%[1.Options
%%@EHCOpts.1
                                }

%%@defaultEHCOpts.1
                                }

%%@cmdLineOptsA.1
     ]
%%@cmdLineOptsB.1
%%]

%%[8.Options -1.Options
%%@EHCOpts.1
%%@EHCOpts.8
                                }

%%@defaultEHCOpts.1
%%@defaultEHCOpts.8
                                }

%%@cmdLineOptsA.1
%%@cmdLineOptsA.8
     ]
%%@cmdLineOptsB.1
%%@cmdLineOptsB.8
%%]

%%[8_1.Options -(1.Options 8.Options)
%%@EHCCodeOpts.8.1
%%@EHCOpts.1
%%@EHCOpts.8.1
                                }

%%@defaultEHCOpts.1
%%@defaultEHCOpts.8.1
                                }

%%@cmdLineOptsA.1
%%@cmdLineOptsA.8.1
     ]
%%@cmdLineOptsB.1
%%@cmdLineOptsB.8.1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AssocL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AssocL
type AssocL k v = [(k,v)]
%%]

%%[1.ppAssocL
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = ppListSepFill "[ " " ]" ", " (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[9.ppAssocL -1.ppAssocL
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = pp_block "[" "]" "," (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[2
assocLMap :: (k -> v -> (k',v')) -> AssocL k v -> AssocL k' v'
assocLMap f = map (uncurry f)

assocLMapSnd :: (v -> v') -> AssocL k v -> AssocL k v'
assocLMapSnd f = assocLMap (\k v -> (k,f v))
%%]

%%[9
assocLValues :: AssocL k v -> [v]
assocLValues = map snd
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Misc
hdAndTl :: [a] -> (a,[a])
hdAndTl (a:as) = (a,as)
%%]

%%[2.Misc
unionL :: Eq a => [[a]] -> [a]
unionL = foldr union []
%%]

%%[4.listCombineUniq
listCombineUniq :: Eq a => [[a]] -> [a]
listCombineUniq = nub . concat
%%]

%%[8
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn sel = groupBy (\e1 e2 -> sel e1 == sel e2) . sortBy (\e1 e2 -> sel e1 `compare` sel e2)
%%]

%%[8
strBlankPad :: Int -> String -> String
strBlankPad n s = s ++ replicate (n - length s) ' '
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Strictness domain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex

%%]

%%[(8 codegen strictana) module {%{EH}StrictnessDomain}
%%]

-- PP
%%[(8 codegen) import(EH.Util.Pretty)
%%]

%%[(8 codegen strictana) hs import(Control.Monad, {%{EH}Base.Serialize})
%%]

%%[(50 codegen grin) hs import(Data.Typeable(Typeable), Data.Generics(Data))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Relevance types for strictness analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen strictana) hs export(RelInfo(..), RelTy(..), RelVal(..))
data RelInfo 
  = RelInfo { relTy  :: RelTy
            , relVal :: RelVal
            , satl   :: Int
            , satr   :: Int
            }
  deriving ( Eq, Show, Data, Typeable )

data RelTy
  = RelTy_None
  | RelTy_Func RelTy RelVal RelTy
  | RelTy_Unit
  deriving ( Eq, Show, Data, Typeable )
  
data RelVal = RelVal_S
            | RelVal_L
  deriving ( Eq, Show, Data, Typeable )

instance PP RelInfo where
  pp (RelInfo relty relval satl satr) = text "(" >|< 
                                        pp relty >|<
                                        text ", " >|<
                                        pp relval >|<
                                        text ", " >|<
                                        pp satl >|<
                                        text ", " >|<                                        
                                        pp satr >|<
                                        text ")" 

instance PP RelTy where
  pp RelTy_None            = text "None"
  pp (RelTy_Func fr an to) = pp fr >#< "-" >|< pp an >|< "->" >#< pp to
  pp (RelTy_Unit)          = text "()"

instance PP RelVal where
  pp RelVal_S = text "S"
  pp RelVal_L = text "L" 

instance Serialize RelInfo where
  sput (RelInfo relty relval satl satr) = sput relty >> sput relval >> sput satl >> sput satr
  sget = liftM4 RelInfo sget sget sget sget  
  
instance Serialize RelTy where
  sput (RelTy_None)          = sputWord8 0 
  sput (RelTy_Func fr an to) = sputWord8 1 >> sput fr >> sput an >> sput to
  sput (RelTy_Unit)          = sputWord8 2 
  sget = do t <- sgetWord8
            case t of
              0 -> return RelTy_None  
              1 -> liftM3 RelTy_Func sget sget sget
              2 -> return RelTy_Unit 
              
instance Serialize RelVal where
  sput (RelVal_S) = sputWord8 0
  sput (RelVal_L) = sputWord8 1
  sget = do t <- sgetWord8
            case t of
              0 -> return RelVal_S
              1 -> return RelVal_L 
%%]
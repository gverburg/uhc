module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner
import Debug.Trace

lyrops  = [":", ","]
lyrkeys = [ "layer", "extends", "params", "uses", "pattern"
          , "in", "out", "inout", "node", "interface"]

main :: IO ()
main = do { impls <- compile "Equation"
          ; putStr $ show impls
          }

compile :: String -> IO Implementation
compile name = do target <- parseLayer name
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  return $ head impls ------------------ foldImpls   

parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	lyrkeys lyrops "{(:)}" "" (file ++ ".inf")
                     ; layer  <- parseIO pLayer tokens
                     ; checkLayerName file layer
                     ; player <- resolveParents layer
                     ; checkParamAccess player
                     ; return player
                     }

pLayer :: Parser Token Layer
pLayer = Layer_RawLayer <$ pKey "layer" 
                        <*> pConid
                        <*> opt (Just <$ pKey "extends" <*> pConid) Nothing
                        <*> pList pInterface

pInterface :: Parser Token Interface
pInterface = Interface_Interface <$ pKey "interface"
                                    <*> pId
                                    <*  pKey "params"
                                    <*> pList pParam
                                    <*> opt (pKey "uses" *> pList pParam) []
                                    <*  pKey "pattern" 
                                    <*> pString

pParam :: Parser Token Parameter
pParam = mkParam <$> pDirection 
                   <*> pVarid 
                   <*  pKey ":" 
                   <*> pConid 
                   <*> (opt (pParens_pCommas pVarid) [])
   where mkParam io nm ty ds = Parameter_Parameter nm io ty (vis ds) (fltr ds)
         vis   ds            = not (elem "private" ds)
         fltr  ds            = filter (/= "private") ds

pDirection :: Parser Token Direction
pDirection =    (In <$ pKey "in")
            <|> (InOut <$ pKey "inout")
            <|> (Out <$ pKey "out")
            <|> (Node <$ pKey "node")

----------------- LAYER POST PROCESSING -----------------

resolveParents :: Layer -> IO Layer
resolveParents (Layer_RawLayer n Nothing is) = return $ Layer_Layer n Nothing is
resolveParents (Layer_RawLayer n (Just p) is) 
   = do parent <- parseLayer p
        return $ Layer_Layer n (Just parent) is

checkLayerName :: String -> Layer -> IO Layer
checkLayerName nm l@(Layer_RawLayer n p is) = if n == nm then return l else error msg
   where msg = "Layer file: " ++ nm ++ " contains layer with name: " ++ n


checkParamAccess :: Layer -> IO ()
checkParamAccess child@(Layer_Layer n Nothing is) = return ()
checkParamAccess child@(Layer_Layer n (Just parent) is) 
   = checkSubset (usedParams child) (visibleParams parent)
   where checkSubset us vs = let ba = badaccess us vs in
                                 if null $ ba
                                 then return () 
                                 else error $ msg ba
         badaccess us vs = foldr (++) [] (zipWith notin us (repeat vs))
         msg ba = "you lose" ++ show ba
   

notin (i,ps) vis = map (\x -> i ++ "." ++ x) [p | p <- ps, not (elem p vs)]
   where vs = foldr (++) [] [xs | (i',xs) <- vis, i' == i]
   
type ParamFilter = Parameter -> Bool


visibleParams :: Layer -> [(String,[String])]
visibleParams l = ps2n $ layerParams isVisible isVisible l
   where isVisible (Parameter_Parameter _ _ _ v _) = v
         ps2n = map (\(n,ps) -> (n, map p2n ps) )
         p2n (Parameter_Parameter n d t v ds) = n

usedParams :: Layer -> [(String,[String])]
usedParams l = ps2n $ layerParams (const False) (const True) l
   where ps2n = map (\(n,ps) -> (n, map p2n ps) )
         p2n (Parameter_Parameter n d t v ds) = n


layerParams :: ParamFilter -> ParamFilter -> Layer -> [(String,[Parameter])]
layerParams pf uf l = map (\i->(name i, tracer $ params pf uf i)) (ifaces l)

ifaces :: Layer -> [Interface]
ifaces (Layer_Layer n p is) = is


params :: ParamFilter -> ParamFilter -> Interface -> [Parameter]
params pf uf (Interface_Interface n ps us p) = filter pf ps ++ filter uf us

tracer = (\x -> trace (show x) x) 


get :: Named a => String -> [a] -> a
get nm xs = if null fs then err else head fs
   where fs  = [x| x <- xs, name x == nm]
         err = error $ "value with name: " ++ nm ++ " was not found."

get' :: String -> [(String,a)] -> a
get' nm xs = if null fs then err else head fs
   where fs  = [x| (n,x) <- xs, n == nm]
         err = error $ "value with name: " ++ nm ++ " was not found."

getR :: Named a => [a] -> String -> a
getR = flip get

----------------- IMPLEMENTATION POST PROCESSING -----------------

type ImplAlg = ( String -> Layer               -- name to layer
               , String -> Interface           -- name to interface
               , String -> String -> Parameter -- name & name to param
               )

foldImpl :: ImplAlg -> Implementation -> Implementation
foldImpl (n2l, n2i, nn2p) = f0
   where f0 (Implementation_RawImplementation nm rs)  
             = Implementation_Implementation (n2l nm) (map f1 rs)
         f1 (Rule_RawRule nm ni pres post) 
             = Rule_Rule nm (n2i ni) (map f2 pres) (f2 post)
         f2 (Judgment_RawJudgment2 nm ni bdy defs) 
             = Judgment_Judgment nm (n2i ni) (map (f3 ni) bdy) defs
         f3 ni (pnm,expr) = BodyAssignment_BodyAssignment (nn2p ni pnm) expr


hierarchy :: Layer -> [Layer]
hierarchy l@(Layer_Layer _ Nothing _) = [l]
hierarchy l@(Layer_Layer _ (Just p) _) = hierarchy p ++ [l]


---------------------------------------------------------------------------

parameters :: Layer -> [(String, Parameter)]
parameters = undefined


resolve :: Layer -> Implementation -> IO Implementation
resolve l i = return $ foldImpl (a,b,c) i
   where a = getR $ hierarchy l
         b = getR $ ifaces l
         c pi p2 = get p2 $ get' pi (allParams l)
         allParams = layerParams (const True) (const True)

linkLayer :: Layer -> IO [Implementation]
linkLayer l 
   = do let hier = hierarchy l
        let intsd = getR $ ifaces l
        let alg = (getR hier, intsd, undefined)
        return undefined

resolveImpls :: [Layer] -> IO [Implementation]
resolveImpls []     = return []
resolveImpls (l:ls) = do impl <- parseImpl (name l) 
                         impl <- resolve l impl
                         impls <- resolveImpls ls
                         return $ impl : impls

-------------------------------------------------------------------------------

implops  = [":.",";"]
implkeys = ["implementation","of","rule","implements","pre","post", "where"]

parseImpl :: String -> IO Implementation
parseImpl file = do { tokens <- scanFile implkeys implops "{(:;)}=.|" "" (file ++ ".impl")
                    ; parseIO pImpl tokens
                    }

pImpl :: Parser Token Implementation
pImpl = Implementation_RawImplementation 
                  <$  pKey "implementation"
                  <*  pKey "of"
                  <*> pConid
                  <*> pList pRule

pRule :: Parser Token Rule
pRule = Rule_RawRule <$  pKey "rule"
                  <*> pVarid
                  <*  pKey "implements"
                  <*> pConid
                  <*> opt (pKey "pre" *> pList pJudge) []
                  <* pKey "post" <*> pJudge

pJudge = pRawJudge1 <|> pRawJudge2

pRawJudge1 = mkJudge1 <$> pConid 
                                   <*  pKey "."
                                   <*> pConid
                                   <*  pKey "="
                                   <*> pString
                                   <*> pDefinitions
   where mkJudge1 int nm bdy defs = Judgment_RawJudgment1 nm int bdy defs

pRawJudge2 = mkJudge2 <$> pConid 
                                   <*  pKey "."
                                   <*> pConid
                                   <*> pList (pKey "|" *> pBinding)
                                   <*> pDefinitions
   where mkJudge2 int nm bdy defs = Judgment_RawJudgment2 nm int bdy defs

pDefinitions :: Parser Token [(String,String)]
pDefinitions = opt (pKey "where" *> pList pBinding) []

pBinding :: Parser Token (String,String)
pBinding = (,) <$> pId <* pKey "=" <*> pString

pId :: Parser Token String
pId = pConid <|> pVarid

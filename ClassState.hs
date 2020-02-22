module ClassState
where
import Data.Map (Map)
import qualified Data.Map as Map


-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq , Ord)

-- TODO - Trebuie definit ClassState
data ClassState  =  ClassState {class_map :: (Map [String] InstrType)} deriving Show

initEmptyClass :: ClassState
initEmptyClass = ClassState (Map.empty)

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass cs itype attr = ClassState $ Map.insert attr itype (class_map cs)

getValues :: ClassState -> InstrType -> [[String]]
getValues cs itype = map fst l
                where
                l = Map.toList (Map.filterWithKey (\k v -> v == itype) ( class_map cs ))

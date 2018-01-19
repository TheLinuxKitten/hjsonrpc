
module Data.Aeson.JsonUtils
  ( parseListOfKeyObject
  ) where

import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

-- | Devuelve la lista de objetos asociados a la clave del objeto. Los objetos
-- se procesan con la función, que recibe como primer parámetro la clave del
-- objeto y el objeto.
parseListOfKeyObject :: Object
                     -> Text
                     -> (Text -> Value -> Parser a)
                     -> Parser [a]
parseListOfKeyObject o k p = withObject (T.unpack k)
                                         (mapM (uncurry p) . HM.toList)
                                         (o HM.! k)



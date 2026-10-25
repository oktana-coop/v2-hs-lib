module Utils.Either (bothOrErrors) where

import Data.List.NonEmpty (NonEmpty (..))

-- Unlike `Either`'s applicative, a failure on one side does not hide a failure on the other.
bothOrErrors :: Either e a -> Either e b -> Either (NonEmpty e) (a, b)
bothOrErrors (Left err1) (Left err2) = Left (err1 :| [err2])
bothOrErrors (Left err1) _ = Left (err1 :| [])
bothOrErrors _ (Left err2) = Left (err2 :| [])
bothOrErrors (Right a) (Right b) = Right (a, b)

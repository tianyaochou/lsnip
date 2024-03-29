module Lib where

lastMaybe :: [a] -> Maybe a
lastMaybe [h] = Just h
lastMaybe [] = Nothing
lastMaybe (_ : t) = lastMaybe t

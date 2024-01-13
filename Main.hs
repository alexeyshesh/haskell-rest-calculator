{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Web.Scotty
import Network.HTTP.Types
import Data.Aeson (FromJSON, ToJSON)

data Response = Response {
  operator :: String,
  result :: Maybe Double,
  arguments :: [Double],
  error :: Maybe String
} deriving (Show, Generic)

instance ToJSON Response
instance FromJSON Response


main = scotty 3000 $ do
  get "/:operator/:x/:y" $ do
    operator <- param "operator"
    x <- param "x"
    y <- param "y"
    json Response {
      operator = operator,
      result = case (operator, x, y) of
        ("add", x, y) -> Just (x + y)
        ("sub", x, y) -> Just (x - y)
        ("mul", x, y) -> Just (x * y)
        ("div", x, 0) -> Nothing
        ("div", x, y) -> Just (x / y)
        ("pow", 0, y) -> if y > 0 then Just 0 else Nothing
        ("pow", x, y) -> Just (x ** y)
        (_, _, _) -> Nothing
      ,
      arguments = [x, y],
      Main.error = case (operator, x, y) of
        ("add", _, _) -> Nothing
        ("sub", _, _) -> Nothing
        ("mul", _, _) -> Nothing
        ("div", _, 0) -> Just "Division by zero"
        ("div", _, _) -> Nothing
        ("pow", 0, _) -> if y > 0 then Nothing else Just "Negative power"
        ("pow", _, _) -> Nothing
        ("sqrt", _, _) -> Just "Too many arguments for sqrt"
        (_, _, _) -> Just ("Unknown operator " ++ operator)
    }
  get "/sqrt/:x" $ do
    x <- param "x"
    json Response {
      operator = "sqrt",
      result = if x >= 0 then Just (x ** 0.5) else Nothing,
      arguments = [x],
      Main.error = if x < 0 then Just "Square root of negative number" else Nothing
    }

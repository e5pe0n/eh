{-# LANGUAGE RecordWildCards #-}

module Main where

data CustomerInfo = CustomerInfo
  { firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
  }

customerGeorge :: CustomerInfo
customerGeorge =
  CustomerInfo
    { balance = 100,
      lastName = "Bird",
      firstName = "George",
      widgetCount = 10
    }

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo {..} = firstName <> " " <> lastName <> " " <> show widgetCount <> " " <> show balance

main = print $ showCustomer customerGeorge

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
showCustomer (CustomerInfo first last count balance) =
  let fullName = first <> " " <> last
      name = "name: " <> fullName
      count' = "count: " <> (show count)
      balance' = "balance: " <> (show balance)
   in name <> " " <> count' <> " " <> balance'

main = print $ showCustomer customerGeorge

module Main where

data PreferredContactMethod
  = Email String
  | TextMessage String
  | Mail String String String Int

confirmContact :: PreferredContactMethod -> String
confirmContact contact =
  case contact of
    Email emailAddress -> "Okay, I'll email you at " <> emailAddress
    TextMessage number -> "Okay, I'll text you at" <> number
    Mail street1 street2 citystate zip -> "Okay, I'll send a letter to\n" <> street1 <> "\n" <> street2 <> "\n" <> citystate <> " " <> show zip

main = print ""

module HaskellBook.Examples.Introduction.CreatingModules where

excitingMessage :: String -> String
excitingMessage message =
  "Exciting news: " <> message <> "!!!"

data Name = Name {getName :: String}

data Salutation = Salutation {getSalutation :: String}

data GreetingMessage = GreetingMessage
  { greetingSalutation :: Salutation,
    greetingTo :: Name,
    greetingFrom :: [Name]
  }

defaultMessage :: GreetingMessage
defaultMessage =
  GreetingMessage
    { greetingSalutation = Salutation "Hello",
      greetingTo = Name "Friend",
      greetingFrom = []
    }

formatMessage :: GreetingMessage -> String
formatMessage (GreetingMessage greetingSalutation greetingTo greetingFrom) =
  greetingWithSuffix
  where
    basicGreeting =
      getSalutation greetingSalutation <> " " <> getName greetingTo

    greetingWithSuffix =
      case greetingFrom of
        [] -> basicGreeting <> "!"
        [friend] -> basicGreeting <> ", from: " <> getName friend
        [friendA, friendB] -> basicGreeting <> ", from: " <> getName friendA <> " and " <> getName friendB
        friends -> basicGreeting <> ", from your friends: " <> formatFriendList friends

    formatFriendList friends =
      case friends of
        [] -> ""
        [friend] -> " and " <> getName friend
        (friend : moreFriends) -> getName friend <> ", " <> formatFriendList moreFriends

testMessage :: String
testMessage =
  formatMessage $ defaultMessage {greetingFrom = [Name "test example"]}

module AdventOfCode
    ( Passport(..)
    , newPassport
    , checkRequiredFields
    , checkFieldsValid
    , parseField
    , parsePassport
    , parseFile
    ) where

import Data.List.Split

data Height = Cm Int | In Int deriving (Show)

data Passport = Passport {
  birthYear :: Maybe Int,
  issueYear :: Maybe Int,
  expirationYear :: Maybe Int,
  height :: Maybe Height,
  hairColor :: Maybe String,
  eyeColor :: Maybe String,
  passportID :: Maybe String,
  countryID :: Maybe String
} deriving (Show)

newPassport :: Passport
newPassport = Passport {
  birthYear = Nothing,
  issueYear = Nothing,
  expirationYear = Nothing,
  height = Nothing,
  hairColor = Nothing,
  eyeColor = Nothing,
  passportID = Nothing,
  countryID = Nothing
}

checkRequiredFields :: Passport -> Bool
checkRequiredFields (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
checkRequiredFields _ = False

checkFieldsValid :: Passport -> Bool
checkFieldsValid p = validBirthYear (birthYear p) &&
                     validIssueYear (issueYear p) &&
                     validExpirationYear (expirationYear p) &&
                     validHeight (height p) &&
                     validHairColor (hairColor p) &&
                     validEyeColor (eyeColor p) &&
                     validPassportID (passportID p)
  where
    validBirthYear Nothing = False
    validBirthYear (Just n) = n >= 1920 && n <= 2002
    validIssueYear Nothing = False
    validIssueYear (Just n) = n >= 2010 && n <= 2020
    validExpirationYear Nothing = False
    validExpirationYear (Just n) = n >= 2020 && n <= 2030
    validHeight Nothing = False
    validHeight (Just (Cm v)) = v >= 150 && v <= 193
    validHeight (Just (In v)) = v >= 59 && v <= 76
    isDigit = flip elem ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    isHex = flip elem ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
    validHairColor Nothing = False
    validHairColor (Just ('#':v)) = all isHex v && length v == 6
    validHairColor (Just _) = False
    validEyeColor Nothing = False
    validEyeColor (Just c) = c `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    validPassportID Nothing = False
    validPassportID (Just v) = length v == 9 && all isDigit v


parseField :: Passport -> String -> Passport
parseField p s = case key of
                   "byr" -> p { birthYear = Just (read value)}
                   "iyr" -> p { issueYear = Just (read value)}
                   "eyr" -> p { expirationYear = Just (read value)}
                   "hgt" -> p { height = parseHeight value}
                   "hcl" -> p { hairColor = Just value}
                   "ecl" -> p { eyeColor = Just value}
                   "pid" -> p { passportID = Just value}
                   "cid" -> p { countryID = Just value}
                   _ -> p
  where
    [key,value] = splitOn ":" s

parseHeight :: String -> Maybe Height
parseHeight v = case unit of
    "cm" -> Just $ Cm (read value)
    "in" -> Just $ In (read value)
    _ -> Nothing
  where
    (value,unit) = splitAt (length v - 2) v


parsePassport :: [String] -> Passport
parsePassport = foldl parseField newPassport . concat . map words

parseFile :: String -> [Passport]
parseFile = map parsePassport . splitWhen (== "") . lines

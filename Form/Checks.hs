-- | Simple functions to check that text-based input values satisfy validations.

{-# LANGUAGE OverloadedStrings #-}

module Form.Checks where

import           Control.Monad       ((>=>))
import           Data.Char           (isDigit)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import qualified Data.Time           as Time
import           Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as Email
import           Text.Read           (readMaybe)


-- | Checks that text has a value other than white space.
requiredText
  :: Text -- ^ Error message if value is empty
  -> Text -- ^ Field value to check
  -> Either Text Text -- 'Left' has the error message; 'Right' has the input value with white space stripped.
requiredText msg x = if T.null cleaned then Left msg else Right cleaned
  where cleaned = T.strip x


-- | Checks that text can be parsed as a valid 'EmailAddress'.
email :: Text -> Either Text EmailAddress
email x = maybe
  (Left "Please enter a valid email address") Right (Email.emailAddress (encodeUtf8 x))

-- | Like 'email' but leaves the parsed email as 'Text' after canonicalization.
emailText :: Text -> Either Text Text
emailText x = maybe
  (Left "Please enter a valid email address") Right (decodeUtf8 <$> Email.canonicalizeEmail (encodeUtf8 x))

-- | Parses a number with some optional range.
numeric
  :: (Ord a, Read a, Show a)
  => Text -- Unit
  -> (Maybe a, Maybe a)  -- Optional low / high limit
  -> Text -- Input value
  -> Either Text a
numeric unit (low', high') x = case readMaybe (T.unpack x) of
  Nothing -> Left msg
  Just n  -> let
    isInRange = case (low', high') of
      (Nothing, Nothing)    -> True
      (Just low, Nothing)   -> n >= low
      (Nothing, Just high)  -> n <= high
      (Just low, Just high) -> n >= low && n <= high

    in if isInRange then Right n else Left msg

  where
    msg = "Please enter a valid " <> unit <> case (low', high') of
      (Nothing, Nothing)    -> ""
      (Just low, Nothing)   -> " above " <> tshow low
      (Nothing, Just high)  -> " below " <> tshow high
      (Just low, Just high) -> " between " <> tshow low <> " and " <> tshow high

    tshow = T.pack . show

-- | Simple check for US zip codes format of @NNNNN@ or @NNNNN-NNNN@.
usZipCode :: Text -> Either Text Text
usZipCode = requiredText msg >=> satisfies isValidUsZipCode msg
  where
    msg = "Please enter a valid zip code"

-- | Returns 'True' iff the given text has a valid US Zip Code format.
isValidUsZipCode :: Text -> Bool
isValidUsZipCode txt = T.length leftPart == 5
                  && T.all isDigit leftPart
                  && (T.null rightPart || isRightPartValid rightPart)
  where
    (leftPart, rightPart) = T.breakOn "-" txt
    isRightPartValid x = T.length x == 5 && T.head x == '-' && T.all isDigit (T.drop 1 x)

-- | Simple check for phone numbers by requiring that there are at least 10 digits in the input.
phoneNumberSimple :: Text -> Either Text Text
phoneNumberSimple = requiredText msg >=> satisfies isValidPhoneNumberSimple msg
  where msg = "Please enter a valid phone number"

-- | Returns 'True' iff the input has at least 10 digits.
isValidPhoneNumberSimple :: Text -> Bool
isValidPhoneNumberSimple txt = T.length (T.filter isDigit txt) >= 10

-- | General check for any 'Bool' predicate.
satisfies :: (a -> Bool) -> err -> a -> Either err a
satisfies pred_ err a = if pred_ a then Right a else Left err

-- | Checks that the input has a date format as provided by HTML date fields: @YYYY-MM-DD@.
htmlDate :: Text -> Either Text Time.Day
htmlDate txt = maybe (Left "Please enter a valid date (YYYY-MM-DD)") Right
  $ Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" (T.unpack txt)

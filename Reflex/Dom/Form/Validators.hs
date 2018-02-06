-- | Combinators for building validated text inputs.

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Form.Validators where

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Time            as Time
import           Text.Email.Validate  (EmailAddress)

import qualified Form.Checks          as Check
import           Reflex.Dom.TextField (TextField)
import qualified Reflex.Dom.TextField as TxtField

-- | A product type capturing both a checking function and a way to modify a 'TextField'
-- such that it's attributes match the validation.
--
-- For example, if a validation rule requrise that a text field parse as a number and
-- have a maximum value of 10, the validator can capture this rule while also setting
-- the HTML input attributes that correspond to these rules (@type="number" max="10"@)
data Validator t m a = Validator (Text -> Either Text a) (TextField t m -> TextField t m)
  deriving (Functor)

-- | Validates an email and configures the input to have an @email@ type.
validateEmail :: Validator t m EmailAddress
validateEmail = Validator Check.email TxtField.setEmailType

-- | Like 'validateEmail' but leaves the email as 'Text'.
validateEmailText :: Validator t m Text
validateEmailText = Validator Check.emailText TxtField.setEmailType

validatePhoneNumberSimple :: Validator t m Text
validatePhoneNumberSimple = Validator Check.phoneNumberSimple TxtField.setPhoneNumberType

validateNumeric :: (Ord a, Num a, Read a, Show a) => Text -> (Maybe a, Maybe a) -> Maybe a -> Validator t m a
validateNumeric unit range@(low, high) step = Validator
  (Check.numeric unit range)
  ( maybe id TxtField.setMin low
  . maybe id TxtField.setMax high
  . maybe id TxtField.setStep step
  . TxtField.setNumberType
  )

validateDate :: Validator t m Time.Day
validateDate = Validator Check.htmlDate TxtField.setDateType

validateText' :: Text -> Validator t m Text
validateText' msg = Validator (Check.requiredText msg) id

validateText :: Validator t m Text
validateText = validateText' "Please enter a value"


validateUsZipCode :: Validator t m Text
validateUsZipCode = Validator Check.usZipCode id

-- | Converts a validator into one that permits empty values defaulting to 'Nothing' in the empty case.
optional :: Validator t m a -> Validator t m (Maybe a)
optional = optionalWith Nothing Just

-- | Converts a validator into one that permits empty values.
optionalWith
  :: b -- ^ Default when value is empty
  -> (a -> b) -- ^ Function to convert non-empty value to result type
  -> Validator t m a  -- ^ Input validator
  -> Validator t m b
optionalWith empty notEmpty (Validator check mods) = Validator
  (\txt -> let
      cleaned = T.strip txt
    in if T.null cleaned then Right empty else notEmpty <$> check txt
  )
  (TxtField.setOptional . mods)

-- | A no-op validator for text that strips whitespace.
optionalText :: Validator t m Text
optionalText = Validator (Right . T.strip) TxtField.setOptional

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Reflex.Dom.TextField where

import           Control.Lens    ((%~), (^.))
import           Control.Lens.TH (makeLenses)
import           Data.Default    (Default, def)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Reflex.Dom.Core


-- ^ Core parts to a text field, meant to be built by combinators.
data TextFieldType
  = TextInputType !Text  -- ^ Text input with a given element type (e.g. @"text"@, @"date"@, etc.)
  | TextAreaType         -- ^ Text area with a given size in rows

data TextField t m = TextField{
  _textField_classes    :: ![Text],
  _textField_type       :: !TextFieldType,
  _textField_initValue  :: !Text,
  _textField_attrs      :: !(Map Text Text),
  _textField_insidePre  :: !(m ()),  -- ^ Stuff appearing *before* the input element within the widget
  _textField_insidePost :: !(m ()),  -- ^ Stuff appearing *after* the input element within the widget
  _textField_outsidePre :: !(m ()),  -- ^ Stuff appearing *before* the widget itself
  _textField_setValue   :: !(Event t Text)
  } deriving (Generic)
makeLenses 'TextField

instance (Applicative m, Reflex t) => Default (TextField t m) where def = TextField{
  _textField_classes    = [],
  _textField_type       = TextInputType "text",
  _textField_initValue  = "",
  _textField_attrs      = "required"=:"required",
  _textField_insidePre  = pure (),
  _textField_insidePost = pure (),
  _textField_outsidePre = pure (),
  _textField_setValue   = never
  }

mkField :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m) => TextField t m -> m (Dynamic t Text)
mkField cfg = do
  cfg^. textField_outsidePre
  elAttr "div" ("class" =: T.intercalate " " ("ui input" : cfg^. textField_classes)) $ do
    cfg^. textField_insidePre

    textDyn <- case cfg^. textField_type of
      TextInputType typ ->
        _textInput_value <$> textInput (def
          & textInputConfig_inputType    .~ typ
          & textInputConfig_initialValue .~ cfg^. textField_initValue
          & textInputConfig_attributes   .~ pure (cfg^. textField_attrs)
          & textInputConfig_setValue     .~ cfg^. textField_setValue
          )
      TextAreaType ->
        _textArea_value <$> textArea (def
          & textAreaConfig_initialValue .~ cfg^. textField_initValue
          & textAreaConfig_attributes   .~ pure (cfg^. textField_attrs)
          & textAreaConfig_setValue     .~ cfg^. textField_setValue
          )

    cfg^. textField_insidePost
    pure textDyn


addIcon :: (Applicative m) => Either (m ()) (m ()) -> TextField t m -> TextField t m
addIcon mkIcon' cfg = cfg
  & textField_classes    %~ (cls:)
  & textField_insidePost %~ (*> mkIcon)
  where
    (cls, mkIcon) = case mkIcon' of
      Left  mkIcon_ -> ("left icon", mkIcon_)
      Right mkIcon_ -> ("icon",      mkIcon_)


addInnerLabel :: (Applicative m) => Either (m ()) (m ()) -> TextField t m -> TextField t m
addInnerLabel mkLabel' cfg = case mkLabel' of
  Left  mkLabel -> cfg
    & textField_classes   %~ ("labeled":)
    & textField_insidePre %~ (*> mkLabel)
  Right mkLabel -> cfg
    & textField_classes    %~ ("right labeled":)
    & textField_insidePost %~ (*> mkLabel)


addLabel :: (Applicative m) => m () -> TextField t m -> TextField t m
addLabel mkLabel = textField_outsidePre %~ (*> mkLabel)

setFluid :: TextField t m -> TextField t m
setFluid = textField_classes %~ ("fluid":)


basicLabel :: DomBuilder t m => m a -> m a
basicLabel = elAttr "div" ("class"=:"ui basic label")

tagLabel :: DomBuilder t m => m a -> m a
tagLabel = elAttr "div" ("class"=:"ui tag label")

setNumberType :: TextField t m -> TextField t m
setNumberType = textField_type .~ TextInputType "number"

setDateType :: TextField t m -> TextField t m
setDateType = textField_type .~ TextInputType "date"

setPasswordType :: TextField t m -> TextField t m
setPasswordType = textField_type .~ TextInputType "password"

setEmailType :: TextField t m -> TextField t m
setEmailType = textField_type .~ TextInputType "email"

setPhoneNumberType :: TextField t m -> TextField t m
setPhoneNumberType = textField_type .~ TextInputType "tel"

setTextAreaType :: Int -> TextField t m -> TextField t m
setTextAreaType rows
  = (textField_type  .~ TextAreaType)
  . (textField_attrs %~ Map.insert "rows" (T.pack $ show rows))

setOptional :: TextField t m -> TextField t m
setOptional = textField_attrs %~ Map.delete "required"

setFieldName :: Text -> TextField t m -> TextField t m
setFieldName name = textField_attrs %~ Map.insert "name" name

setPlaceholder :: Text -> TextField t m -> TextField t m
setPlaceholder placeholder = textField_attrs %~ Map.insert "placeholder" placeholder

setMin :: (Num a, Show a) => a -> TextField t m -> TextField t m
setMin v = textField_attrs %~ Map.insert "min" (T.pack $ show v)

setMax :: (Num a, Show a) => a -> TextField t m -> TextField t m
setMax v = textField_attrs %~ Map.insert "max" (T.pack $ show v)

setStep :: (Num a, Show a) => a -> TextField t m -> TextField t m
setStep v = textField_attrs %~ Map.insert "step" (T.pack $ show v)

setInitial :: Text -> TextField t m -> TextField t m
setInitial = (textField_initValue .~)

setChangeEvent :: Event t Text -> TextField t m -> TextField t m
setChangeEvent = (textField_setValue .~)

-- Common embellishments
addPrefixLabel :: DomBuilder t m => m () -> TextField t m -> TextField t m
addPrefixLabel lbl = addInnerLabel (Left $ elAttr "div" ("class"=:"ui label") lbl)

addSuffixBasicLabel :: DomBuilder t m => m () -> TextField t m -> TextField t m
addSuffixBasicLabel lbl = addInnerLabel (Right $ basicLabel lbl)

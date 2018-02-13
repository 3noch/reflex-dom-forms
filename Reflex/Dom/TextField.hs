{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Reflex.Dom.TextField where

import           Data.Default    (Default, def)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Reflex.Dom.Core

data TextFieldType
  = TextInputType !Text  -- ^ Text input with a given element type (e.g. @"text"@, @"date"@, etc.)
  | TextAreaType         -- ^ Text area with a given size in rows

-- ^ Describes a text-based input, attributes, embellishments, etc.
--
-- You should build this type with 'def' and combinators instead of directly.
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

instance (Applicative m, Reflex t) => Default (TextField t m) where
  def = TextField{
    _textField_classes    = [],
    _textField_type       = TextInputType "text",
    _textField_initValue  = "",
    _textField_attrs      = "required"=:"required",
    _textField_insidePre  = pure (),
    _textField_insidePost = pure (),
    _textField_outsidePre = pure (),
    _textField_setValue   = never
    }

-- | Builds an @input@ element based on a 'TextField' configuration.
mkField :: forall t m. (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m)
        => TextField t m -> m (Dynamic t Text)
mkField cfg = do
  _textField_outsidePre cfg
  elAttr "div" ("class" =: T.intercalate " " ("ui input" : _textField_classes cfg)) $ do
    _textField_insidePre cfg

    textDyn <- case _textField_type cfg of
      TextInputType typ -> _textInput_value <$> textInput (def
        & textInputConfig_inputType    .~ typ
        & textInputConfig_initialValue .~ _textField_initValue cfg
        & textInputConfig_attributes   .~ pure (_textField_attrs cfg)
        & textInputConfig_setValue     .~ _textField_setValue cfg
        )
      TextAreaType      -> _textArea_value <$> textArea (def
        & textAreaConfig_initialValue .~ _textField_initValue cfg
        & textAreaConfig_attributes   .~ pure (_textField_attrs cfg)
        & textAreaConfig_setValue     .~ _textField_setValue cfg
        )

    _textField_insidePost cfg
    pure textDyn


addIcon :: (Applicative m) => Either (m ()) (m ()) -> TextField t m -> TextField t m
addIcon mkIcon' cfg = cfg{
  _textField_classes    = cls : _textField_classes cfg,
  _textField_insidePost = _textField_insidePost cfg *> mkIcon
  }
  where
    (cls, mkIcon) = case mkIcon' of
      Left  mkIcon_ -> ("left icon", mkIcon_)
      Right mkIcon_ -> ("icon",      mkIcon_)


addInnerLabel :: (Applicative m) => Either (m ()) (m ()) -> TextField t m -> TextField t m
addInnerLabel mkLabel' cfg = case mkLabel' of
  Left  mkLabel -> cfg{
    _textField_classes   = "labeled" : _textField_classes cfg,
    _textField_insidePre = _textField_insidePre cfg *> mkLabel
    }
  Right mkLabel -> cfg{
    _textField_classes    = "right labeled" : _textField_classes cfg,
    _textField_insidePost = _textField_insidePost cfg *> mkLabel
    }


addLabel :: (Applicative m) => m () -> TextField t m -> TextField t m
addLabel mkLabel cfg = cfg{ _textField_outsidePre = _textField_outsidePre cfg *> mkLabel }

setFluid :: TextField t m -> TextField t m
setFluid cfg = cfg{ _textField_classes = "fluid" : _textField_classes cfg }


basicLabel :: DomBuilder t m => m a -> m a
basicLabel = elAttr "div" ("class"=:"ui basic label")

tagLabel :: DomBuilder t m => m a -> m a
tagLabel = elAttr "div" ("class"=:"ui tag label")

setNumberType :: TextField t m -> TextField t m
setNumberType cfg = cfg{ _textField_type = TextInputType "number" }

setDateType :: TextField t m -> TextField t m
setDateType cfg = cfg{ _textField_type = TextInputType "date" }

setPasswordType :: TextField t m -> TextField t m
setPasswordType cfg = cfg{ _textField_type = TextInputType "password" }

setEmailType :: TextField t m -> TextField t m
setEmailType cfg = cfg{ _textField_type = TextInputType "email" }

setPhoneNumberType :: TextField t m -> TextField t m
setPhoneNumberType cfg = cfg{ _textField_type = TextInputType "tel" }

setTextAreaType :: Int -> TextField t m -> TextField t m
setTextAreaType rows cfg = cfg{
  _textField_type  = TextAreaType,
  _textField_attrs = Map.insert "rows" (T.pack $ show rows) (_textField_attrs cfg)
  }

setOptional :: TextField t m -> TextField t m
setOptional cfg = cfg{ _textField_attrs = Map.delete "required" (_textField_attrs cfg) }

setFieldName :: Text -> TextField t m -> TextField t m
setFieldName name cfg = cfg{ _textField_attrs = Map.insert "name" name (_textField_attrs cfg) }

setPlaceholder :: Text -> TextField t m -> TextField t m
setPlaceholder placeholder cfg = cfg{ _textField_attrs = Map.insert "placeholder" placeholder (_textField_attrs cfg) }

setMin :: (Num a, Show a) => a -> TextField t m -> TextField t m
setMin v cfg = cfg{ _textField_attrs = Map.insert "min" (T.pack $ show v) (_textField_attrs cfg) }

setMax :: (Num a, Show a) => a -> TextField t m -> TextField t m
setMax v cfg = cfg{ _textField_attrs = Map.insert "max" (T.pack $ show v) (_textField_attrs cfg) }

setStep :: (Num a, Show a) => a -> TextField t m -> TextField t m
setStep v cfg = cfg{ _textField_attrs = Map.insert "step" (T.pack $ show v) (_textField_attrs cfg) }

setInitial :: Text -> TextField t m -> TextField t m
setInitial val0 cfg = cfg{ _textField_initValue = val0 }

setChangeEvent :: Event t Text -> TextField t m -> TextField t m
setChangeEvent setVal cfg = cfg{ _textField_setValue = setVal }

-- Common embellishments
addPrefixLabel :: DomBuilder t m => m () -> TextField t m -> TextField t m
addPrefixLabel lbl = addInnerLabel (Left $ elAttr "div" ("class"=:"ui label") lbl)

addSuffixBasicLabel :: DomBuilder t m => m () -> TextField t m -> TextField t m
addSuffixBasicLabel lbl = addInnerLabel (Right $ basicLabel lbl)

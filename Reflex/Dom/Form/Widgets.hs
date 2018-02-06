{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

module Reflex.Dom.Form.Widgets where

import           Control.Monad     (when)
import           Control.Monad.Fix (MonadFix)
import           Data.Either       (isLeft)
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import           Reflex.Dom.Core   (DomBuilder, Dynamic, Event, MonadHold,
                                    PostBuild, Reflex, blank, dynText, elAttr,
                                    elDynAttr, ffor, fmapMaybe, holdDyn,
                                    holdUniqDyn, updated, widgetHold, (=:))


formItem
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Eq a)
  => m (Dynamic t (Either Text a)) -> m (Dynamic t (Either Text a))
formItem = formItem' ""

formItem'
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Eq a)
  => Text -> m (Dynamic t (Either Text a)) -> m (Dynamic t (Either Text a))
formItem' classes_ input = mdo
  val <- elDynAttr "div" fieldClasses $ do
    val_ <- input

    widgetHold blank $ ffor hasErrorState $ \isError -> when isError $
      elAttr "div" ("class"=:"ui basic red pointing prompt label visible transition") (dynText latestErr)

    pure val_

  -- Keep the latest error message to prevent updating the DOM
  latestErr <- holdUniqDyn =<< latestLeft "" (updated val)
  let hasErrorState = isLeft <$> updated val

  -- Initially hide error messages since the input is unchanged; show the message once typing starts.
  let mkFieldClasses showErr = "class"=:("field" <> (if showErr then " error " else " ") <> classes_)
  fieldClasses <- holdDyn (mkFieldClasses False) (mkFieldClasses <$> hasErrorState)

  pure val


latestLeft :: (Reflex t, MonadHold t m) => a -> Event t (Either a b) -> m (Dynamic t a)
latestLeft initial x = holdDyn initial $ fmapMaybe (either Just (const Nothing)) x

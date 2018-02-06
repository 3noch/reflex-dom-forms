{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Reflex.Dom.Form.FieldWriter where

import           Control.Applicative ((<|>))
import           Control.Lens        (Lens', (.~), (^.), (^?), _Left)
import           Control.Monad.Fix   (MonadFix)
import           Data.Function       ((&))
import           Data.List           (foldl')
import           Data.Maybe          (fromMaybe)
import           Reflex              (Dynamic, DynamicWriterT,
                                      MonadDynamicWriter, MonadHold, Reflex,
                                      ffor, filterRight, foldDynMaybe,
                                      holdUniqDyn, runDynamicWriterT, tellDyn,
                                      updated)

type FormFieldWriter t err record m = MonadDynamicWriter t [Either err (record -> Maybe record)] m

withFormFields :: (Reflex t, MonadFix m) => record -> DynamicWriterT t [record -> record] m () -> m (Dynamic t record)
withFormFields record0 content = foldlFields record0 . snd <$> runDynamicWriterT content

tellField :: (Reflex t, MonadDynamicWriter t [record -> record] m) => Lens' record a -> Dynamic t a -> m ()
tellField setter dynVal = tellDyn $ ffor dynVal $ \val -> [setter .~ val]


withFormFieldsErr'
  :: (Reflex t, MonadFix m, MonadHold t m, Eq err)
  => record -> DynamicWriterT t [Either err (record -> Maybe record)] m a -> m (a, Dynamic t (Either err record))
withFormFieldsErr' record0 content = do
  (a, updates) <- runDynamicWriterT content
  result <- foldFieldsErr record0 updates
  pure (a, result)

withFormFieldsErr
  :: (Reflex t, MonadFix m, MonadHold t m, Eq err)
  => record -> DynamicWriterT t [Either err (record -> Maybe record)] m () -> m (Dynamic t (Either err record))
withFormFieldsErr record0 content = snd <$> withFormFieldsErr' record0 content

tellFieldErr :: (Reflex t, FormFieldWriter t err record m, Eq a) => Lens' record a -> Dynamic t (Either err a) -> m ()
tellFieldErr lens valDyn = tellDyn $ fmap ((:[]) . fmap updateMaybe) valDyn
  where
    updateMaybe val record = if val == record^.lens then Nothing else Just (record & lens .~ val)


foldlFields :: (Reflex t) => record -> Dynamic t [record -> record] -> Dynamic t record
foldlFields record0 updatesDyn = foldl' (&) record0 <$> updatesDyn

foldFieldsErr
  :: (Reflex t, MonadFix m, MonadHold t m, Eq err)
  => record -> Dynamic t [Either err (record -> Maybe record)] -> m (Dynamic t (Either err record))
foldFieldsErr record0 updatesDyn = do
  let
    mergedFieldsDyn = sequenceA <$> updatesDyn
    validUpdateEv = filterRight (updated mergedFieldsDyn)

  validFormDyn <- foldDynMaybe (flip foldMaybe) record0 validUpdateEv

  errorsDyn <- holdUniqDyn ((^? _Left) <$> mergedFieldsDyn)

  let
    result = errorsDyn >>= \case
      Just err -> pure $ Left err
      Nothing  -> Right <$> validFormDyn

  pure result
  where
    foldMaybe start = foldl' (\record' change -> change (fromMaybe start record') <|> record') Nothing

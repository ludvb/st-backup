{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module STBackup.API.Types.Field where

import Control.Lens (makeLenses, makePrisms)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    fromJSON,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as JSON
  ( Result (Error, Success),
    Value (Object),
    defaultOptions,
    genericToEncoding,
    object,
  )
import Data.Data (Data, Typeable)
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.String (IsString, fromString)
import Data.Text (unpack)
import GHC.Generics (Generic)

type Value a = a

data Error e = NoError | HasError e
  deriving (Eq, Show, Generic, Data, Typeable)

type ErrorS = Error String

instance Hashable e => Hashable (Error e)

instance Foldable Error where
  foldMap f NoError = mempty
  foldMap f (HasError e) = f e

instance Semigroup (Error a) where
  x@(HasError e) <> _ = x
  _ <> x@(HasError e) = x
  _ <> _ = NoError

instance Monoid (Error a) where
  mempty = NoError

$(makePrisms ''Error)

-- | Data type defining API values that may be subject to constraints.
-- Typically, a client will submit a 'Field' with 'fieldError' set to
-- 'Nothing'. If the server rejects the request due to an unmet constraint,
-- the request will be returned with 'fieldError' containing an error message.
data Field a = Field
  { _value :: Value a,
    _error :: ErrorS
  }
  deriving (Eq, Show, Generic, Data, Typeable)

$(makeLenses ''Field)

-- | This instance ignores the 'fieldError' field when set to 'Nothing' in order
-- to simplify the API schema
instance ToJSON a => ToJSON (Field a) where
  toJSON Field {..} =
    case _error of
      HasError e -> JSON.object ["value" .= _value, "error" .= e]
      NoError -> toJSON _value
  {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Field a) where
  parseJSON (JSON.Object v) = do
    _value <- v .: "value"
    _error <- v .:? "error"
    pure
      Field
        { _value = _value,
          _error = case _error of
            Just e -> HasError e
            _ -> NoError
        }
  parseJSON v = do
    let result = fromJSON v
    case result of
      JSON.Error e -> fail e
      JSON.Success x -> pure Field {_value = x, _error = NoError}
  {-# INLINE parseJSON #-}

instance Hashable a => Hashable (Field a)

instance Foldable Field where
  foldMap f Field {..} =
    case _error of
      HasError _ -> mempty
      NoError -> f _value
  {-# INLINE foldMap #-}

toField :: a -> Field a
toField x = Field {_value = x, _error = NoError}
{-# INLINE toField #-}

fromField :: Field a -> a
fromField Field {..} = _value
{-# INLINE fromField #-}

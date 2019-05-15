{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import Control.Lens

import Control.Monad.Except(
  ExceptT,
  MonadError
  )
import Control.Monad.Reader(
  MonadReader,
  ReaderT(..)
  )
import Control.Monad.IO.Class(MonadIO)

data Options = Options { _optionsCommand :: [String] } deriving Show

class HasOptions a where
  getOptions :: Lens' a Options
  getCommand :: Lens' a [String]

  getCommand = getOptions . getCommand

instance HasOptions Options where
  getOptions = id
  getCommand = lens _optionsCommand (\ o c -> o{ _optionsCommand = c })

data AppConfig = AppConfig {
  _appOptions :: Options
    } deriving Show

class HasAppConfig a where
  getConfig :: Lens' a AppConfig


instance HasAppConfig AppConfig where
  getConfig = id

instance HasOptions AppConfig where
  getOptions = lens _appOptions (\ c o -> c{ _appOptions = o })


data InputError = CommandError String | InvalidCommand
  deriving Show

class AsInputError a where
  _InputError :: Prism' a InputError
  _CommandError :: Prism' a String
  _InvalidCommand :: Prism' a ()

  _CommandError = _InputError . _CommandError
  _InvalidCommand = _InputError . _InvalidCommand

instance AsInputError InputError where
  _InputError = id
  _CommandError = prism' CommandError (\ case CommandError s -> Just s
                                              _              -> Nothing)
  _InvalidCommand = prism' (\() -> InvalidCommand)
                           (\ case InvalidCommand -> Just ()
                                   _              -> Nothing)
data RuntimeError = CustomRuntimeError String | ServerDown
  deriving Show

class AsRuntimeError a where
  _RuntimeError :: Prism' a RuntimeError
  _CustomRuntimeError :: Prism' a String
  _ServerDown :: Prism' a ()

  _CustomRuntimeError = _RuntimeError . _CustomRuntimeError
  _ServerDown = _RuntimeError . _ServerDown

instance AsRuntimeError RuntimeError where
  _RuntimeError = id
  _CustomRuntimeError = prism' CustomRuntimeError
                               (\ case (CustomRuntimeError msg) -> Just msg
                                       _                        -> Nothing)
  _ServerDown = prism' (\() -> ServerDown)
                       (\ case ServerDown -> Just ()
                               _          -> Nothing)

data AppError = AppInputError { _appInputError :: InputError }
              | AppRuntimeError { _appRuntimeError :: RuntimeError }
              deriving Show

instance AsInputError AppError where
  _InputError = prism' AppInputError
                       (\ case AppInputError e -> Just e
                               _               -> Nothing)

instance AsRuntimeError AppError where
  _RuntimeError = prism' AppRuntimeError
                        (\ case AppRuntimeError e -> Just e
                                _                 -> Nothing)


newtype AppT m a = App
  {
    unAppT:: (ReaderT AppConfig (ExceptT AppError m)) a

  } deriving
  (
    Functor,
    Applicative,
    Monad,
    MonadReader AppConfig,
    MonadError AppError,
    MonadIO
  )

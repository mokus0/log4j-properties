{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Java.Log4j.PropertyConfigurator
    ( Level(..)
    , BasicLevel(..)
    , LoggingConfiguration(..), emptyLoggingConfiguration
    , ClassConfig(..), newClassConfig
    , AppenderConfiguration(..), newAppenderConfiguration
    , ErrorHandler(..), newErrorHandler
    , LoggerConfiguration(..), newLoggerConfiguration
    , toProperties, fromProperties
    ) where

import Data.Properties
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data Level
    = Basic BasicLevel
    | INHERITED
    | NULL
    deriving (Eq, Show)

data BasicLevel
    = OFF
    | FATAL
    | ERROR
    | WARN
    | INFO
    | DEBUG
    | ALL
    | Other String  
        -- ^ NB: this string will be included verbatim in the generated properties file!
    deriving (Eq, Ord, Show)

data LoggingConfiguration = LoggingConfiguration
    { reset                 :: Bool
    , appenders             :: M.Map String AppenderConfiguration
    , rootLogger            :: Maybe (LoggerConfiguration BasicLevel)
    , loggers               :: M.Map String (LoggerConfiguration Level)
    , objectRenderers       :: M.Map String String
    , throwableRenderer     :: Maybe String
    } deriving (Eq, Show)

emptyLoggingConfiguration = LoggingConfiguration
    { reset                 = False
    , appenders             = M.empty
    , rootLogger            = Nothing
    , loggers               = M.empty
    , objectRenderers       = M.empty
    , throwableRenderer     = Nothing
    } 

data ClassConfig = ClassConfig
    { className     :: String
    , classOptions  :: [(String, String)]
    } deriving (Eq, Show)

newClassConfig clsName = ClassConfig
    { className     = clsName
    , classOptions  = []
    }

data AppenderConfiguration = AppenderConfiguration
    { appenderClass         :: ClassConfig
    , appenderLayout        :: Maybe ClassConfig
    , appenderFilters       :: M.Map String ClassConfig
    , appenderErrorHandler  :: Maybe ErrorHandler
    } deriving (Eq, Show)

newAppenderConfiguration clsName = AppenderConfiguration
    { appenderClass         = newClassConfig clsName
    , appenderLayout        = Nothing
    , appenderFilters       = M.empty
    , appenderErrorHandler  = Nothing
    } 

data ErrorHandler = ErrorHandler
    { errorHandlerClass     :: ClassConfig
    , errorHandlerRoot      :: Bool
    , errorHandlerLogger    :: Maybe String
    , errorHandlerAppender  :: Maybe String
    } deriving (Eq, Show)

newErrorHandler cls = ErrorHandler
    { errorHandlerClass     = newClassConfig cls
    , errorHandlerRoot      = False
    , errorHandlerLogger    = Nothing
    , errorHandlerAppender  = Nothing
    }

-- TODO: implement support for logger additivity (is additivity=false allowed for root logger?  I think not...)
data LoggerConfiguration levelType = LoggerConfiguration
    { loggerLevel       :: levelType
    , loggerAppenders   :: S.Set String
    } deriving (Eq, Show)

newLoggerConfiguration level = LoggerConfiguration
    { loggerLevel       = level
    , loggerAppenders   = S.empty
    }

toProperties :: LoggingConfiguration -> Properties
toProperties LoggingConfiguration{..} = foldl (.) id
    [ setProperty ("log4j." ++ key) val
    | (key, val) <- [ ("reset", "true") | reset ]
                 ++ concatMap appenderToProperties (M.toList appenders)
                 ++ concatMap (loggerToProperties basicLevelToPropString "rootLogger") (maybeToList rootLogger)
                 ++ concatMap (uncurry (loggerToProperties levelToPropString . ("logger." ++)))  (M.toList loggers)
                 ++ [ ("renderer." ++ key, val) | (key, val) <- M.toList objectRenderers]
                 ++ [ ("throwableRenderer", cls) | cls <- maybeToList throwableRenderer]
    ] newProperties

appenderToProperties (("appender." ++) -> prefix, AppenderConfiguration{..}) =
    classConfigToProperties prefix appenderClass
    ++ maybe [] (classConfigToProperties (prefix ++ ".layout")) appenderLayout
    ++ concat [ classConfigToProperties (prefix ++ ".filter." ++ filterId) filterClass 
              | (filterId, filterClass) <- M.toList appenderFilters]
    ++ maybe [] (errorHandlerToProperties (prefix ++ ".errorhandler")) appenderErrorHandler

errorHandlerToProperties prefix ErrorHandler{..}
    = classConfigToProperties prefix errorHandlerClass
    ++ [ (prefix ++ ".root-ref", "true")        | errorHandlerRoot ]
    ++ [ (prefix ++ ".logger-ref", logger)      | Just logger   <- [errorHandlerLogger]]
    ++ [ (prefix ++ ".appender-ref", appender)  | Just appender <- [errorHandlerAppender]]

classConfigToProperties prefix ClassConfig{..}
    = (prefix, className)
    : [ (prefix ++ '.' : k, v) | (k,v) <- classOptions]

loggerToProperties showLevel name LoggerConfiguration{..} = [(name, propString)]
    where
        propString = intercalate ", " 
            ( showLevel loggerLevel
            : S.toList loggerAppenders
            )

levelToPropString (Basic basicLevel) = basicLevelToPropString basicLevel
levelToPropString other              = show other

basicLevelToPropString (Other raw) = raw
basicLevelToPropString basicLevel  = show basicLevel

fromProperties :: Properties -> LoggingConfiguration
fromProperties props = undefined


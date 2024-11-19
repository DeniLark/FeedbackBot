module Main where

import Config (Config (..), getConfig)
import Logging
import Server (runServer)
import TelegramClientEnv

main :: IO ()
main = do
    runLoggerEnv $ \loggerEnv -> do
        logInfo loggerEnv "Logging works"
        ec <- getConfig
        case ec of
            Left e -> logCrit loggerEnv $ showLS e
            Right config -> do
                let port = configPort config
                telegramEnv <- initClientEnv
                case telegramEnv of
                    Nothing -> logCrit loggerEnv "Token not available"
                    Just env -> runServer port env -- сюда бы добавить логирование но не хочется добавлять еще один аргумент

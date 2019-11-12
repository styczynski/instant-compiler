{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want ["build.lock", ("insc_jvm" <.> exe), ("insc_llvm" <.> exe)]

        "test.lock" %> \out -> do
            alwaysRerun
            success <- executeStackBuild ["test", "--no-terminal", "--test-arguments=--jobs=1"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "touch" [out] "."
            return ()

        "build.lock" %> \out -> do
            alwaysRerun
            success <- executeStackBuild ["build"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            need ["test.lock"]
            executeCommand "touch" [out] "."
            return ()

        [("insc_llvm" <.> exe)] &%> \[ouths] -> do
            need ["build.lock"]
            executeCommand "cp" ([(stackDir </> "bin" </> "inscllvm"), ("insc_llvm" <.> exe)]) "."
            message "Done!"

        [("insc_jvm" <.> exe)] &%> \[ouths] -> do
            need ["build.lock"]
            executeCommand "cp" ([(stackDir </> "bin" </> "inscjvm"), ("insc_jvm" <.> exe)]) "."
            message "Done!"
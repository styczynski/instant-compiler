{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want ["build.lock", ("insc_jvm" <.> exe), ("insc_llvm" <.> exe)]

        "build.lock" %> \out -> do
            alwaysRerun
            executeCommandStack ["upgrade", "--binary-version", "2.1.1"] "."
            success <- executeStackBuild ["build"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
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
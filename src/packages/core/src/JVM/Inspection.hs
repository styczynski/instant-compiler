module JVM.Inspection where

import JVM.Jasmine

getStackSizeForPush :: Int -> JConstant -> Int
getStackSizeForPush oldSize v = oldSize + jasminePushSize v

getStackSizeForInstruction :: Int -> JInstruction -> Int
getStackSizeForInstruction oldSize (Push v) = getStackSizeForPush oldSize v
getStackSizeForInstruction oldSize (StoreInt _) = oldSize - 1
getStackSizeForInstruction oldSize (LoadInt _) = oldSize + 1
getStackSizeForInstruction oldSize _ = oldSize

getStackSize :: [JInstruction] -> Int
getStackSize = foldl getStackSizeForInstruction 0

getLocalsSizeForInstruction :: JInstruction -> Int
getLocalsSizeForInstruction (LoadInt index) = index+1
getLocalsSizeForInstruction (StoreInt index) = index+1
getLocalsSizeForInstruction _ = 1

getLocalsSize :: [JInstruction] -> Int
getLocalsSize ins = maximum $ map getLocalsSizeForInstruction ins
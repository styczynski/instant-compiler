module JVM.Inspection where

import JVM.Jasmine

-- Determine new stack size after a push instruction
getStackSizeForPush :: Int -> JConstant -> Int
getStackSizeForPush oldSize v = oldSize + jasminePushSize v

-- Determine new stack size after a given instruction will be executed
getStackSizeForInstruction :: Int -> JInstruction -> Int
getStackSizeForInstruction oldSize (Push v) = getStackSizeForPush oldSize v
getStackSizeForInstruction oldSize (StoreInt _) = oldSize - 1
getStackSizeForInstruction oldSize (LoadInt _) = oldSize + 1
getStackSizeForInstruction oldSize (ConstInt _) = oldSize + 1
getStackSizeForInstruction oldSize Pop = oldSize - 1
getStackSizeForInstruction oldSize _ = oldSize

-- Get max stack size for the list of instructions
getStackSize :: [JInstruction] -> Int
getStackSize incs = let (_, v) = foldl (\(cur, max) ins -> let new = getStackSizeForInstruction cur ins in (new, if new > max then new else max)) (0,0) incs in v

-- Get locals count for the given instruction
getLocalsSizeForInstruction :: JInstruction -> Int
getLocalsSizeForInstruction (LoadInt index) = index+1
getLocalsSizeForInstruction (StoreInt index) = index+1
getLocalsSizeForInstruction _ = 1

-- Determine locals count for a given list of instructions
getLocalsSize :: [JInstruction] -> Int
getLocalsSize ins = maximum $ map getLocalsSizeForInstruction ins

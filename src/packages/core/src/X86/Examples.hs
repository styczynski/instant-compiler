-- | Calling conventions. There are basically only two: System V (Linux, OSX, BSD) and Win64\/fastcall

module X86.Examples where

import X86.X86

-- | Example: Fibonacci function in Assembly
fibCode = saveNonVolatile $ do
  l <- label
  r1 <- allocReg
  r2 <- allocReg
  --leaData r1 $ CString "ala ma kota"
  mov r1 arg1
  inc r1
  mov r1 r2


testx86 = show $ withLabels fibCode
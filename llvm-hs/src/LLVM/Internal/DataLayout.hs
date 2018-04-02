module LLVM.Internal.DataLayout where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class

import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI

import LLVM.AST.DataLayout
import LLVM.DataLayout
import LLVM.AST.Type
import LLVM.Context

import LLVM.Internal.Coding
import LLVM.Internal.String ()
import LLVM.Internal.EncodeAST
import LLVM.Internal.Type

withFFIDataLayout :: DataLayout -> (Ptr FFI.DataLayout -> IO a) -> IO a
withFFIDataLayout dl f = flip runAnyContT return $ do
  dls <- encodeM (dataLayoutToString dl)
  liftIO $ bracket (FFI.createDataLayout dls) FFI.disposeDataLayout f


getTypeAllocSize :: DataLayout -> Type -> IO Word64
getTypeAllocSize dl t = withContext $ \ctx -> runEncodeAST ctx $ do
  ty <- encodeM t
  liftIO $ withFFIDataLayout dl (\dl -> FFI.getTypeAllocSize dl ty)
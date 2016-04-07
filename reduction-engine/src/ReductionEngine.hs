{-# LANGUAGE ForeignFunctionInterface #-}
module ReductionEngine (makeNumber, makeApp, printCell) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.IO

newtype CellPtr = CellPtr (Ptr ())

foreign import ccall "make_number" make_number :: CInt -> IO (Ptr ())
makeNumber :: Int -> IO CellPtr
makeNumber = fmap CellPtr . make_number . fromIntegral

foreign import ccall "make_app" make_app :: Ptr () -> Ptr () -> IO (Ptr ())
makeApp :: CellPtr -> CellPtr -> IO CellPtr
makeApp (CellPtr c1) (CellPtr c2) = fmap CellPtr (make_app c1 c2)

foreign import ccall "print_cell" print_cell :: Ptr () -> IO ()
printCell :: CellPtr -> IO ()
printCell (CellPtr c) = print_cell c













{-# LANGUAGE ForeignFunctionInterface #-}
module ReductionEngine ( CellPtr
                       , makeVar
                       , makeNumber
                       , makeApp
                       , makeAbstr
                       , makeBuiltin
                       , makeConstructor
                       , printCell
                       , reduce
                       , reducePrintList
                       ) where

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import System.IO

newtype CellPtr = CellPtr (Ptr ())

foreign import ccall unsafe "make_empty_cell" _make_empty_cell :: IO (Ptr ())
foreign import ccall unsafe "set_cell_var" _set_cell_var :: Ptr () -> CString -> IO ()
foreign import ccall unsafe "set_cell_app" _set_cell_app :: Ptr () -> Ptr () -> Ptr () -> IO ()
foreign import ccall unsafe "set_cell_abstr" _set_cell_abstr :: Ptr () -> CString -> Ptr () -> IO ()
foreign import ccall unsafe "set_cell_number" _set_cell_number :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "set_cell_builtin" _set_cell_builtin :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "set_cell_empty_data" _set_cell_empty_data :: Ptr () -> CInt -> CSize -> IO ()
foreign import ccall unsafe "set_cell_constructor" _set_cell_constructor :: Ptr () -> CInt -> CInt -> IO ()

foreign import ccall unsafe "select_data_field" _select_data_field :: Ptr () -> CSize -> IO (Ptr ())
foreign import ccall unsafe "print_cell" _print_cell :: Ptr () -> IO ()
foreign import ccall unsafe "reduce" _reduce :: Ptr () -> IO (Ptr ())
foreign import ccall unsafe "reduce_print_list" _reduce_print_list :: Ptr () -> IO ()

makeCell :: (Ptr () -> IO ()) -> IO CellPtr
makeCell set = do
  cell <- _make_empty_cell
  set cell
  return (CellPtr cell)

makeVar :: String -> IO CellPtr
makeVar sym = do
  csym <- newCString sym
  makeCell $ \cell -> _set_cell_var cell csym

makeNumber :: Int -> IO CellPtr
makeNumber n = makeCell set
    where set cell = _set_cell_number cell (fromIntegral n)

makeApp :: CellPtr -> CellPtr -> IO CellPtr
makeApp (CellPtr c1) (CellPtr c2) = makeCell set
    where set cell = _set_cell_app cell c1 c2

makeAbstr :: String -> CellPtr -> IO CellPtr
makeAbstr sym (CellPtr body) = do
  csym <- newCString sym
  makeCell $ \cell -> _set_cell_abstr cell csym body

makeBuiltin :: Enum b => b -> IO CellPtr
makeBuiltin e = makeCell set
    where set cell = _set_cell_builtin cell (fromIntegral $ fromEnum e)

makeConstructor :: Int -> Int -> IO CellPtr
makeConstructor tag nargs = makeCell set
    where set cell = _set_cell_constructor cell (fromIntegral tag) (fromIntegral nargs)

printCell :: CellPtr -> IO ()
printCell (CellPtr c) = _print_cell c

reduce :: CellPtr -> IO CellPtr
reduce (CellPtr c) = fmap CellPtr (_reduce c)

reducePrintList :: CellPtr -> IO ()
reducePrintList (CellPtr c) = _reduce_print_list c

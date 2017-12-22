module Main where

import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Monad
import Control.Concurrent
import System.Timeout

import Day18Parser

type Address = Int
type Program = V.Vector Instruction
type VmState = (Address, M.Map Char Int)

main :: IO ()
main = do
  day18Input <- readFile "day18.input"
  let instructions = loadProgram day18Input
  either (error "parse error") (runVms. V.fromList) instructions


runVms :: Program -> IO ()
runVms code = do
  chan0to1   <- newChan
  chan1to0   <- newChan
  chanResult <- newChan

  let sym0 = M.insert 'p' 0 M.empty
      sym1 = M.insert 'p' 1 M.empty
      vm0 = runProgram code sym0 chan1to0 chan0to1 (writeChan chanResult)
      vm1 = runProgram code sym1 chan0to1 chan1to0 (return . const ())
  forkIO vm0
  forkIO vm1
  void $ timeout 1000000 (recordRecv chanResult)

recordRecv :: Chan Int -> IO ()
recordRecv chan = forM_ [1..] $ \idx-> do
  x <- readChan chan
  print $ "report #" ++ show idx ++ " recv: " ++ show x

runProgram :: Program
  -> M.Map Char Int         -- symbol table
  -> Chan Int               -- input channel
  -> Chan Int               -- output channel
  -> (Int -> IO ())         -- callback on rcv instruction
  -> IO ()
runProgram code symbols ich och rcvCb = loop (0, symbols)
  where
    loop (addr, sym) = do
      case code ! addr of
        Snd reg -> do
                    void $ send reg sym och
                    loop (addr + 1, sym)
        Rcv reg -> do
                    (x, nSym) <- recv reg sym ich
                    rcvCb x
                    loop (addr + 1, nSym)
        asm     -> do
                    loop $ evalPure (addr, sym) asm

evalPure :: VmState -> Instruction -> VmState
evalPure (addr, m) (Add lop rop) = (addr + 1, insert lop rop (+) m)
evalPure (addr, m) (Mul lop rop) = (addr + 1, insert lop rop (*) m)
evalPure (addr, m) (Mod lop rop) = (addr + 1, insert lop rop mod m)
evalPure (addr, m) (Set lop rop) = (addr + 1, insert lop rop (flip const) m)
evalPure (addr, m) (Jgz lop rop)
  | evalR lop m > 0 = (addr + (evalR rop m), m)
  | otherwise       = (addr + 1, m)

insert :: Operand
  -> Operand
  -> (Int -> Int -> Int)
  -> M.Map Char Int
  -> M.Map Char Int
insert lop@(C c) rop f m = M.insert c (f (evalR lop m) (evalR rop m)) m
insert     (I _) _   _ m = m -- undefined behavior. defaulting to no-op

evalR :: Operand -> M.Map Char Int -> Int
evalR (C c) m = fromMaybe 0 (M.lookup c m)
evalR (I i) _ = i

send :: Operand
  -> M.Map Char Int
  -> Chan Int
  -> IO ()
send (C c) m chan = do
  writeChan chan (fromMaybe 0 $ M.lookup c m)

recv :: Operand
  -> M.Map Char Int
  -> Chan Int
  -> IO (Int, M.Map Char Int)
recv (C c) m chan = do
  x <- readChan chan
  return $ (x, M.insert c x m)
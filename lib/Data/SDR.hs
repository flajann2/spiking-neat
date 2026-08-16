module Data.SDR where

import qualified Data.IntSet as IS

-- construction
IS.fromList [3, 17, 402, 1998]       :: IntSet
IS.empty                             :: IntSet
IS.singleton 5                       :: IntSet

-- the operations you'd actually use constantly for SDR work
IS.intersection sdrA sdrB            -- overlap, as a set
IS.size (IS.intersection sdrA sdrB)  -- overlap score, an Int
IS.union sdrA sdrB
IS.difference sdrA sdrB
IS.member 402 sdrA                   -- is bit 402 active?
IS.toList sdrA                       -- back to [Int] for iterating active bits

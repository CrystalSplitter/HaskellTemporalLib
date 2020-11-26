module QCStn where

import HaskellTemporalLib.Stn (STNMap, fromList, constrain)

test :: STNMap Char Double
test
  = fromList 'z'
    $  constrain 'z' 'a' 10.0    10.0
    ++ constrain 'a' 'b' 200.0   200.0
    ++ constrain 'b' 'c' 300.0   3000.0
    ++ constrain 'c' 'd' 40000.0 40000.0

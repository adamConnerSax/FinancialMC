import Criterion.Main

import BenchMoneyOps
import BenchPath
import BenchMapLike
import BenchEngine

benchesIO::IO [Benchmark]
benchesIO = sequence [benchMoneyOpsIO,benchMapLikeIO,benchPathsIO]

main = do
  benches <- benchesIO
  defaultMain benches


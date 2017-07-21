import           Criterion.Main

import           BenchEngine
import           BenchMapLike
import           BenchMoneyOps
import           BenchPath

benchesIO::IO [Benchmark]
benchesIO = sequence [benchMoneyOpsIO, benchMapLikeIO, benchEngineIO, benchPathsIO]

main = do
  benches <- benchesIO
  defaultMain benches


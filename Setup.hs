import Distribution.PackageDescription (PackageDescription(extraSrcFiles), HookedBuildInfo)
import Distribution.Simple (Args, UserHooks(buildHook, preBuild))
import qualified Distribution.Simple as DS
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import qualified System.Process as Proc
import qualified System.Directory as Dir
import Control.Monad (when, void)

preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
preBuild' args buildFlags = do
    exists <- Dir.doesDirectoryExist "./src/BNFC"
    when (not exists) $ void $ Proc.system "cd syntax && ./generate-parser"
    (preBuild DS.simpleUserHooks) args buildFlags

main :: IO ()
main = DS.defaultMainWithHooks DS.simpleUserHooks{preBuild = preBuild'}

module Main where

import Control.Monad.IO.Class
import GHC.Paths
import GHC.IO.Handle.FD
import GHC
import HscTypes
import SimplStg
import SimplCore
import CorePrep
import CoreToStg
import DynFlags
import TyCon
import Pretty
import Outputable
import System.IO
import StgSyn
import Name
import qualified Data.ByteString.Char8 as C

import CodeGen

main :: IO ()
main = do
    print ("libdir: " ++ libdir)
    print ("ghc_pkg: " ++ ghc_pkg)
    print ("docdir: " ++ docdir)
    print ("ghc: " ++ ghc)
    runGhc (Just libdir) $ do
        env <- getSession
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscAsm {-HscInterpreted, HscC-}
            -- , ghcLink = LinkInMemory
        }

        target <- guessTarget "src/Example.hs" Nothing
        targetLib <- guessTarget "src/Lib.hs" Nothing
        setTargets [target, targetLib]
        load LoadAllTargets
        depanal [] True
        modSum <- getModSummary $ mkModuleName "Example"

        pmod <- parseModule modSum      -- ModuleSummary
        tmod <- typecheckModule pmod    -- TypecheckedSource
        dmod <- desugarModule tmod      -- DesugaredModule
        let coreMod = coreModule dmod      -- CoreModule
        let mod   = ms_mod modSum
        let loc   = ms_location modSum
        let core  = mg_binds coreMod
        let tcs   = filter isDataTyCon (mg_tcs coreMod) -- see note in source code:
        -- cg_tycons includes newtypes, for the benefit of External Core,
        -- but we don't generate any code for newtypes

        let dflags' = foldl gopt_set dflags [Opt_StgCSE,
                Opt_DoEtaReduction,
                Opt_CallArity,
                Opt_FunToThunk,
                Opt_StgStats
                ]
        let env' = env {hsc_dflags = dflags'}
        -- run core2core passes
        guts' <- liftIO $ core2core env' coreMod
        let core' = mg_binds guts'
        (prep, _) <- liftIO $ corePrepPgm env' mod loc core' tcs
        let (stg,_) = coreToStg dflags' mod prep
        stg_binds2 <- liftIO $ stg2stg dflags' stg
        liftIO $ do
            logFile <- openFile "/tmp/example.hs.stg.txt" WriteMode
            mapM_ print (map fooCompile stg_binds2)
            printSDocLn PageMode dflags' logFile (defaultUserStyle dflags') $ ppr stg_binds2
            hClose logFile

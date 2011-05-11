-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) The University of Glasgow 2004-2008, John Chee 2011
-- License     :  BSD-style (see the file LICENSE)
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

module SystemExtras (readCommandWithExitCode) where

import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..))
import qualified System.Process as P
import qualified Control.Concurrent as Conc
import qualified Control.Exception as C
import qualified Control.Monad as M
import System.IO as IO

{- |
readCommandWithExitCode creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.
-}

readCommandWithExitCode
    :: String                   -- ^ command to run
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readCommandWithExitCode cmd input = do
    (Just inh, Just outh, Just errh, pid) <-
        P.createProcess (P.shell cmd){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- Conc.newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- Conc.forkIO $ C.evaluate (length out) >> Conc.putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    _ <- Conc.forkIO $ C.evaluate (length err) >> Conc.putMVar outMVar ()

    -- now write and flush any input
    M.when (not (null input)) $ do hPutStr inh input; hFlush inh
    IO.hClose inh -- done with stdin

    -- wait on the output
    Conc.takeMVar outMVar
    Conc.takeMVar outMVar
    IO.hClose outh
    IO.hClose errh

    -- wait on the process
    ex <- P.waitForProcess pid

    return (ex, out, err)

-----------------------------------------------------------------------------
-- |
-- Copyright (C) 2011 John Chee
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- dupcommand -- duplicate commands by running them many times in many threads
--
-----------------------------------------------------------------------------

-- TODO allow to talk to processes via stdin?
-- TODO running 0 times = run infinite times

import qualified System.Environment as Env
import Control.Concurrent (Chan)
import qualified Control.Concurrent as C
import qualified Control.Monad as M
import qualified SystemExtras as SE
import System.Exit (ExitCode(..))


-- three arguments: threads, number of times to run (optional), command
main :: IO ()
main = do
  (threads : times : stdin : command) <- Env.getArgs
  chan <- C.newChan
  forker chan (read threads) (read times) stdin (unwords command)
  (mapM_ print . take (read threads * read times)) =<< C.getChanContents chan

forker :: Chan (ExitCode, String, String) -> Int -> Int -> String -> String -> IO ()
forker chan threads times stdin command =
  M.replicateM_ threads (C.forkIO (runN chan times stdin command))

runN :: Chan (ExitCode, String, String) -> Int -> String -> String -> IO ()
runN chan times stdin command
  | times <= 0 = return ()
  | otherwise  = do
    (ex, out, err) <- SE.readCommandWithExitCode command ""
    C.writeChan chan (ex, out, err)
    runN chan (times - 1) stdin command

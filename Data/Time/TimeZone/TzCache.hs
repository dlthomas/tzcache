
module Data.Time.TimeZone.TzCache (cacheTimeZones) where

import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)

import Data.List (isPrefixOf)

import Data.Map (Map)
import qualified Data.Map as M

import System.Posix.Files (getFileStatus, isSymbolicLink, isDirectory, isRegularFile, readSymbolicLink)
import System.Directory (getDirectoryContents)

import Control.Exception (try, SomeException (..))

import Control.Monad.State


-- | Read timezone files below a directory and store the @TimeZoneSeries@
-- in a map, indexed by the relative path below the start point.
--
-- Typical use would be on \/usr\/share\/zoneinfo on most Unix-like systems,
-- where the relative paths wind up being useful timezone names.
--

cacheTimeZones :: FilePath -> IO (Map String TimeZoneSeries)
cacheTimeZones = flip execStateT M.empty . doCacheTimeZones
    where doCacheTimeZones filename = do
            cache <- Control.Monad.State.get
            if M.member filename cache
             then return ()
             else do
                status <- liftIO $ getFileStatus filename
                case () of
                    _ | isSymbolicLink status -> do
                        target <- liftIO $ readSymbolicLink filename
                        cache' <- liftIO $ execStateT (doCacheTimeZones target) cache
                        put $ maybe cache' (\ value -> M.insert filename value cache') $ M.lookup target cache'

                      | isDirectory status -> do
                        files <- liftIO $ getDirectoryContents filename
                        let files' = map (\ f -> filename ++ "/" ++ f) $ filter (not . isPrefixOf ".") files
                        mapM_ doCacheTimeZones files'

                      | isRegularFile status -> do
                        tz <- liftIO $ try $ getTimeZoneSeriesFromOlsonFile filename
                        case tz of
                            Left (SomeException _) -> return ()
                            Right tz' -> put $ M.insert filename tz' cache

                      | otherwise -> return ()
   

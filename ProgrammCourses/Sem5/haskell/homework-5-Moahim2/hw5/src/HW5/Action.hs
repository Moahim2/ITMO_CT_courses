{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

import HW5.Base (HiAction (..), HiMonad (runAction), HiValue (..))

import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Set (Set, member)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import GHC.Exts (fromList)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (newStdGen, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

-- | Special type for IO read-write activities with check permissions.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a     -- ^ check permission and do activity
                    } deriving (Functor, Applicative, Monad) -- ^ deriving 'Monad' via ReaderT
                        via ReaderT (Set HiPermission) IO

-- |Create context, in which doing activity if we have permission else throw PermissionException.
doWithPermission :: HiPermission -> IO a -> HIO a
doWithPermission permission activity = do
  set <- HIO return
  HIO . return $ if member permission set
    then activity
    else throwIO $ PermissionRequired permission

-- |doWithPermission, but always return HiValueNull.
doWithNullResult :: HiPermission -> IO a -> HIO HiValue
doWithNullResult permission action = doWithPermission permission $ action >> return HiValueNull

doWithoutPermission :: IO a -> HIO a
doWithoutPermission activity = HIO $ const activity

packStrToHVStr :: String -> HiValue
packStrToHVStr = HiValueString . T.pack

instance HiMonad HIO where
  -- runAction on read return bytes or string if path - file, else if it's dir return [names] else return null
  runAction (HiActionRead path) = doWithPermission AllowRead $ do
    isFile <- doesFileExist path
    if isFile
      then do
        bytes <- B.readFile path
        return $ case decodeUtf8' bytes of
          Left _     -> HiValueBytes bytes
          Right text -> HiValueString text
      else do
        isDir <- doesDirectoryExist path
        if isDir
          then do
            dirs <- listDirectory path
            return . HiValueList . fromList $ packStrToHVStr <$> dirs
          else return HiValueNull
  runAction (HiActionWrite path bytes) = doWithNullResult AllowWrite $ B.writeFile path bytes
  runAction (HiActionMkDir path)       = doWithNullResult AllowWrite $ createDirectory path
  runAction (HiActionChDir path)       = doWithNullResult AllowRead $ setCurrentDirectory path
  runAction HiActionCwd                = doWithPermission AllowRead $ packStrToHVStr <$> getCurrentDirectory
  runAction HiActionNow                = doWithPermission AllowTime $ HiValueTime <$> getCurrentTime
  runAction (HiActionRand a b)         = doWithoutPermission $
    HiValueNumber . toRational . fst . uniformR (a, b) <$> newStdGen
  runAction (HiActionEcho text)        = doWithNullResult AllowWrite $ print text

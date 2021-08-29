{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Brick                (AttrName)
import           Brick.BChan          (BChan)
import qualified Brick.Focus          as F
import           Brick.Types          (Widget)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Control.Concurrent   (ThreadId)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           GHC.IO.Exception     (ExitCode)
import           Graphics.Vty         (Attr, Style)
import           Lens.Micro.TH        (makeLenses)


-- | An external representation of the theme, replacing @fg@, @bg@, @on@ and @withStyle@
-- functions with constructors which can simply be @read@ in
type ThemeAux = [(AttrName, AttrAux)]

-- | Actual representation of the theme, using Brick types
type Theme = [(AttrName, Attr)]

-- | A 'dumb' representation of the @fg@, @bg@, @on@, and @withStyle@ functions
data AttrAux = Fg ColorAux
             | Bg ColorAux
             | On ColorAux ColorAux
             | WithStyle AttrAux Style
             deriving (Show, Read)

-- | An external representation of either an ISO color code or an RGB color
-- Needs to be converted into a Vty Color
-- This is because the Vty Color240 is extremely weird
data ColorAux = ISO Word8 | RGB Word8 Word8 Word8
  deriving (Show, Read)

data Timeout = Seconds Int | DoNotClear
  deriving (Show, Read)

data Config = Config { timeout     :: Maybe Timeout
                     , dbPath      :: Maybe Text
                     , keyfilePath :: Maybe Text
                     } deriving (Show, Read)


data CmdAction = Ls | Clip | Show

data View = PasswordView
          | BrowserView
          | SearchView
          | EntryDetailsView
          | ExitView
          deriving (Eq)

data Field = PathField
           | PasswordField
           | KeyfileField
           | BrowserField
           | SearchField
           deriving (Ord, Eq, Show)

data CopyType = CopyUsername | CopyPassword

data ExitDialog = Clear | Exit | Cancel

-- | (exitcode, stdout, stderr)
type CmdOutput = (ExitCode, Text, Text)

data Event = Login CmdOutput
           | EnterDir Text CmdOutput   -- ^ Text is the currently selected entry
           | ShowEntry Text CmdOutput  -- ^ Text is the currently selected entry
           | ClearClipCount Int
           | Copying (ExitCode, Text)  -- ^ Excludes stdout

data State = State
  { -- | The name of visible entries in the current directory
    _visibleEntries         :: L.List Field Text,
    -- | All the entries (visible or not) that has been loaded from all directories
    -- Mapping between directory name to list of entry names
    _allEntryNames          :: M.Map Text [Text],
    -- | The name of the entry selected to show details for
    _selectedEntryName      :: Maybe Text,
    -- | All the entry details that has been opened
    -- Mapping between directory name to (entry names and their details)
    _allEntryDetails        :: M.Map Text (M.Map Text Text),
    -- | The currently visible View
    _activeView             :: View,
    -- | The previous View
    _previousView           :: View,
    -- | The widget in the bottom of the window
    _footer                 :: Widget Field,
    -- | Determines the fields that can be focused and their order
    _focusRing              :: F.FocusRing Field,
    -- | Field for the database path
    _dbPathField            :: E.Editor Text Field,
    -- | Field for the database password
    _passwordField          :: E.Editor Text Field,
    -- | Field for the keyfile path
    _keyfileField           :: E.Editor Text Field,
    -- | Field for the Text in the search bar
    _searchField            :: E.Editor Text Field,
    -- | List of directory names that make up the path of the current directory
    _currentPath            :: [Text],
    -- | The state container for the exit dialog
    _exitDialog             :: D.Dialog ExitDialog,
    -- | Whether the clipboard contains a copied value from kpxhs
    _hasCopied              :: Bool,
    -- | The app event channel; contains all the info that needs to be passed from
    -- a background thread to the AppEvent handler
    _chan                   :: BChan Event,
    -- | Number of seconds to wait before clearing the clipboard
    -- If Nothing, then the clipboard won't be automatically cleared
    _clearTimeout           :: Maybe Int,
    -- | The current clipboard clear countdown thread id
    _countdownThreadId      :: Maybe ThreadId,
    -- | The current value of the counter
    _currentCountdown       :: Maybe Float
  }

makeLenses ''State

{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Brick.AttrMap        (AttrMap)
import           Brick.BChan          (BChan)
import qualified Brick.Focus          as F
import           Brick.Types          (Widget)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Control.Concurrent   (ThreadId)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import           GHC.IO.Exception     (ExitCode)
import           Lens.Micro.TH        (makeLenses)


data CmdAction = Ls | Clip | Show

data View = LoginView
          | LoginFrozenView
          | BrowserView
          | SearchView
          | EntryDetailsView
          | ExitDialogView
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
    _visibleEntries     :: L.List Field Text,
    -- | All the entries (visible or not) that has been loaded from all directories
    -- Mapping between directory name to list of entry names
    _allEntryNames      :: M.Map Text [Text],
    -- | The name of the entry selected to show details for
    _selectedEntryName  :: Maybe Text,
    -- | All the entry details that has been opened
    -- Mapping between directory name to (entry names and their details)
    _allEntryDetails    :: M.Map Text (M.Map Text Text),
    -- | The currently visible View
    _activeView         :: View,
    -- | The previous View
    _previousView       :: View,
    -- | The widget in the bottom of the window
    _footer             :: Widget Field,
    -- | Determines the fields that can be focused and their order
    _focusRing          :: F.FocusRing Field,
    -- | Field for the database path
    _dbPathField        :: E.Editor Text Field,
    -- | Field for the database password
    _passwordField      :: E.Editor Text Field,
    -- | Field for the keyfile path
    _keyfileField       :: E.Editor Text Field,
    -- | Field for the Text in the search bar
    _searchField        :: E.Editor Text Field,
    -- | List of directory names that make up the path of the current directory
    _currentPath        :: [Text],
    -- | The exit dialog
    _exitDialog         :: D.Dialog ExitDialog,
    -- | Whether the clipboard contains a copied value from kpxhs
    _isClipboardCleared :: Bool,
    -- | The app event channel; contains all the info that needs to be passed from
    -- a background thread to the AppEvent handler
    _chan               :: BChan Event,
    -- | Number of seconds to wait before clearing the clipboard
    -- If Nothing, then the clipboard won't be automatically cleared
    _clearTimeout       :: Maybe Int,
    -- | The current clipboard clear countdown thread id
    _countdownThreadId  :: Maybe ThreadId,
    -- | The current value of the counter
    _counterValue       :: Maybe Float,
    -- | The current pending vim-like command (how many lines to jump up/down)
    -- A String is used instead of Text because it is a [Char] where
    -- every Char is the digit that the user pressed
    -- There is no need to store the direction/motion command, because
    -- as soon as it is pressed, the list can be scrolled and this setting
    -- cleared
    _currentCmd         :: String,
    -- | The app's attribute map
    _theMap             :: AttrMap,
    -- | The first field to focus when first logging in
    _fieldToFocus       :: Field
  }

makeLenses ''State

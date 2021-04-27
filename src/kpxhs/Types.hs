{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Map.Strict as Map
import Lens.Micro.TH (makeLenses)
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

data Action = Ls | Clip | Show

data View = PasswordView | BrowserView | SearchView | EntryView

data Field = PathField | PasswordField | KeyfileField | BrowserField | SearchField
  deriving (Ord, Eq, Show)

data CopyType = CopyUsername | CopyPassword

data State = State
  { -- | The name of visible entries in the current directory
    _visibleEntries :: L.List Field String,
    -- | All the entries (visible or not) that has been loaded from all directories
    -- Mapping between directory name to list of entry names
    _allEntryNames :: Map.Map String [String],
    -- | The name of the entry selected to show details for
    _currentEntryDetailName :: Maybe String,
    -- | All the entry details that has been opened
    -- Mapping between directory name to (entry names and their details)
    _allEntryDetails :: Map.Map String (Map.Map String String),
    _activeView :: View,
    _footer :: String,
    -- | Determines fields that can be focused and their order
    _focusRing :: F.FocusRing Field,
    -- | Field for the database path
    _dbPathField :: E.Editor String Field,
    -- | Field for the database password
    _passwordField :: E.Editor String Field,
    -- | Field for the keyfile path
    _keyfileField :: E.Editor String Field,
    -- | Field for the string in the search bar
    _searchField :: E.Editor String Field,
    -- | List of directory names that make up the path of the current directory
    _currentDir :: [String]
  }

makeLenses ''State

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Menu where

import Brick (bg, fg)
import Brick.AttrMap (attrMap, attrName)
import Brick.Main
import Brick.Types
import Brick.Util (on)
import Brick.Widgets.Core
import Brick.Widgets.Edit (Editor, handleEditorEvent)
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Graphics.Vty (Color (..), defAttr)
import Graphics.Vty.Attributes (black, white)
import Graphics.Vty.Input.Events
import Lens.Micro (set)
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)
import System.Exit (die)

type ResourceName = String

data PathOption = PathOption
    { name :: String
    , path :: String
    }
    deriving (Show, Read, Eq)

data MenuState = MenuState
    { _filePaths :: NonEmptyCursor PathOption
    , _selection :: Maybe FilePath
    }
    deriving (Show, Eq)

makeLenses ''MenuState

drawMenu :: MenuState -> [Widget ResourceName]
drawMenu s =
    let nec = view filePaths s
     in [ vBox $
            concat
                [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
                , [drawPath True $ nonEmptyCursorCurrent nec]
                , map (drawPath False) $ nonEmptyCursorNext nec
                ]
        ]

drawPath :: Bool -> PathOption -> Widget n
drawPath False (PathOption nm pth) = str nm
drawPath True (PathOption nm pth) =
    let nameAttr = attrName "selectedName"
        pathAttr = attrName "selectedPath"
        padding = replicate 8 ' '
        nameW = withAttr nameAttr $ str nm
        pathW = withAttr pathAttr $ str (padding ++ pth)
     in nameW <+> pathW

handleTuiEvent :: BrickEvent n e -> EventM n MenuState ()
handleTuiEvent e = do
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt
                EvKey KEsc [] -> halt
                EvKey KUp [] -> scrollUp
                EvKey (KChar 'k') [] -> scrollUp
                EvKey KDown [] -> scrollDown
                EvKey (KChar 'j') [] -> scrollDown
                EvKey KEnter [] -> returnSelection
                _other -> continueWithoutRedraw
        _other -> continueWithoutRedraw

scrollDown :: EventM n MenuState ()
scrollDown = do
    nec <- use filePaths
    case nonEmptyCursorSelectNext nec of
        Nothing -> continueWithoutRedraw
        Just nec' -> do
            filePaths .= nec'
            return ()

scrollUp :: EventM n MenuState ()
scrollUp = do
    nec <- use filePaths
    case nonEmptyCursorSelectPrev nec of
        Nothing -> continueWithoutRedraw
        Just nec' -> do
            filePaths .= nec'
            return ()

returnSelection :: EventM n MenuState ()
returnSelection = do
    cursor <- use filePaths

    let p = path $ nonEmptyCursorCurrent cursor
    selection .= Just p
    halt

gray :: Color
gray = RGBColor 100 100 100

app :: App MenuState Event ResourceName
app =
    App
        { appDraw = drawMenu
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = return ()
        , appAttrMap =
            const $
                attrMap
                    defAttr
                    [ (attrName "selectedName", black `on` white)
                    , (attrName "selectedPath", gray `on` white)
                    ]
        }

buildInitialState :: [PathOption] -> IO MenuState
buildInitialState opts =
    let contents = NE.nonEmpty opts
     in case contents of
            Nothing -> die "There is no content"
            Just ne ->
                return $
                    MenuState
                        { _filePaths = makeNonEmptyCursor ne
                        , _selection = Nothing
                        }

getMenuSelection :: [PathOption] -> IO (Maybe FilePath)
getMenuSelection opts = do
    initialState <- buildInitialState opts
    endState <- defaultMain app initialState

    return $ view selection endState

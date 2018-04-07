module Graphics.UI.GIGtkStrut where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Int
import           Data.Text
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.UI.EWMHStrut

data StrutPosition = TopPos | BottomPos | LeftPos | RightPos
data StrutAlignment = Beginning | Center | End
data StrutSize = ExactSize Int32 | Percentage Int32

data StrutConfig = StrutConfig
  { strutWidth :: StrutSize
  , strutHeight :: StrutSize
  , strutXPadding :: Int32
  , strutYPadding :: Int32
  , strutMonitor :: Int32
  , strutPosition :: StrutPosition
  , strutAlignment :: StrutAlignment
  , strutDisplayName :: Maybe Text
  }

buildWindow :: MonadIO m => StrutConfig -> m Gtk.Window
buildWindow StrutConfig
              { strutWidth = widthSize
              , strutHeight = heightSize
              , strutXPadding = xpadding
              , strutYPadding = ypadding
              , strutMonitor = monitorNumber
              , strutPosition = position
              , strutAlignment = alignment
              , strutDisplayName = displayName
              } = do
  Just display <- maybe Gdk.displayGetDefault Gdk.displayOpen displayName
  Just monitor <- Gdk.displayGetMonitor display monitorNumber
  screen <- Gdk.displayGetDefaultScreen display

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTypeHint window Gdk.WindowTypeHintDock
  geometry <- Gdk.newZeroGeometry

  monitorGeometry <- Gdk.monitorGetGeometry monitor
  monitorWidth <- Gdk.getRectangleWidth monitorGeometry
  monitorHeight <- Gdk.getRectangleHeight monitorGeometry
  monitorX <- Gdk.getRectangleX monitorGeometry
  monitorY <- Gdk.getRectangleY monitorGeometry

  width <- case widthSize of
             ExactSize w -> return w
             Percentage p -> return $ (p * (monitorWidth - (2 * xpadding))) `div` 100
  height <- case heightSize of
              ExactSize h -> return h
              Percentage p -> return $ (p * (monitorHeight - (2 * ypadding))) `div` 100

  Gdk.setGeometryBaseWidth geometry width
  Gdk.setGeometryBaseHeight geometry height
  Gdk.setGeometryMinWidth geometry width
  Gdk.setGeometryMinHeight geometry height
  Gdk.setGeometryMaxWidth geometry width
  Gdk.setGeometryMaxHeight geometry height
  Gtk.windowSetGeometryHints window (Nothing :: Maybe Gtk.Window)
       (Just geometry) allHints

  let paddedHeight = (height + 2 * ypadding)
      paddedWidth = (width + 2 * xpadding)
      getAlignedPos dimensionPos dpadding monitorSize barSize =
        dimensionPos +
        case alignment of
          Beginning -> dpadding
          Center -> (monitorSize - barSize) `div` 2
          End -> monitorSize - barSize - dpadding
      xAligned = getAlignedPos monitorX xpadding monitorWidth width
      yAligned = getAlignedPos monitorY ypadding monitorHeight height
      (xPos, yPos) =
        case position of
          TopPos -> (xAligned, monitorY + ypadding)
          BottomPos -> (xAligned, monitorY + monitorHeight - paddedHeight)
          LeftPos -> (monitorX + xpadding, yAligned)
          RightPos -> (monitorX + monitorWidth - paddedWidth, yAligned)

  Gtk.windowSetScreen window screen
  Gtk.windowMove window xPos yPos

  let ewmhSettings =
        case position of
          TopPos ->
            zeroStrutSettings
            { _top = paddedHeight
            , _top_start_x = xPos - xpadding
            , _top_end_x = xPos + width + xpadding
            }
          BottomPos ->
            zeroStrutSettings
            { _bottom = paddedHeight
            , _bottom_start_x = xPos - xpadding
            , _bottom_end_x = xPos + width + xpadding
            }
          LeftPos ->
            zeroStrutSettings
            { _left = paddedWidth
            , _left_start_y = yPos - ypadding
            , _left_end_y = yPos + height + ypadding
            }
          RightPos ->
            zeroStrutSettings
            { _right = paddedWidth
            , _right_start_y = yPos - ypadding
            , _right_end_y = yPos + height + ypadding
            }
      setStrutProperties =
        void $ runMaybeT $ do
          gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
          lift $ setStrut gdkWindow ewmhSettings

  Gtk.onWidgetRealize window setStrutProperties

  return window

allHints =
  [ Gdk.WindowHintsMinSize
  , Gdk.WindowHintsMaxSize
  , Gdk.WindowHintsBaseSize
  , Gdk.WindowHintsUserPos
  , Gdk.WindowHintsUserSize
  ]

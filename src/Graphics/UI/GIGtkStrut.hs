module Graphics.UI.GIGtkStrut where

import           Data.Int
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

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
  }

buildWindow StrutConfig
              { strutWidth = widthSize
              , strutHeight = heightSize
              , strutXPadding = xpadding
              , strutYPadding = ypadding
              , strutMonitor = monitorNumber
              , strutPosition = position
              , strutAlignment = alignment
              } = do
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTypeHint window Gdk.WindowTypeHintDock
  geometry <- Gdk.newZeroGeometry

  screen <- Gtk.windowGetScreen window
  display <- Gdk.screenGetDisplay screen

  monitorGeometry <- Gdk.screenGetMonitorGeometry screen monitorNumber
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

  let getAlignedPos dimensionPos dpadding monitorSize barSize =
        dimensionPos +
        case alignment of
          Beginning -> dpadding
          Center -> (monitorSize - barSize) `div` 2
          End -> monitorSize - barSize - dpadding
      xAligned = getAlignedPos monitorX xpadding monitorWidth width
      yAligned = getAlignedPos monitorY ypadding monitorHeight height

  let (xPos, yPos) =
        case position of
          TopPos -> (xAligned, monitorY + ypadding)
          BottomPos -> (xAligned, monitorY + monitorHeight - (height + 2 * ypadding))
          LeftPos -> (monitorX + xpadding, yAligned)
          RightPos -> (monitorX + monitorWidth - (width + 2 * xpadding), yAligned)

  Gtk.windowMove window xPos yPos

  return window

allHints =
  [ Gdk.WindowHintsMinSize
  , Gdk.WindowHintsMaxSize
  , Gdk.WindowHintsBaseSize
  , Gdk.WindowHintsUserPos
  , Gdk.WindowHintsUserSize
  ]

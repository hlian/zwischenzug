-- canvas.hs
module Canvas where
  import Graphics.Rendering.Cairo
  import Data.Time.Clock.POSIX

  frac = snd . properFraction

  modf a b = frac (a / b) * b

  normalizeAngle a | a < 0 = 2*pi + (a `modf` (2*pi))
  normalizeAngle a         = a `modf` (2*pi)

  floorf = fromInteger . fst . properFraction

  angularDistance a b =
    f (na - nb)
    where na = normalizeAngle a
          nb = normalizeAngle b
          f a | a > pi  = a - 2*pi
          f a | a < -pi = a + 2*pi
          f a           = a

  scaleP fx fy (x,y) = (x*fx, y*fy)
  uscaleP f = scaleP f f
  translateP u v (x,y) = (x+u, y+v)
  rotateP a (x,y) = (cos a * x - sin a * y, sin a * x + cos a * y)

  rgba = setSourceRGBA
  rgb = setSourceRGB
  lineWidth = setLineWidth

  color (r,g,b,a) = rgba r g b a
  black = (0.0, 0.0, 0.0, 1.0)
  white = (1.0, 1.0, 1.0, 1.0)

  coordListToPath (x:y:xs) = ((x,y), coordListToPathSegments xs)

  coordListToPathSegments [] = []
  coordListToPathSegments (x1:y1:x2:y2:x3:y3:xs) =
    ((x1,y1), (x2,y2), (x3,y3)) : (coordListToPathSegments xs)

  coordListToPolygon [] = []
  coordListToPolygon (x:y:xs) = (x,y) : (coordListToPolygon xs)

  moveToP = uncurry moveTo
  lineToP = uncurry lineTo
  curveToP ((x1,y1),(x2,y2),(x3,y3)) = curveTo x1 y1 x2 y2 x3 y3
  mapPath f (x,xs) = (f x, map (tripleMap f) xs)
  mapWidthLine f = fupleR (map f)

  fuple f g (a,b) = (f a, g b)
  fupleL f (a,b) = (f a, b)
  fupleR f (a,b) = (a, f b)

  tupleMap f (a,b) = (f a, f b)
  tripleMap f (a,b,c) = (f a, f b, f c)

  doWith g f x = do { f x; g }
  withDo f g x = do { f; g x }

  listDo _ _ [] = return ()
  listDo f g (x:xs) = do { f x; g xs }
  tupleDo f g (x,y) = do { f x; g y }

  fillWith = doWith fill
  strokeWith = doWith stroke
  closePathWith = doWith closePath

  fillPolygon = fillWith drawPolygon

  strokePolygon = strokeWith drawPolygon
  strokeOpenPolygon = strokeWith drawOpenPolygon
  strokeLine = strokeOpenPolygon
  strokeWidthLine = tupleDo lineWidth strokeLine


  drawPolygon = closePathWith drawOpenPolygon
  drawOpenPolygon = withDo newPath drawSubPolygon
  drawSubPolygon = listDo moveToP addToPolygon
  addToPolygon = mapM_ lineToP

  fillPath = fillWith drawPath
  strokePath = strokeWith drawPath
  strokeOpenPath = strokeWith drawOpenPath

  drawPath = closePathWith drawOpenPath
  drawOpenPath = withDo newPath drawSubPath
  drawSubPath = tupleDo moveToP addToPath
  addToPath = mapM_ curveToP

  gon n =
    map nrot [0..n-1]
    where nrot i = let a = 2*pi*i/n in
                   (cos a, sin a)
  octagon = gon 8
  septagon = gon 7
  hexagon = gon 6
  pentagon = gon 5
  square = gon 4
  triangle = gon 3

  exposeHandler widget draw e = do
    drawWin <- widgetGetDrawWindow widget
    (wi,hi) <- widgetGetSize widget
    let (w,h) = (realToFrac wi, realToFrac hi)
    t <- getPOSIXTime
    renderWithDrawable drawWin $ do
      draw w h (realToFrac t)
    return True

  canvas' animate draw w h = do
    initGUI
    window <- windowNew
    da <- drawingAreaNew
    set window [ containerChild := da ]
    windowSetDefaultSize window w h
    onExpose da (exposeHandler da draw)
    if animate
      then timeoutAdd (widgetQueueDraw da >> return True) 16 >> return ()
      else return ()
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

  canvas = canvas' True
  staticCanvas = canvas' False

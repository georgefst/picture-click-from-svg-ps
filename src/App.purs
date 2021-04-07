module App where

import Prelude
import Control.Alternative (guard, (<|>))
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (concat, fromFoldable, intercalate, some, toUnfoldable, uncons)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, foldMap, foldl, length, null, sum)
import Data.Generic.Rep (class Generic)
import Data.Int (round)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString)
import Data.Profunctor.Strong (second)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (Tuple3, Tuple6, tuple3, tuple6)
import Data.Vector.Polymorphic (Vector2(..), getX, getY)
import Svg.Parser (Element, SvgAttribute(..), SvgNode(..), parseToSvgNode)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (option, sepBy, sepBy1)
import Text.Parsing.Parser.String (skipSpaces, string)
import Text.Parsing.Parser.Token (digit)

data Error
  = ParseError { error :: String, pos :: Int }

app ::
  String ->
  { warnings :: Array String, result :: Either Error String }
app inSvg = case parseToSvgNode inSvg of
  Left e -> { warnings: [], result: Left $ ParseError e }
  Right doc ->
    { warnings: warnings <#> \(Warning s x) -> s <> " (" <> x <> ")"
    , result: Right $ intercalate "\n" $ map renderShape entries
    }
    where
    Tuple entries warnings = runWriter $ allShapes doc

allShapes :: SvgNode -> M (Array Shape)
allShapes = case _ of
  SvgElement doc -> do
    v0 <- maybe (warn "No viewbox" unit *> pure (Vector2 0.0 0.0)) (\v -> pure $ Vector2 v.x v.y) $ getViewbox doc
    concat <<< fromFoldable
      <$> traverse (\e -> uncurry makeShape <<< (second $ map (_ - v0)) <<$>> treePaths e) doc.children
  _ -> do
    warn "Top-level node is not an element" unit
    mempty

makeShape :: MetaData -> Array (Vector2 Number) -> Shape
makeShape meta vs =
  { meta
  , shape: round <<$>> vs
  , answerPos: round <$> mean vs
  }

treePaths :: SvgNode -> M (Array (Tuple MetaData (Array (Vector2 Number))))
treePaths x = case x of
  SvgElement element ->
    let
      top = case getAttribute "d" element of
        Nothing -> mempty -- element is not a path
        Just p -> case runParser p $ manyComma command of
          Left e -> warn "Couldn't parse path" e *> mempty
          Right pcs -> case extractPath $ toUnfoldable pcs of
            Left e -> warn "Failed to extract path" e *> mempty
            Right xs -> do
              m <- maybe (pure def) parseMetaData $ getAttribute "id" element
              pure $ pure $ Tuple m $ fromFoldable xs
    in
      (<>)
        <$> top
        <*> foldMap treePaths (fromFoldable element.children)
  SvgText e -> mempty
  SvgComment e -> mempty
  where
  def =  --TODO repetition with `parseMetaData`
    { hint: "hint"
    , answer: "answer"
    , extra: "extra"
    }

-- | Read from a path's 'id' tag.
-- Sticking to the sporcle convention, we separate by tab.
parseMetaData :: String -> M MetaData
parseMetaData s = do
  let
    xs0 = uncons' $ split (Pattern "\t") s

    xs1 = uncons' xs0.tail

    xs2 = uncons' xs1.tail
  unless (null xs2.tail) $ warn "Failed to fully parse metadata (more than one tab character)" s
  pure
    { hint: fromMaybe "hint" xs0.head
    , answer: fromMaybe "answer" xs1.head
    , extra: fromMaybe "extra" xs2.head
    }
  where
  uncons' = maybe { head: Nothing, tail: [] } (\x -> x { head = Just (x.head) }) <<< uncons

-- | Expects a 'MoveTo', several 'LineTo', then an 'EndPath'.
extractPath :: List PathCommand -> Either String (List (Vector2 Number))
extractPath = case _ of
  Cons (MoveTo OriginAbsolute [ v ]) cs -> (Cons v) <$> go cs
  _ -> Left "Illegal start of path"
  where
  go = case _ of
    Cons (LineTo OriginAbsolute [ v ]) cs -> Cons v <$> go cs
    Cons EndPath Nil -> pure Nil
    _ -> Left "Malformed path"

renderShape :: Shape -> String
renderShape e =
  intercalate
    "\t"
    [ e.meta.hint
    , e.meta.answer
    , e.meta.extra
    , intercalate "; " $ map vec e.shape
    , vec e.answerPos
    ]
  where
  vec v = show (getX v) <> "," <> show (getY v)

type Shape
  = { meta :: MetaData
    , shape :: Array (Vector2 Int)
    , answerPos :: Vector2 Int
    }

type MetaData
  = { hint :: String
    , answer :: String
    , extra :: String
    }

{- Our main monad -}
type M a
  = Writer (Array Warning) a

data Warning
  = Warning String String

warn :: forall a. Show a => String -> a -> M Unit
warn s x = tell $ pure $ Warning s $ show x

{- Util -}
--TODO centroid would be preferable - although in practice we'll often manually adjust anyway
-- besides, this point is used for top-left of text box rather than centre
mean :: forall t a. EuclideanRing a => Foldable t => t a -> a
mean xs = sum xs / length xs

mapNested2 :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
mapNested2 = map <<< map

infixl 4 mapNested2 as <<$>>

-- | Generalisation of 'find'.
firstJust :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
firstJust p = foldl go Nothing
  where
  go Nothing x = p x

  go r _ = r

{- Beginnings of a PureScript clone of svg-tree/reanimate-svg -}
type Viewbox
  = { x :: Number, y :: Number, w :: Number, h :: Number }

viewboxParser :: Parser String Viewbox
viewboxParser =
  (\x y w h -> { x, y, w, h })
    <$> iParse
    <*> iParse
    <*> iParse
    <*> iParse
  where
  iParse = num <* skipSpaces

getViewbox :: Element -> Maybe Viewbox
getViewbox doc = do
  case getAttribute "viewBox" doc of
    Just val -> hush $ runParser val viewboxParser
    Nothing -> Nothing

getAttribute :: String -> Element -> Maybe String
getAttribute name0 doc = firstJust (\(SvgAttribute name val) -> guard (name == name0) $> val) doc.attributes

-- | Path command definition.
data PathCommand
  = MoveTo Origin (Array RPoint) -- ^ 'M' or 'm' command
  | LineTo Origin (Array RPoint) -- ^ Line to, 'L' or 'l' Svg path command.
  | HorizontalTo Origin (Array Coord) -- ^ Equivalent to the 'H' or 'h' svg path command.
  | VerticalTo Origin (Array Coord) -- ^ Equivalent to the 'V' or 'v' svg path command.
  | CurveTo Origin (Array (Tuple3 RPoint RPoint RPoint)) -- ^ Cubic bezier, 'C' or 'c' command
  | SmoothCurveTo Origin (Array (Tuple RPoint RPoint)) -- ^ Smooth cubic bezier, equivalent to 'S' or 's' command
  | QuadraticBezier Origin (Array (Tuple RPoint RPoint)) -- ^ Quadratic bezier, 'Q' or 'q' command
  | SmoothQuadraticBezierCurveTo Origin (Array RPoint) -- ^ Quadratic bezier, 'T' or 't' command
  | EllipticalArc Origin (Array (Tuple6 Coord Coord Coord Boolean Boolean RPoint)) -- ^ Elliptical arc, 'A' or 'a' command.
  | EndPath -- ^ Close the path, 'Z' or 'z' svg path command.

derive instance genericPathCommand :: Generic PathCommand _

instance showPathCommand :: Show PathCommand where
  show = genericShow

data Origin
  = OriginAbsolute -- ^ Next point in absolute coordinate
  | OriginRelative -- ^ Next point relative to the previous

derive instance genericOrigin :: Generic Origin _

instance showOrigin :: Show Origin where
  show = genericShow

type RPoint
  = Vector2 Coord

type Coord
  = Number

num :: Parser String Number
num = skipSpaces *> plusMinus <* skipSpaces
  where
  doubleNumber = do
    cs <- fromString <<< fromCharArray <$> some digit
    case cs of
      Just x -> pure x
      Nothing -> fail "not recognised as number"

  plusMinus =
    negate <$ string "-" <*> doubleNumber
      <|> string "+"
      *> doubleNumber
      <|> doubleNumber

flag :: Parser String Boolean
flag = map (_ /= '0') digit

commaWsp :: Parser String Unit
commaWsp = skipSpaces *> option unit (string "," $> unit) <* skipSpaces

point :: Parser String RPoint
point = Vector2 <$> num <* commaWsp <*> num

pointData :: Parser String (List RPoint)
pointData = point `sepBy` commaWsp

pathParser :: Parser String (Array PathCommand)
pathParser = skipSpaces *> some command

command :: Parser String PathCommand
command =
  (MoveTo OriginAbsolute <$ string "M" <*> pointList)
    <|> (MoveTo OriginRelative <$ string "m" <*> pointList)
    <|> (LineTo OriginAbsolute <$ string "L" <*> pointList)
    <|> (LineTo OriginRelative <$ string "l" <*> pointList)
    <|> (HorizontalTo OriginAbsolute <$ string "H" <*> coordList)
    <|> (HorizontalTo OriginRelative <$ string "h" <*> coordList)
    <|> (VerticalTo OriginAbsolute <$ string "V" <*> coordList)
    <|> (VerticalTo OriginRelative <$ string "v" <*> coordList)
    <|> (CurveTo OriginAbsolute <$ string "C" <*> manyComma curveToArgs)
    <|> (CurveTo OriginRelative <$ string "c" <*> manyComma curveToArgs)
    <|> (SmoothCurveTo OriginAbsolute <$ string "S" <*> pointPairList)
    <|> (SmoothCurveTo OriginRelative <$ string "s" <*> pointPairList)
    <|> (QuadraticBezier OriginAbsolute <$ string "Q" <*> pointPairList)
    <|> (QuadraticBezier OriginRelative <$ string "q" <*> pointPairList)
    <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ string "T" <*> pointList)
    <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ string "t" <*> pointList)
    <|> (EllipticalArc OriginAbsolute <$ string "A" <*> manyComma ellipticalArgs)
    <|> (EllipticalArc OriginRelative <$ string "a" <*> manyComma ellipticalArgs)
    <|> (EndPath <$ string "Z" <* commaWsp)
    <|> (EndPath <$ string "z" <* commaWsp)
  where
  pointList = fromNE $ point `sepBy1` commaWsp

  pointPair = Tuple <$> point <* commaWsp <*> point

  pointPairList = fromNE $ pointPair `sepBy1` commaWsp

  coordList = fromNE $ num `sepBy1` commaWsp

  curveToArgs =
    tuple3
      <$> (point <* commaWsp)
      <*> (point <* commaWsp)
      <*> point

  numComma = num <* commaWsp

  flagComma = flag <* commaWsp

  ellipticalArgs =
    tuple6
      <$> numComma
      <*> numComma
      <*> numComma
      <*> flagComma
      <*> flagComma
      <*> point

manyComma :: forall a. Parser String a -> Parser String (Array a)
manyComma a = fromNE $ a `sepBy1` commaWsp

fromNE :: forall f a. Functor f => f (NonEmptyList a) -> f (Array a)
fromNE = map (Array.fromFoldable <<< NE.toList)

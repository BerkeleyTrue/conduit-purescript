module Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat) where

import Prelude

import Ansi.Codes as Ansi.Codes
import Ansi.Output as Ansi.Output
import Data.Array as Data.Array
import Data.DateTime as Data.DateTime
import Data.Formatter.DateTime as Data.Formatter.DateTime
import Data.Int as Data.Int
import Data.String as Data.String
import Data.String.Utils (padEnd)
import Data.Time.Duration as Data.Time.Duration
import Effect.Aff (Aff, throwError)
import Effect.Aff as Effect.Aff
import Effect.Class (liftEffect)
import Effect.Class as Effect.Class
import Effect.Class.Console as Console
import Effect.Now as Effect.Now
import HTTPurple (Request, Response(..))
import HTTPurple as HTTP
import Server.Infra.HttPurple.Types (Middleware')

-- | The lifecycle functions around logging.
-- |
-- | Used to prepare metadata for the logs.
type LogLifecycle route a =
  { after :: Request route -> Response -> a -> Aff String
  , before :: HTTP.Request route -> Aff a
  }

-- | A helper that encapsulates the different information around request time.
type LogTime =
  { duration :: Data.Time.Duration.Milliseconds
  , start :: Data.DateTime.DateTime
  , stop :: Data.DateTime.DateTime
  }

colorMethod :: HTTP.Method -> String
colorMethod method = Ansi.Output.withGraphics graphics (padEnd 4 (renderMethod method))
  where
  graphics = case method of
    HTTP.Connect -> Ansi.Output.foreground Ansi.Codes.Blue
    HTTP.Delete -> Ansi.Output.foreground Ansi.Codes.Red
    HTTP.Get -> Ansi.Output.foreground Ansi.Codes.Cyan
    HTTP.Head -> Ansi.Output.foreground Ansi.Codes.Cyan
    HTTP.Options -> Ansi.Output.foreground Ansi.Codes.Blue
    HTTP.Patch -> Ansi.Output.foreground Ansi.Codes.Magenta
    HTTP.Post -> Ansi.Output.foreground Ansi.Codes.Yellow
    HTTP.Put -> Ansi.Output.foreground Ansi.Codes.Green
    HTTP.Trace -> Ansi.Output.foreground Ansi.Codes.Blue

colorStatus :: HTTP.Status -> String
colorStatus status = Ansi.Output.withGraphics graphics (show status)
  where
  graphics
    | status < 200 = Ansi.Output.foreground Ansi.Codes.Blue
    | 200 <= status && status < 300 = Ansi.Output.foreground Ansi.Codes.Green
    | 300 <= status && status < 400 = Ansi.Output.foreground Ansi.Codes.Cyan
    | 400 <= status && status < 500 = Ansi.Output.foreground Ansi.Codes.BrightYellow
    | otherwise = Ansi.Output.foreground Ansi.Codes.Red

-- | Logs the request given the lifecycle functions.
log :: forall a route. LogLifecycle route a -> Middleware' route
log config router request =
  Effect.Aff.generalBracket
    (config.before request)
    { completed: \response before -> do
        str <- config.after request response before
        Console.log str
    , failed: \error _ -> do
        Console.error $ "Error: " <> show error
        throwError error
    , killed: \error _ -> do
        Console.error $ "Error: " <> show error
        throwError error
    }
    \_ -> router request

-- | Helper for logging when all you need is the time metadata.
logWithTime :: forall route. (LogTime -> Request route -> Response -> Aff String) -> Middleware' route
logWithTime format = log { after, before }
  where
  after
    :: Request route
    -> Response
    -> Data.DateTime.DateTime
    -> Aff String
  after request response start = do
    stop <- liftEffect Effect.Now.nowDateTime
    let duration = Data.DateTime.diff stop start
    format { duration, start, stop } request response

  before :: Request route -> Aff Data.DateTime.DateTime
  before _ = Effect.Class.liftEffect Effect.Now.nowDateTime

renderDateTime :: Data.DateTime.DateTime -> String
renderDateTime = Ansi.Output.withGraphics (Ansi.Output.foreground Ansi.Codes.BrightBlack) <<<
  Data.Formatter.DateTime.format
    ( Data.Array.toUnfoldable
        [ Data.Formatter.DateTime.Placeholder "["
        , Data.Formatter.DateTime.DayOfMonthTwoDigits
        , Data.Formatter.DateTime.Placeholder "/"
        , Data.Formatter.DateTime.MonthShort
        , Data.Formatter.DateTime.Placeholder "/"
        , Data.Formatter.DateTime.YearFull
        , Data.Formatter.DateTime.Placeholder ":"
        , Data.Formatter.DateTime.Hours24
        , Data.Formatter.DateTime.Placeholder ":"
        , Data.Formatter.DateTime.MinutesTwoDigits
        , Data.Formatter.DateTime.Placeholder ":"
        , Data.Formatter.DateTime.SecondsTwoDigits
        , Data.Formatter.DateTime.Placeholder "]"
        ]
    )

renderDuration :: Data.Time.Duration.Milliseconds -> String
renderDuration = case _ of
  Data.Time.Duration.Milliseconds x -> show (Data.Int.round x) <> "ms"

renderMethod :: HTTP.Method -> String
renderMethod = case _ of
  HTTP.Connect -> "CONNECT"
  HTTP.Delete -> "DELETE"
  HTTP.Get -> "GET"
  HTTP.Head -> "HEAD"
  HTTP.Options -> "OPTIONS"
  HTTP.Patch -> "PATCH"
  HTTP.Post -> "POST"
  HTTP.Put -> "PUT"
  HTTP.Trace -> "TRACE"

-- | A middleware that logs request in an unstandardized development format.
-- | The logs are more verbose, colorful, and a bit easier to read.
developmentLogFormat :: forall route. Middleware' route
developmentLogFormat = logWithTime developmentLogFormat'

developmentLogFormat'
  :: forall route
   . LogTime
  -> Request route
  -> Response
  -> Aff String
developmentLogFormat' logTime request response =
  pure
    $ Data.String.joinWith
        " "
        ( time
            <> status response.status
            <> method
            <> duration
            <> path
        )

  where
  time = [ renderDateTime logTime.start ]
  duration = [ renderDuration logTime.duration ]
  method = [ colorMethod request.method ]
  path = [ " /" <> Data.String.joinWith "/" request.path ]
  status x = [ colorStatus x ]

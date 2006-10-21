{-
    JSDate.hs
    Dateオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-9_Date_Objects.html
-}

module JSDate where
import Monad
import System.Time

import DataTypes
import Context

-- Date.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("constructor", constructor,    7),
                                        ("toString",    toStringMethod, 0),
                                        ("valueOf",     valueOf,        1)]
    }

-- Date()
function :: NativeFunction
function [] =
    do time <- liftAll $ getClockTime
       return $ String $ show time

-- new Date()
constructor :: NativeFunction
constructor [] =
    do time <- liftAll $ getClockTime
       return $ nullObject { objClass = "Date", objValue = toValue $ toMillisecs time }

-- Date.prototype.toString
toStringMethod :: NativeFunction
toStringMethod _ =
    do this <- readRef =<< getThis
       if objClass this == "Date"
          then liftAll $ liftM (String . calendarTimeToString) (millisecsToCT $ getMillisecs this)
          else throw $ TypeError $ "Date.prototype.toString called on incompatible"

-- Date.prototype.valueOf
valueOf :: NativeFunction
valueOf _ =
    do this <- readRef =<< getThis
       if objClass this == "Date"
          then return $ toValue $ getMillisecs this
          else throw $ TypeError $ "Date.prototype.toString called on incompatible"

getMillisecs (Object { objValue = Number (Integer millisecs) }) = millisecs

toMillisecs ct =
    let TOD secs picosecs = ct
        in secs * 1000 + picosecs `div` 1000000000

millisecsToCT millisecs =
    do let (secs, milli) = millisecs `divMod` 1000
           nanosecs = milli * 1000000000
       toCalendarTime =<< return (TOD secs nanosecs)

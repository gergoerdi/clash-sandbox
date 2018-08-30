{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "VGA"
    , clashModule = "VGA"
    , clashTopName = "VGA"
    , topName = "Top"
    , ipCores = ["ClockMan25"]
    , vhdlSrcs = ["Top"]
    , clashFlags = ["-i../../lib/src-clash"]
    , shakeDir = "../../shake"
    }

main :: IO ()
main = mainFor clashProject

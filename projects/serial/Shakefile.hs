{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Serial"
    , clashModule = "Serial"
    , clashTopName = "Serial"
    , topName = "Top"
    , ipCores = []
    , vhdlSrcs = ["Top"]
    , clashFlags = ["-i../../lib/src-clash"]
    , shakeDir = "../../shake"
    }

main :: IO ()
main = mainFor clashProject

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Counter"
    , clashModule = "Counter"
    , clashTopName = "Counter"
    , topName = "CounterTop"
    , clashFlags = ["-i../../clash-utils/src-clash"]
    , shakeDir = "../../clash-utils/shake"
    }

main :: IO ()
main = mainFor clashProject

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "VGA"
    , clashModule = "VGA"
    , clashTopName = "VGA"
    , topName = "Top"
    , clashFlags = ["-i../../clash-utils/src-clash"]
    , shakeDir = "../../clash-utils/shake"
    }

main :: IO ()
main = mainFor clashProject

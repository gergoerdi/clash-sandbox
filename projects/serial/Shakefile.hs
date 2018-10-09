{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Serial"
    , clashModule = "Serial"
    , clashTopName = "Serial"
    , topName = "Top"
    , clashFlags = [ "-i../../clash-utils/src-clash"
                   , "-Wno-partial-type-signatures"
                   ]
    , shakeDir = "../../clash-utils/shake"
    }

main :: IO ()
main = mainFor clashProject

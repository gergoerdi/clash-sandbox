{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Counter"
    , clashModule = "Counter"
    , clashTopName = "Counter"
    , topName = "CounterTop"
    , clashFlags = ["-i../../lib/src-clash"]
    , shakeDir = "../../shake"
    }

main :: IO ()
main = mainFor clashProject

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Brainfuck"
    , clashModule = "Brainfuck"
    , clashTopName = "Brainfuck"
    , topName = "Top"
    , clashFlags = ["-i../../clash-utils/src-clash"]
    , shakeDir = "../../clash-utils/shake"
    }

main :: IO ()
main = mainFor clashProject

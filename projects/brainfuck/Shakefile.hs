{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Brainfuck"
    , clashModule = "Brainfuck"
    , clashTopName = "Brainfuck"
    , topName = "Top"
    , clashFlags = ["-i../../lib/src-clash"]
    , shakeDir = "../../shake"
    }

main :: IO ()
main = mainFor clashProject

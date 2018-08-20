{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "Brainfuck"
    , clashModule = "Brainfuck"
    , clashTopName = "Brainfuck"
    , topName = "Top"
    , ipCores = []
    , vhdlSrcs = ["Top"]
    , clashFlags = ["-i../../lib/src-clash"]
    , shakeDir = "../../shake"
    }

main :: IO ()
main = mainFor clashProject

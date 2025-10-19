module Main (main) where
import Data.Csv

video_info_data :: FilePath
video_point_data :: FilePath
video_info_data  = "~/data/cvse_video_info_data.csv"
video_point_data = "~/data/cvse_video_point_data.csv"

main :: IO ()
main = putStrLn "Hello, Haskell!"

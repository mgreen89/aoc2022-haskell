module Advent.Extra (
  module Advent,
  showAoCError,
  showAoCSubmitRes,
)
where

import Advent
import Text.Printf

-- | Convert an 'AoCError' into a list of human-readable strings.
showAoCError :: AoCError -> [String]
showAoCError = \case
  AoCClientError e ->
    [ "Error contacting Advent of Code server to fetch input"
    , "Possible invalid session key"
    , printf "Server response: %s" (show e)
    ]
  AoCReleaseError t ->
    ["Challenge not yet released!", printf "Please wait %s" (show t)]
  AoCThrottleError -> ["Too many requests at a time.  Please slow down."]

-- | Convert a 'SubmitRes' to a human-readable string description.
showAoCSubmitRes :: SubmitRes -> String
showAoCSubmitRes = \case
  SubCorrect Nothing -> "Answer was correct!"
  SubCorrect (Just r) ->
    printf
      "Answer was correct, and you made the global leaderboard at rank %d !!"
      r
  SubIncorrect t h ->
    let hintStr = maybe "" (printf "  Hint: Answer was %s") h
        (m, s) = t `divMod` 60
     in printf
          "Answer was incorrect!%s  Please wait %dmin %dsec before submitting again"
          hintStr
          m
          s
  SubWait t ->
    let (m, s) = t `divMod` 60
     in printf "Answer re-submitted too soon.  Please wait %dmin %dsec" m s
  SubInvalid ->
    "Submission was rejected.  Maybe not unlocked yet, or already answered?"
  SubUnknown _ -> "Response from server was not recognized."

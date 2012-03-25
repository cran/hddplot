.onAttach <-
function (lib,pkg)  {
    packageStartupMessage(
   "\nPrior to version 0.53, an error in simulateScores() had",
        "\nthe effect that a random set of features was selected.\n")
  }


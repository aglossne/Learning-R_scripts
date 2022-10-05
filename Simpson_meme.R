library(meme)
if (.Platform$OS.type == "windows") {
  windowsFonts(Comic = windowsFont("Comic Sans MS"))
}
bart <- "bart_simpson_chalkboard-5157.gif" # source: http://free-extras.com/images/bart_simpson_chalkboard-5157.htm

meme(bart, "\nfor (i in 1:100) {\n  print(\"I will not use loops in R\")\n}", size = 1.8, font = "Comic", vjust = 0, r = 0)

meme(bart, "\njag ska alltid använda rätt stopporsak \njag ska alltid använda rätt stopporsak \njag ska alltid använda rätt stopporsak \njag ska alltid använda rätt stopporsak \njag ska alltid använda rätt stopporsak \njag ska alltid använda rätt stopporsak\n", size = 1.8, font = "Comic", vjust = 0, r = 0)

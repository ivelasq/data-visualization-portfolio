# install.packages("RCurl", dependencies = TRUE)
source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}

source_github("https://raw.githubusercontent.com/robertgrant/pictogram/master/pictogram.R")

# install.packages("png", dependencies = TRUE)
require(png)

dat <- read.csv("fatalities.csv")

img <- readPNG(system.file("img", "images.png", package="png"))
  
pictogram(icon = img, n = c( 12, 35, 7),
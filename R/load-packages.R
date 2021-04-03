# load packages
MyInstall <- function(string){
  if(! require(string, character.only = TRUE))
  {
    install.packages(string)
  }
  require(string, character.only = TRUE)
}
lapply(package.list, MyInstall)


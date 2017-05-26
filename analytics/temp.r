#update the progress for every 1% complete
pct <- 0
itNum <- 2000
for(iCount in 1:itNum){

if(iCount%%(itNum/100) == 0){
  pct <- pct + 1
  
  cat("\r", pct, "% complete.", sep = "")
  Sys.sleep(.1)
}
}

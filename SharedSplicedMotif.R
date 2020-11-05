#Finding	a	Shared	Spliced	Motif

#1. RNA	Splicing

source("LongestCommonSubsequence.R")
con <- "rosalind_splc.txt"
conn <- file(con,open="r")
linn <- readLines(conn)
j <- 0
vec <- vector()
for (i in 1:length(linn)){
  if(any(grepl(">Rosalind_",linn[i]))){
    j <- j+1
  }
  if(!any(grepl(">Rosalind_",linn[i]))){
    vec[[j]] <- ifelse(!is.na(vec[j]),
                       paste(vec[j],linn[i],sep = ""),
                       paste(linn[i],sep=""))
  }
}
close(conn)
vec
s1 <- "AACCTTGG"
s2 <- "ACACTGTGA"
s1 <- vec[1]
s2 <- vec[2]
s1 <- as.character(s1)
s1 <- strsplit(s1,NULL)[[1]]
s2 <- as.matrix(s2)
s2 <- as.character(s2)
s2 <- strsplit(s2,NULL)[[1]]
s2 <- as.matrix(s2)

x <- lcsm(s1,s2)
x

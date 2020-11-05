#Finding	a	Spliced Motif

#read FASTA
con <- "rosalind_sseq.txt"
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

s <- vec[1]
t <- vec[2]
s <- as.character(s)
s <- as.matrix(strsplit(s,NULL)[[1]])
s
t <- as.character(t)
t <- as.matrix(strsplit(t,NULL)[[1]])
t
ret <- vector("numeric",length(t))

for(i in 1:length(t)){
  for(j in 1:length(s)){
    if(t[i] == s[j] && i==1){
      ret[[i]] <-j
      break
    }
    if(t[i] == s[j] && j>ret[i-1]){
      ret[[i]] <- j
      break
    }
  }
}
write.table(ret,quote=FALSE,eol = " ",row.names = FALSE,col.names = FALSE)

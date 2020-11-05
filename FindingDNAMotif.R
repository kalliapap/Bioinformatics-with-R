#Finding a Motif in DNA

s<- unlist(read.table("rosalind_subs.txt",nrow=1))
s <- as.character(s)
t <- unlist(read.table("rosalind_subs.txt",skip=1,nrow=1))
#t<-"CAGTGATGCTAGTCAGGCTAGACACACAAA"
t <- as.character(t)
s <- strsplit(s, NULL)[[1]]
t <- strsplit(t, NULL)[[1]]
m1 <-as.matrix(s)
m2 <- as.matrix(t)

m1
m2
#----------------------------------------
subString <- function(k,l){
  positions <- c()
  if(length(k) < length(l)){
    ch<-"t is not a substring of s"
    return(ch)
  } 
  for(i in 1:length(k)){
    flag <- FALSE
    if(k[i] == l[1]){
      pos <- i
      for(j in 1:length(l)){
        if((k[pos] == l[j]) && (!is.na(k[pos]))){
          pos <- pos + 1
        }else{
          flag <-TRUE
          break
        }
        if(flag == FALSE && j == length(l)) {
          positions <- c(positions, i)
          break;
        }
      }
     
    }
  }
  
write.table(positions,quote=FALSE,eol = " ",row.names = FALSE,col.names = FALSE)
return(positions)
}

subString(m1,m2)


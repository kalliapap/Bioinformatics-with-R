#Longest Common Subsequence Problem

k<- unlist(read.table("rosalind_ba5c.txt",nrow=1))
k1 <- unlist(read.table("rosalind_ba5c.txt",skip=1,nrow=1))
k <- as.character(k)
k <- strsplit(k, NULL)[[1]]
k1 <- as.character(k1)
k1 <- strsplit(k1, NULL)[[1]]
k1 <- as.matrix(k1)
k<- as.matrix(k)


lcsm <- function(a,b){
  
    l <- matrix( ,length(a),length(b))
    sol <- matrix(,length(a),length(b))
    
    for(i in 1:(length(a))){
      l[[i,1]] <- 0
      sol[[i,1]] <- 0
    }
    for(j in 1:(length(b))){
      l[[1,j]] <- 0
      sol[[1,j]] <-0
    }
    
    for(i in 2:length(a)){
       for(j in 2:length(b)){
          if(a[i-1]==b[j-1]){
            l[[i,j]] <- l[i-1,j-1] +1 
            sol[[i,j]] <- "diagonal" 
          } else{
            l[[i,j]] <- max(l[i,j-1],l[i-1,j])
            if(l[i,j] == l[i-1,j]){
              sol[[i,j]] <- "top"
            }else{
              sol[[i,j]] <- "left"
            }
          }
      }
    }
    
    x <- sol[length(a),length(b)]
    ans <- ""
    m <- length(a)
    n <- length(b)
    while(x != "0"){
      print(m)
      if(sol[m,n] == "diagonal"){
        ans <- paste(a[m-1],ans,sep = "")
        m <- m-1
        n <- n-1
      }else if(sol[m,n] == "left"){
        n <- n-1
      }else if(sol[m,n] == "top"){
        m <- m-1
      }
      x <- sol[m,n]
    }
    print(ans)
    write.table(ans,quote=FALSE,eol = " ",row.names = FALSE,col.names = FALSE)
    return (ans)
}
x<-lcsm(k,k1)
x

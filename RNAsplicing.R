#1. RNA	Splicing

#read FASTA
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

dna <- vec[1]
introns <- vector()
for(i in 2:length(vec)){
  introns[[i-1]] <- vec[i]
}

dna1<-dna
for(i in 1:length(introns)){
  dna1 <- gsub(introns[i],"",dna1)
}
dna1 <- strsplit(as.character(dna1),"")
for(i in dna1){
  dna1<-as.vector(ifelse(i=="T", i<-gsub("T","U",i),i))
}
dna1 <- as.character(dna1)
write.table(dna1,file= "dna.txt",quote=FALSE,eol = "",row.names = FALSE,col.names = FALSE)
dna1 <- unlist(read.table("dna.txt"))
dna1 <- as.character(dna1)
aminoac <- as.matrix(sapply(seq(from=1,to=nchar(dna1),by=3),function(i) substr(dna1,i,i+2)))
aminoac

map = c("GCA"="A","GCC"="A","GCG"="A","GCU"="A","AGA"="R","AGG"="R","CGA"="R","CGC"="R","CGG"="R","CGU"="R",
        "GAC"="D","GAU"="D","AAC"="N","AAU"="N","UGC"="C","UGU"="C","GAA"="E","GAG"="E","CAA"="Q","CAG"="Q",
        "GGA"="G","GGC"="G","GGG"="G","GGU"="G","CAC"="H","CAU"="H","AUA"="I","AUC"="I","AUU"="I","UUA"="L",
        "UUG"="L","CUA"="L","CUC"="L","CUG"="L","CUU"="L","AAA"="K","AAG"="K","AUG"="M","UUC"="F","UUU"="F",
        "CCA"="P","CCC"="P","CCG"="P","CCU"="P","AGC"="S","AGU"="S","UCA"="S","UCC"="S","UCG"="S","UCU"="S",
        "ACA"="T","ACC"="T","ACG"="T","ACU"="T","UGG"="W","UAC"="Y","UAU"="Y","GUA"="V","GUC"="V","GUG"="V",
        "GUU"="V","UAA"="","UAG"="","UGA"="")

proteins <- unname(map[aminoac])
proteins
write.table(proteins,quote=FALSE,eol = "",row.names = FALSE,col.names = FALSE)


#Turning DNA into Protein

#1. Counting DNA Nucleotides
dseq <- unlist(read.table("rosalind_dna.txt"))
dseq
table(strsplit(as.character(dseq), "")[[1]])


#2. Transcribing DNA into RNA
rseq <- unlist(read.table("rosalind_rna.txt"))
m <- strsplit(as.character(rseq),"")
m

for(i in m){
  rna<-as.vector(ifelse(i=="T", i<-gsub("T","U",i),i))
}

write.table(rna,file= "rna.txt",quote=FALSE,eol = "",row.names = FALSE,col.names = FALSE)

#3. Complementing a Strand of DNA
seq<-unlist(read.table("rosalind_revc.txt"))
seq<-as.character(seq)
rev <-rev(strsplit(seq,NULL)[[1]])
rev
map=c("T"="A","A"="T","G"="C","C"="G")
s<-unname(map[rev])
s

write.table(s,file= "rev.txt",quote=FALSE,eol = "",row.names = FALSE,col.names = FALSE)

#4. Counting Point Mutations 

s <- unlist(read.table("rosalind_hamm1.txt"))
t <- unlist(read.table("rosalind_hamm2.txt"))
s <- as.character(s)
t <- as.character(t)
s <- strsplit(s, NULL)[[1]]
t <- strsplit(t, NULL)[[1]]
m1 <-as.matrix(s)
m2 <- as.matrix(t)
count <- 0
for(i in 1:length(m1)){
  if(m1[i]!=m2[i]){
    count=count+1
  }
}
count


#5. Translating RNA into Protein

t <- unlist(read.table("rosalind_prot.txt"))
t <- as.character(t)
t
aminoac <- as.matrix(sapply(seq(from=1,to=nchar(t),by=3),function(i) substr(t,i,i+2)))
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

write.table(proteins,file= "proteins.txt",quote=FALSE,eol = "",row.names = FALSE,col.names = FALSE)






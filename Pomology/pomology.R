install.packages('seqinr')
library(seqinr)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.16")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()

assvd_k1 <- read.fasta(file = "ASSVd_K1.fasta")
assvdk <- assvd_k1[[1]]
assvdk
length(assvdk)
print(assvdk[1:50])
table(assvdk)
GC(assvdk)


slidingwindowplot <- function(windowsize, inputseq)
{
  starts <- seq(1, length(inputseq)-windowsize, by = windowsize)
  n <- length(starts)    # Find the length of the vector "starts"
  chunkGCs <- numeric(n) # Make a vector of the same length as vector "starts", but just containing zeroes
  for (i in 1:n) {
    chunk <- inputseq[starts[i]:(starts[i]+windowsize-1)]
    chunkGC <- GC(chunk)
    # print(chunkGC)
    chunkGCs[i] <- chunkGC
  }
  plot(starts,chunkGCs,type="b",xlab="Nucleotide start position",ylab="GC content")
}

# 300개씩 서열을 나누어서 GC() 기능을 사용합니다.
slidingwindowplot(50, assvdk)


breaburn <- read.fasta(file = "braeburn.fasta")
bb <- breaburn[[1]]

golden <- read.fasta(file = "GD.fasta")
gd <- golden[[1]]

dotPlot(bb, gd)



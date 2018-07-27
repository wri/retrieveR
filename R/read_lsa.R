tv <- read.table("termvectors.txt", sep="|", skip=1)
tv <- as.matrix(tv)
rownames(tv) <- tv[,1]
tv <- tv[,-1]

lsaspace <- mapply(tv, FUN=as.numeric)
lsaspace <- as.numeric(lsaspace)
lsaspace <- matrix(lsaspace, ncol=dim(tv)[2], nrow=dim(tv)[1])
rownames(lsaspace) <- rownames(lsaspace)
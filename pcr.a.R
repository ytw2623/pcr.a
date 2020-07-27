##### This pcr.a() is for calculating the cycles 4 & 5 and above.
pcr.a <- function(x.t = x.t, x.a = x.a, errot.t = error.t, error.a = error.a,
 cycles = cycles, n = n, set.val = set.val)
{
     cycle.0 <- rep(set.val, n)
     
     # for cycle 1
     res <- NA
     for(j in 1:length(x.t)){
          for(k in 1:length(table(cycle.0))){
               res1 <- cbind(as.numeric(rownames(table(cycle.0)))[[k]] + x.t[j],
                table(cycle.0)[[1]] * error.t[j])
               res <- rbind(res, res1)
          }
     }
     result <- res[-1,]
     result <- as.data.frame(result)
     result[rownames(subset(result, V1==set.val)),2] <- subset(result, V1==set.val)[2] + n

     # for cycle 3 (identical to cycle 2 on another chain)
     res <- NA
     for(j in 1:dim(result)[1]){
          for(k in 1:length(x.a)){
               res1 <- cbind(result[j,1] + x.a[k], result[j,2] * error.a[k])
               res <- rbind(res, res1)
          }
     }
     result <- res[-1,]
     result <- as.data.frame(result)

     for(i in 1:cycles){
          res <- NA
          for(j in 1:dim(result)[1]){
               for(k in 1:length(x.t)){
                    res1 <- cbind(result[j,1] + x.t[k], result[j,2] * error.t[k])
                    res <- rbind(res, res1)
               }
          }
          result <- res[-1,]
          result <- as.data.frame(result)

          res1 <- 0
          res <- 0
          for(l in 1:length(table(result[,1]))){
               res1 <- cbind(as.numeric(rownames(table(result[,1])))[l],
                apply(subset(result, result[,1]==as.numeric(rownames(table(result[,1])))[l]), 2, sum)[2])
               res <- rbind(res1, res)
          }
          rownames(res) <- res[,1]
          result <- res[order(res[,1]),]
          result <- as.data.frame(result[-1,])

          res <- NA
          for(j in 1:dim(result)[1]){
               for(k in 1:length(x.a)){
                    res1 <- cbind(result[j,1] + x.a[k], result[j,2] * error.a[k])
                    res <- rbind(res, res1)
               }
          }
          result <- res[-1,]
          result <- as.data.frame(result)

          res1 <- 0
          res <- 0
          for(l in 1:length(table(result[,1]))){
               res1 <- cbind(as.numeric(rownames(table(result[,1])))[l],
                apply(subset(result, result[,1]==as.numeric(rownames(table(result[,1])))[l]), 2, sum)[2])
               res <- rbind(res1, res)
          }
          rownames(res) <- res[,1]
          result <- res[order(res[,1]),]
          result <- as.data.frame(result[-1,])
     }

     res1 <- 0
     res <- 0
     for(l in 1:length(table(result[,1]))){
          res1 <- cbind(as.numeric(rownames(table(result[,1])))[l],
           apply(subset(result, result[,1]==as.numeric(rownames(table(result[,1])))[l]), 2, sum)[2])
          res <- rbind(res1, res)
     }
     rownames(res) <- res[,1]
     res <- res[order(res[,1]),]
     res <- res[-1,-1]

     res2 <- matrix(rep(0,41), 1,41)
     colnames(res2) <- 0:40
     for(i in 1:length(res)){
          for(j in 1:length(res2)){
               if(colnames(res2)[j]==names(res)[i]) res2[j] <- res[[i]]
          }
     }

     return(res2)
}

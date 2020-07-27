##### Tth
error.t <- c(.050, .150, .700, .100)
error.a <- c(.100, .350, .500, .050)
x.t <- c(-2, -1, 0, 1)
x.a <- c(-2, -1, 0, 1)
n <- 100
set.val <- 26



### cycle 3
res <- pcr.a.first(x.t, x.a, error.t, error.a, n, set.val) * 3
t(res/sum(res))
#par(cex.axis=1.8, mar=c(5,5,3,2), cex.lab=1.8, cex.main=1.8,
# xaxs="i", yaxs="i")
#barplot(res/sum(res), ylim=c(0,1), space=.1, las=2)
#write.csv(t(res/sum(res)), "cycle-03.csv")



### cycle 5
cycles <- 1 # choose 1 for 5 cycles; 4 for 10; 6 for 15; 9 for 20; 11 for 25; 14 for 30; 16 for 35
res.a <- pcr.a(x.t, x.a, error.t, error.a, cycles, n, set.val) * 3
res.b <- pcr.a.first(x.t, x.a, error.t, error.a, n, set.val) * 7
res <- res.a + res.b
t(res/sum(res))
#par(cex.axis=1.8, mar=c(5,5,3,2), cex.lab=1.8, cex.main=1.8,
# xaxs="i", yaxs="i")
#barplot(res/sum(res), ylim=c(0,1), space=.1, las=2)
#write.csv(t(res/sum(res)), "cycle-05.csv")



### cycle 10
cycles <- 4 # choose 1 for 5 cycles; 4 for 10; 6 for 15; 9 for 20; 11 for 25; 14 for 30; 16 for 35
res.a <- pcr.a(x.t, x.a, error.t, error.a, cycles, n, set.val)
cycles <- 3 # choose 1 for 5 cycles; 4 for 10; 6 for 15; 9 for 20; 11 for 25; 14 for 30; 16 for 35
res.b <- pcr.a(x.t, x.a, error.t, error.a, cycles, n, set.val) * 5
cycles <- 2 # choose 1 for 5 cycles; 4 for 10; 6 for 15; 9 for 20; 11 for 25; 14 for 30; 16 for 35
res.c <- pcr.a(x.t, x.a, error.t, error.a, cycles, n, set.val) * 9
cycles <- 1 # choose 1 for 5 cycles; 4 for 10; 6 for 15; 9 for 20; 11 for 25; 14 for 30; 16 for 35
res.d <- pcr.a(x.t, x.a, error.t, error.a, cycles, n, set.val) * 13
res.e <- pcr.a.first(x.t, x.a, error.t, error.a, n, set.val) * 17
res <- res.a + res.b + res.c + res.d + res.e
t(res/sum(res))
#par(cex.axis=1.8, mar=c(5,5,3,2), cex.lab=1.8, cex.main=1.8,
# xaxs="i", yaxs="i")
#barplot(res/sum(res), ylim=c(0,1), space=.1, las=2)
#write.csv(t(res/sum(res)), "cycle-10.csv")



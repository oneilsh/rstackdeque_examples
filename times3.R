rm(list = ls())
library(rstackdeque)
library(microbenchmark)

RUNTESTS <- FALSE

if(RUNTESTS) {
  counts <- c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100)*1000
  reps <- 10
  results <- vector("list",1000)
  resnum <- 1
  
  insert_n <- function(n, structtype, insertfunc) {
    s <- structtype()
    for(i in seq(1,n)) {
      s <- insertfunc(s, i)
      if(i%%(n/10) == 0) {
        print(i/n)
      }
    }
    return(s)
  }
  
  remove_n <- function(n, removefunc, s) {
    for(i in seq(1,n)) {
      s <- removefunc(s)
      if(i%%(n/10) == 0) {
        print(i/n)
      }
    }
    return(s)
  }
  
  mix_n <- function(n, structtype, insertfunc, removefunc) {
    s <- structtype()
    for(i in seq(1,n)) {
      if(i%%3 == 0) {
        s <- removefunc(s)
      } else {
        s <- insertfunc(s, i)      
      }
      if(i%%(n/10) == 0) {
        print(i/n)
      }
    }
    return(s)
  }
  
  structs <- as.list(c(rstack, rdeque, rpqueue))
  insertfuncs <- as.list(c(insert_top, insert_back, insert_back))
  removefuncs <- as.list(c(without_top, without_front, without_front))
  structnames <- as.list(c("rstack", "rdeque", "rpqueue"))
  
  for(count in counts) {
    for(rep in 1:reps) {
      for(structindex in seq(1,length(structs))) {
        struct <- structs[[structindex]]
        insertfunc <- insertfuncs[[structindex]]
        removefunc <- removefuncs[[structindex]]
        structname <- structnames[[structindex]]
        print(paste(count, rep/reps, structname))
        
        # We always remove s and clear the gc() before inserting. we count environment usage immediately
        # before and after the run and use the differences.
        rm(s)
        gc()
        before <- memory.profile()
        res <- microbenchmark({s <- insert_n(count, struct, insertfunc)}, times = 1)
        after <- memory.profile()
        results[[resnum]] <- data.frame(test = "insert", struct = structname, seconds = res$time/1000000000, count = count, rep = rep, envcount = after[["environment"]])
        resnum <- resnum + 1
        
        before <- memory.profile()
        res <- microbenchmark({s <- remove_n(count, removefunc, s)}, times = 1)
        after <- memory.profile()
        results[[resnum]] <- data.frame(test = "remove", struct = structname, seconds = res$time/1000000000, count = count, rep = rep, envcount = after[["environment"]])
        resnum <- resnum + 1 
        
        
        rm(s)
        gc()
        before <- memory.profile()
        res <- microbenchmark({s <- mix_n(count, struct, insertfunc, removefunc)}, times = 1)
        after <- memory.profile()
        results[[resnum]] <- data.frame(test = "mix", struct = structname, seconds = res$time/1000000000, count = count, rep = rep, envcount = after[["environment"]])
        resnum <- resnum + 1
      }
    }
  }
  
  library(reshape2)
  allresdf <- do.call("rbind", results)
  allresdf_long <- melt(allresdf, c("test", "struct", "rep", "count"), variable.name = "variable", value.name = "value")
  print(allresdf_long)
  
  write.table(allresdf_long, "times3_resdf_long.txt", quote = F, sep = "\t", row.names = F)
}
allresdf_long<- read.table("times3_resdf_long.txt", header = T, sep = "\t", stringsAsFactors = F)


allresdf_long$count <- as.numeric(allresdf_long$count)
allresdf_long$struct <- factor(allresdf_long$struct, levels = c("rstack", "rdeque", "rpqueue"))
allresdf_long$test <- factor(allresdf_long$test, levels = c("insert", "remove", "mix"),
                             labels = c("Inserts", "Removes", "Mix"))

library(ggplot2)
p <- ggplot(allresdf_long[allresdf_long$variable == "seconds" & allresdf_long$count >= 10000,]) +
  geom_jitter(aes(x = count, y = value, group = count)) +
  facet_grid(test ~ struct) +
  geom_smooth(aes(x = count, y = value), method = "lm", se = F) +
  scale_x_continuous(name = "Number Operations Completed", breaks = seq(1,10)*10000, labels = paste(seq(1,10)*10, "K", sep = "")) +
  scale_y_continuous(name = "Seconds") +
  expand_limits(y = 0) +
  theme_grey(18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(p)
ggsave("runtimes.pdf", p, width = 12, height = 6)

library(ggplot2)
p <- ggplot(allresdf_long[allresdf_long$variable == "envcount" & allresdf_long$count >= 10000 & allresdf_long$test == "Inserts",]) +
  geom_jitter(aes(x = count, y = value, group = count)) +
  facet_grid(test ~ struct) +
  geom_smooth(aes(x = count, y = value), method = "lm", se = F) +
  scale_x_continuous(name = "Number Operations Completed", breaks = seq(1,10)*10000, labels = paste(seq(1,10)*10, "K", sep = "")) +
  scale_y_continuous(name = "Environments Created", breaks = seq(0,4)*100000, labels = c("0", "100K", "200K", "300K", "400K")) +
  expand_limits(y = 0) +
  theme_grey(18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(p)
ggsave("envcounts.pdf", p, width = 12, height = 3.5)


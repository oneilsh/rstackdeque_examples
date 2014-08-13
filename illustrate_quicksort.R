require(rstackdeque)
require(ggplot2)

quicksort <- function(x, depth) {
  depth <- depth + 1
  if(length(x) < 2) {return(x)}  
  
  pivot <- x[1]
  lt <- x[x < pivot]
  eq <- x[x == pivot]
  gt <- x[x > pivot]
  iter <- length(history)
  if(length(lt) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(lt), vals = lt, type = "Presorted", side = "Left"))}
  if(length(eq) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(eq), vals = eq, type = "Presorted", side = "Equal")) }
  if(length(gt) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(gt), vals = gt, type = "Presorted", side = "Right")) }
  
  lts <- quicksort(lt, depth)
  gts <- quicksort(gt, depth)
  
  ret <- c(lts, eq, gts)
  return(ret)
}

## Sample and sort, keeping the history with a global variable
## This is more an example of using an rstack as a growing dataframe than anything else
test <- sample(1:500, 500)
history <- rstack()
history <- insert_top(history, data.frame(iter = 0, depth = 0, pos = sort(test), vals = test, type = "Presorted", side = "Equal"))
res <- quicksort(test, 0)

## Quick plot:
history_df <- as.data.frame(history)
p <- ggplot(history_df) +
  geom_tile(aes(x = pos, y = depth, fill = vals, color = side)) +
  theme_bw(18) +
  scale_fill_continuous(name = "Element Value") +
  scale_color_discrete(name = "Recursion Side") +
  scale_x_continuous(name = "Element Position") +
  scale_y_reverse(name = "Recursion Depth")
plot(p)






require(rstackdeque)

## span = 2 if we're building, span = 1 if we're searching (post wall removal)
## if all = TRUE returns all unvisited neighbors
## unvisited neighbor defined by a "."
random_unvisited_neighbor <- function(maze, loc, dist = 2, all= FALSE) {
  ll <- rstack()
  if(maze[loc$row + dist, loc$col] == ".") {ll <- insert_top(ll, data.frame(row = loc$row + dist, col = loc$col))}
  if(maze[loc$row - dist, loc$col] == ".") {ll <- insert_top(ll, data.frame(row = loc$row - dist, col = loc$col))}
  if(maze[loc$row, loc$col + dist] == ".") {ll <- insert_top(ll, data.frame(row = loc$row, col = loc$col + dist))}
  if(maze[loc$row, loc$col - dist] == ".") {ll <- insert_top(ll, data.frame(row = loc$row, col = loc$col - dist))}
  if(length(ll) == 0) {return(NULL)}
  if(!all){ 
    return(sample(as.list(ll), 1)[[1]])
  } else {
    return(as.list(ll))
  }
}

## Finds a random unvisited cell in the table by collecting them all and return one
## as a random sample, otherwise returns NULL. There's probably a better way to do this.
random_unvisited_cell <- function(maze) {
  ll <- rstack()
  for(i in seq(1,nrow(maze))) {
    for(j in seq(1, ncol(maze))) {
      if(maze[i,j] == ".") {ll <- insert_top(ll, data.frame(row = i, col = j))}
    }
  }
  if(length(ll) == 0) {return(NULL)}
  return(sample(as.list(ll), 1)[[1]])
}


## A quick and dirty way to plot an ASCII maze, either pre-or post-solution
plot_ascii_maze <- function(maze, save = NULL) {
  library(reshape2)
  library(stringr)
  library(ggplot2)
  colnames(maze) <- as.character(seq(1, ncol(maze)))
  rownames(maze) <- as.character(seq(1, nrow(maze)))
  maze[maze == "#"] <- NA #-1*max(abs(as.numeric(maze)), na.rm = TRUE)
  maze[maze == " "] <- 1
  maze[maze == "."] <- 0
  maze[maze == "S" | maze == "E"] <- 1
  mazedf <- as.data.frame(maze)
  mazedf$row <- as.integer(rownames(mazedf))
  mazedflong <- melt(mazedf, c("row"), value.name = "celltype", variable.name = "col")
  mazedflong$col <- as.numeric(mazedflong$col)
  mazedflong$row <- as.numeric(mazedflong$row)
  mazedflong$celltype <- as.numeric(mazedflong$celltype)
  solution <- mazedflong[mazedflong$celltype < 0,]
  solution$celltype <- 1
  notvisited <- mazedflong[mazedflong$celltype == 0,]
  notvisited$celltype <- -1*max(abs(mazedflong$celltype), na.rm = TRUE)/2
  mazedflong$celltype <- abs(mazedflong$celltype)
  mazedflong$celltype[is.na(mazedflong$celltype)] <- -1*max(abs(mazedflong$celltype), na.rm = TRUE)
  
  library(ggplot2)
  p <- ggplot(mazedflong) +
    geom_tile(aes(x = col, y = row, fill = celltype), linetype = 0) +
    geom_point(data = solution, aes(x = col, y = row), color= "red") +  
    scale_y_reverse() + 
    coord_equal() +
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) 
  if(is.null(save)) {
    plot(p)
  } else {
    ggsave(save, p)
  }
}

create_ascii_maze <- function(nr = 51, nc = 51, save= NULL) {
  if(nr%%2 == 0) {nr <- nr + 1}
  if(nc%%2 == 0) {nc <- nc + 1}
  maze <- matrix(data = ".", nrow = nr, ncol = nc)
  maze[seq(2,nrow(maze)-1,2), ] <- "#"
  maze[, seq(2,ncol(maze)-1,2) ] <- "#"
  maze[c(1, nrow(maze)), ] <- "#"
  maze[ ,c(1, ncol(maze))] <- "#"
  
  maze[3,3] <- " "
  loc <- data.frame(row = 3, col = 3)
  visits <- rstack()
  step <- 1
  while(sum(maze == ".") > 0) {
    random_neighbor <- random_unvisited_neighbor(maze, loc)
    if(!is.null(random_neighbor)) {
      visits <- insert_top(visits, loc)
      ## a sneaky way to get the location of the wall to be dissolved, as the "mean" of loc and lasts position
      rclear = abs(loc$row - random_neighbor$row)/2 + min(loc$row, random_neighbor$row)
      cclear = abs(loc$col - random_neighbor$col)/2 + min(loc$col, random_neighbor$col)
      maze[rclear, cclear] <- " "
      loc <- random_neighbor
      maze[loc$row, loc$col] <- " "
    } else if(!empty(visits)) {
      loc <- peek_top(visits)
      visits <- without_top(visits)
    } else {
      loc <- random_unvisited_cell(maze)
      maze[loc$row, loc$col] <- " "
    }
    #if(step%%4 == 0) {plot_ascii_maze(maze, save = step)} # we could save every frame to create a video ;)
    step <- step + 1
  }
  maze[3,3] <- "E"
  maze[nrow(maze)-2, ncol(maze)-2] <- "S"  
  rownames(maze) <- rep("", nrow(maze))
  colnames(maze) <- rep("", ncol(maze))
  return(maze)
}

solve_ascii_maze_dfs <- function(maze) {
  # find the start and end of the maze
  end = data.frame(which(maze == "E", arr.ind = T))
  start = data.frame(which(maze == "S", arr.ind = T))

  # set all open corredors and the end and start as unvisited
  maze[maze == " " | maze == "E" | maze == "S"] <- "."
  
  # initialize the solution stack and start location
  loc = start
  path <- rstack()
  path <- insert_top(path, loc)
  
  step <- 1
  # while we're not out of the maze
  while(any(peek_top(path) != end)) {
    loc <- peek_top(path)
    
    # if the current location is unvisited, mark it visited with the current timestep
    if(maze[loc$row, loc$col] == ".") {maze[loc$row, loc$col] <- step}
    
    # grab a random unvisited neighbor; if there is one push it on the current path
    nextloc <- random_unvisited_neighbor(maze, peek_top(path), dist = 1)
    if(!is.null(nextloc)) {
      path <- insert_top(path, nextloc)
    } else {
      # otherwise backtrack and try again
      path <- without_top(path)
    }
    step <- step + 1
  }
  
  # mark the solution from end to start by negating the visit numbers
  end <- peek_top(path)
  maze[end$row, end$col] <- step
  while(!empty(path)) {
    loc <- peek_top(path)
    path <- without_top(path)
    maze[loc$row, loc$col] <- -1 * as.numeric(maze[loc$row, loc$col])
  }
  return(maze)
}


solve_ascii_maze_bfs <- function(maze) {
  # find the start and end of the maze
  end = data.frame(which(maze == "E", arr.ind = T))
  start = data.frame(which(maze == "S", arr.ind = T))
  
  # set all open corredors and the end and start as unvisited
  maze[maze == " " | maze == "E" | maze == "S"] <- "."
  
  # initialize the solution stack and start location
  loc = start
  visits <- rdeque()
  visits <- insert_back(visits, loc)
  
  # keep a stack to remember where each visit came from in the BFS
  camefrom <- rstack()
  
  step <- 1
  while(!empty(visits)) {
    loc <- peek_front(visits)
    visits <- without_front(visits)
  
    neighbors <- random_unvisited_neighbor(maze, loc, dist = 1, all = TRUE)
    for(neighbor in neighbors) {
      camefrom <- insert_top(camefrom, list(from = loc, to = neighbor))
      ## push neighbors on the queue and mark them visited with the timestep
      visits <- insert_back(visits, neighbor)
      maze[neighbor$row, neighbor$col] <- step
      step <- step + 1
    }
  }
  
  # set loc to the end, so we can track the camefrom path back to find the solution
  loc <- end
  while(any(loc != start)) {
    maze[loc$row, loc$col] <- as.numeric(maze[loc$row, loc$col]) * -1
    pathpart <- peek_top(camefrom)
    while(any(pathpart$to != loc)) {
      camefrom <- without_top(camefrom)
      pathpart <- peek_top(camefrom)
    }
    loc <- pathpart$from
  }
  maze[loc$row, loc$col] <- as.numeric(maze[loc$row, loc$col]) * -1
  return(maze)
}


maze <- create_ascii_maze(23, 55)
#plot_ascii_maze(maze, "maze_unsolved.pdf")

solved_dfs <- solve_ascii_maze_dfs(maze)
#plot_ascii_maze(solved_dfs, "maze_solved_dfs.pdf")

solved_bfs <- solve_ascii_maze_bfs(maze)
#plot_ascii_maze(solved_bfs, "maze_solved_bfs.pdf")

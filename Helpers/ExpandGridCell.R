#Precision based equality
equals <- function(val1, val2){
  return(abs(val1 - val2) < 0.00001)
} 


expandGridCell <- function(grid, cell){
  
  
  left <- grid[which(equals(grid$right, cell$right + 0.1) &
                       equals(grid$left, cell$left + 0.1) &
                       equals(grid$top, cell$top) &
                       equals(grid$bottom, cell$bottom)),]
  
  
  right <- grid[which(equals(grid$right, cell$right - 0.1) &
                        equals(grid$left, cell$left - 0.1) &
                        equals(grid$top, cell$top) &
                        equals(grid$bottom, cell$bottom)),]
  
  
  top <- grid[which(equals(grid$right, cell$right) &
                      equals(grid$left, cell$left) &
                      equals(grid$top, cell$top + 0.1) &
                      equals(grid$bottom, cell$bottom + 0.1)),]
  
  
  bottom <- grid[which(equals(grid$right, cell$right) &
                         equals(grid$left, cell$left) &
                         equals(grid$top, cell$top - 0.1) &
                         equals(grid$bottom, cell$bottom - 0.1)),]
  
  
  topleft <- grid[which(equals(grid$right, cell$right + 0.1) &
                          equals(grid$left, cell$left + 0.1) &
                          equals(grid$top, cell$top+0.1) &
                          equals(grid$bottom, cell$bottom+0.1)),]
  
  
  topright <- grid[which(equals(grid$right, cell$right - 0.1) &
                           equals(grid$left, cell$left - 0.1) &
                           equals(grid$top, cell$top+0.1) &
                           equals(grid$bottom, cell$bottom+0.1)),]
  
  
  bottomleft<- grid[which(equals(grid$right, cell$right + 0.1) &
                            equals(grid$left, cell$left + 0.1) &
                            equals(grid$top, cell$top - 0.1) &
                            equals(grid$bottom, cell$bottom - 0.1)),]
  
  
  bottomright <- grid[which(equals(grid$right, cell$right - 0.1) &
                              equals(grid$left, cell$left - 0.1) &
                              equals(grid$top, cell$top - 0.1) &
                              equals(grid$bottom, cell$bottom - 0.1)),]
  
  return(rbind(top, bottom, left, right, topleft, topright,
               bottomleft, bottomright))
  
}
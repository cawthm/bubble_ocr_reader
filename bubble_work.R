#### This is a sketch; it may need to be moved to its own project
## 

install.packages("magick")
library(magick)
library(tidyverse)
library(data.table)
library(tesseract)

# Note: ran 'arch -arm64 brew install imagemagick' from terminal to get some libraries

#### Example from ChatGPT

grid <- matrix(c(0, 0, 0, 0, 0,
                 0, 0, 1, 0, 0,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 0,
                 0, 0, 1, 0, 0,
                 0, 0, 0, 0, 0), 
               nrow = 6, byrow = TRUE)

grid

rowwise_add <- function(matrix) {
  result <- matrix
  for (i in 2:nrow(matrix)) {
    result[i, ] <- (matrix[i, ] + result[i - 1, ]) * matrix[i , ]
  }
  
  return(result)
}

colwise_add <- function(matrix) {
  result <- matrix
  ## sweep from left to right
  for (i in 2:ncol(matrix)) {
    result[, i] <- (matrix[ , i] + result[ ,i - 1]) * matrix[ , i]
  }
  result2 <- result
  ## sweep from right to left
  for (i in ncol(matrix):2) {
    result2[, i] <- (matrix[ , i] - result[ ,i - 1]) * matrix[ , i]
  }
  
}


rowwise_add(grid)

colwise_add(grid)

pmax(rowwise_add(grid), colwise_add(grid))

######################
#####################


work <- zzz[[1]][1, , ] |>
  apply(c(1, 2), function(x)
    strtoi(paste0("0x", x)))

work2 <- 1 - work / 255 # normalizes and then swaps 0s and 1s

# user  system elapsed 
# 50.827   1.088  52.101 

work2[25:30, 1000:1005]

unique(work2[1,])

image_summed <- pmax(colwise_add(work2), rowwise_add(work2))

image_col <- colwise_add(work2)

image_row <- rowwise_add(work2)

image_row[203,]

count(group_by(tibble(x = c(work2)),x))

sort(c(image_row), decreasing = T)

plot(sort(c(image_col), decreasing = T))


sum((c(image_row) > 40) *  (c(image_row) < 90))



# ### test how R vectorizes a matrix
matrix(1:6, nrow = 2)
c(matrix(1:6, nrow = 2))
matrix(1:6, ncol = 2)
c(matrix(1:6, ncol = 2))
t(matrix(1:6, ncol = 3))
c(t(matrix(1:6, ncol = 3)))

rowwise_add(grid)
colwise_add(grid)

rowwise_add(grid) * colwise_add(grid)

zzzz <- c(rowwise_add(work2) * colwise_add(work2))

### attempt to use rle() to 

x <- colwise_add(grid)

xx <- rle(c(grid))


xx

# this always works on columns because of how R orders matrices
centralizer <- function(length, value, sum_value = FALSE) {
  left_side <- length %/% 2 - 1
  middle <- rep(value * (sum_value * length + !sum_value), min(length, 2))
  right_side <- length %/% 2 - 1 + length %% 2 # integer division and then modulus
  
  c(rep(0, max(left_side,0)), middle, rep(0,right_side))
  
}
centralizer1 <- function(length, value, sum_value = FALSE) {
  left_side <- length %/% 2
  middle <- rep(value)
  right_side <- length %/% 2 -1# integer division and then modulus
  
  c(rep(0, max(left_side,0)), middle, rep(0,right_side))
  
}

centralizer1(5, 1)

unlist(map2(xx$lengths, xx$values, centralizer)) |> matrix(ncol = ncol(grid))
grid

xx <- rle(c(t(grid)))

## to do rows of grid = G, 
## t(G)
## convert to seq c(t(G))
## take rle of c(t(G))
## run centralizer against rle(t(G))
## unlist()
## re-matricize
## t()
grid
x <- t(matrix(unlist(map2(rle(c(t(grid)))$lengths, rle(c(t(grid)))$values, centralizer)), ncol = ncol(t(grid))))

y <- matrix(unlist(map2(rle(c(grid))$lengths, rle(c(grid))$values, centralizer)), ncol = ncol(grid))

x <- t(matrix(unlist(map2(rle(c(t(work2)))$lengths, rle(c(t(work2)))$values, centralizer)), ncol = ncol(t(work2))))

y <- matrix(unlist(map2(rle(c(work2))$lengths, rle(c(work2))$values, centralizer)), ncol = ncol(work2))

sum(c(x + y) == 2)


y <- matrix(1:24, ncol = 4)

which(y == 17)

find_coords <- function(linear_seq_length, ncol) {
  c( # row
    row = linear_seq_length %% 6,
    col = linear_seq_length %/% ncol + 1
  )
  
}

find_coords(17, 6)

y[5,3]

y[find_coords(17, 6)[1], find_coords(17, 6)[2]]


#### Aside: let's try loading targets
# there should be 90 + 16 + 49*5 +3 = 355

tt <- image_read("INPUT_image_files/bubble_template_targets.pdf")

ttt <- tt |> 
  image_scale("840x1020") |>
  image_quantize(colorspace = 'LinearGray', max = 2) |>
  image_median(radius = 3) |>
  #image_normalize() |>
  image_threshold(
    type = "white",
    threshold = "40%",
    channel = NULL
  ) #|>
#image_quantize(colorspace = 'gray', max = 2)

ttt

twork <- ttt[[1]][1,, ] |>
  apply(c(1, 2), function(x)
    strtoi(paste0("0x", x)))

twork2 <- 1 - t(twork) / 255 # normalizes and then swaps 0s and 1s

x <- matrix(unlist(map2(rle(c(twork2))$lengths, rle(c(twork2))$values, centralizer, sum_value = T)), ncol = ncol(twork2))

y <- t(matrix(unlist(map2(rle(c(t(twork2)))$lengths, rle(c(t(twork2)))$values, centralizer, sum_value = T)), ncol = ncol(t(twork2))))


write_csv(as_tibble(x*y), "~/Desktop/delete_me2.csv")

sum(c(x + y) >= 2)

summed_matrix <- x+y

timesed_matrix <- x*y


###### Sample matrices A and B (replace these with your actual data)
# A <- data.frame(name = c("A1", "A2", "A3"),
#                 x = c(1, 2, 3),
#                 y = c(1, 2, 3))
# 
# B <- data.frame(target = c("T1", "T2", "T3"),
#                 x = c(2, 3, 5),
#                 y = c(1, 3, 5))

bubble_ranges <- c(170:220, 370:430, 1400:1640)

bubble_ranges
A <- data.frame(target = paste0("Pt", "_", 1:length(which((summed_matrix) == 2))),
                x = (which((summed_matrix) == 2) %/% nrow(twork2)),
                y = (which((summed_matrix) == 2) %% nrow(twork2))) 


B <- data.table(target = paste0("Pt", "_", 1:length(which((timesed_matrix) %in% bubble_ranges))),
                x = (which((timesed_matrix) %in% bubble_ranges) %/% nrow(twork2)),
                y = (which((timesed_matrix) %in% bubble_ranges) %% nrow(twork2)),
                z = timesed_matrix[which((timesed_matrix) %in% bubble_ranges)]) 

B

# Function to calculate Euclidean distance
# euclidean_dist <- function(x1, y1, x2, y2) {
#   sqrt((x2 - x1)^2 + (y2 - y1)^2)
# }
# 
# # Find nearest neighbor for each point in A
# nearest_neighbors <- sapply(1:nrow(A), function(i) {
#   distances <- sapply(1:nrow(B), function(j) {
#     euclidean_dist(A[i, "x"], A[i, "y"], B[j, "x"], B[j, "y"])
#   })
#   #order(distances)[2]
#   B
#    #B[order(distances)[2],] #|> select(target = nn, x= nn_x, y = nn_y)
# })
# 
# distances <- map2_dbl(.x = B[1:5,]$x, 
#          .y = B[1:5,]$y, 
#          .f = euclidean_dist, 
#          x2 = B$x[2], 
#          y2 = B$y[2])
# distances
# 
# B[order(distances)[2],] |>
#   mutate(distance = distances[order(distances)[1 + TRUE]])
# 
# B[order(distances)[2],] |>
#   mutate(distance = sort(distances)[2])
# 
# 
# Find nearest neighbor for each point in A
nearest_neighbors <- function(data_frame_target, data_frame_scan, second = FALSE){
  
  neighbor_name <- paste0("neigh", if_else(0 == TRUE,"_second", "_first"))
  neighbors <- tibble(neighbor_name = character(), 
                      x_neigh = numeric(), 
                      y_neigh = numeric(),
                      distance = numeric())
  
  for (i in 1:nrow(data_frame_target)) { 
    
    distances <- map2_dbl(.x = data_frame_scan$x, 
                          .y = data_frame_scan$y, 
                          .f = euclidean_dist, 
                          x2 = data_frame_target$x[i], 
                          y2 = data_frame_target$y[i])
    
    new <- tibble(neighbor_name = data_frame_scan[order(distances)[1+second],]$target,
                  x_neigh = data_frame_scan[order(distances)[1+second],]$x,
                  y_neigh = data_frame_scan[order(distances)[1+second],]$y,
                  z_neigh = data_frame_scan[order(distances)[1+second],]$z,
                  distance = sort(distances)[1+second])
    
    neighbors <- bind_rows(neighbors, new)
    
    #print(distances)
  }
  
  #data_frame_target
  #neighbors
  bind_cols(data_frame_target, neighbors)
}

B[1,]

excel_address <- function(x, y) {
  
  paste0(LETTERS[x %/% 26], LETTERS[(x %% 26)+1], y+1)
}

library(data.table)
ww <- nearest_neighbors(B, B) |> arrange(distance) |> 
  mutate(cell = excel_address(x,y)) |>
  as.data.table()

#excel_address(97, 70)

# Combine results with 



yy <- ww
radius <- 10
exclude_list <- yy[distance > radius,]
dedupe_list <- yy[distance <= radius,]
holding_list <- exclude_list
while(nrow(exclude_list) < nrow(yy)){
  #i = 1
  holding_list <- holding_list[is.na(x),]
  #batch$target <- NULL
  while(nrow(dedupe_list) > 0) {
    batch <- dedupe_list[neighbor_name %in% c(dedupe_list$target[1],dedupe_list$neighbor_name[1]),]
    holding_list <- rbind(holding_list, batch[z == max(z),][1,]) |> unique()
    dedupe_list <- dedupe_list[!target %in% batch$target,]
    #i = i+1
  }
  yy <- rbind(exclude_list, holding_list)
  yy <- nearest_neighbors(yy[,1:4], yy[,1:4]) |> arrange(distance) %>% 
    mutate(cell = excel_address(x,y)) |>
    as.data.table()
  dedupe_list <- yy[distance <= radius,]
  cat(nrow(dedupe_list))
  exclude_list <- yy[distance > radius,]
  cat(nrow(exclude_list))
}

TARGETS <- yy

49*5+90+16+3




TARGETS <- TARGETS |>
  mutate(mark = "xx_ROW_COL")

########################

## We have three regions




### UPPER LEFT ####
upper_left <- rle(as.integer(rowSums(timesed_matrix[1:186,1:466]) >1))

## just take the odd ones
ul_indexes <- cumsum(upper_left$lengths)[(1:length(cumsum(upper_left$lengths))-1) %% 2 != 0]+1

names(ul_indexes) <- c(NA, letters[23:26])

ul_indexes

upper_left_c <- rle(as.integer(colSums(timesed_matrix[1:186,1:466]) >1))

## just take the odd ones
ul_indexes_c <- cumsum(upper_left_c$lengths)[(1:length(cumsum(upper_left_c$lengths))-1) %% 2 != 0]+1

ul_indexes_c

names(ul_indexes_c) <- c(NA, 1:4)

ul_indexes_c

### UPPER right ##############
## rowSums tell you about y, colSums about x
upper_right <- rle(as.integer(rowSums(timesed_matrix[1:273,466:ncol(timesed_matrix)]) >1))

## just take the odd ones
ur_indexes <- cumsum(upper_right$lengths)[(1:length(cumsum(upper_right$lengths))-1) %% 2 != 0]+1

ur_indexes

names(ur_indexes) <- c(0:9)

ur_indexes

upper_right_c <- rle(as.integer(colSums(timesed_matrix[1:273,466:ncol(timesed_matrix)]) >1))

## just take the odd ones
ur_indexes_c <- cumsum(upper_right_c$lengths)[(1:length(cumsum(upper_right_c$lengths))-1) %% 2 != 0]+1

ur_indexes_c

names(ur_indexes_c) <- c(1:9)




### Bottom ##############
## rowSums tell you about y, colSums about x
lower <- rle(as.integer(rowSums(timesed_matrix[274:nrow(timesed_matrix),]) >1))

## just take the odd ones
lower_indexes <- cumsum(lower$lengths)[(1:length(cumsum(lower$lengths))-1) %% 2 != 0]+1

length(lower_indexes)

names(lower_indexes) <- c(paste0(c(1:24), "_",c(25:48)), NA)

lower_indexes2 <- c(rep(lower_indexes[1:24],2), lower_indexes[25]) |> set_names(1:48, NA)

lower_c <- rle(as.integer(colSums(timesed_matrix[274:nrow(timesed_matrix),]) >1))

## just take the odd ones
lower_indexes_c <- cumsum(lower_c$lengths)[(1:length(cumsum(lower_c$lengths))-1) %% 2 != 0]+1

length(lower_indexes_c)

names(lower_indexes_c) <- c(NA, rep(LETTERS[1:5],2), NA)

lower_indexes_c


#marks <- 
########## now we much combine each set
target_labels <-
  bind_rows(
    bind_cols(
      expand_grid(x = ul_indexes_c, y = ul_indexes),
      expand_grid(x = names(ul_indexes_c), y = names(ul_indexes))
    ) |>
      set_names(c("x", "y", "ans", "posn")) |>
      mutate(target = paste0("TV_", ans, "_", posn)) |>
      filter(!(is.na(ans) &
                 !is.na(posn)) & !(is.na(posn) & !is.na(ans))) |>
      mutate(x = x - 2,
             cell = excel_address(x, y)),
    
    bind_cols(
      expand_grid(x = ur_indexes_c, y = ur_indexes),
      expand_grid(x = names(ur_indexes_c), y = names(ur_indexes))
    ) |>
      set_names(c("x", "y", "ans", "posn")) |>
      mutate(target = paste0("ID_", ans, "_", posn)) |>
      filter(!(is.na(ans) &
                 !is.na(posn)) & !(is.na(posn) & !is.na(ans))) |>
      mutate(x = x + 465,
             cell = excel_address(x, y)),
    
    bind_cols(
      expand_grid(x = lower_indexes_c, y = lower_indexes2),
      expand_grid(x = names(lower_indexes_c), y = names(lower_indexes2))
    ) |>
      set_names(c("x", "y", "ans", "posn")) |>
      mutate(target = paste0("QU_", ans, "_", posn)) |>
      filter(!(is.na(ans) &
                 !is.na(posn)) & !(is.na(posn) & !is.na(ans))) |>
      mutate(
        x = x - 2,
        y = y + 274,
        cell = excel_address(x, y)
      )
  )

write_csv(mutate(nearest_neighbors(TARGETS[,1:3], target_labels),orig_cell = excel_address(x, y), label_cell = excel_address(x_neigh, y_neigh)), "~/Desktop/compare.csv")

nearest_neighbors(TARGETS[,1:3], target_labels)

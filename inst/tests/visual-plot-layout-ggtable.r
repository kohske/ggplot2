p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + theme_grey(base_size = i*5))

# automatic layout as square as possible
ggtable(p[[1]], p[[2]], p[[3]])
ggtable(plots = p[1:3])
ggtable(plots = p[1:7] ,byrow = F)
ggtable(plots = p[1:10])

# sepcify dimenstion
ggtable(plots = p[1:5], dim = c(2, 3))
ggtable(plots = p[1:5], dim = c(2, 3), byrow = F)
ggtable(plots = p[1:5], nrow = 2)
ggtable(plots = p[1:5], ncol = 2)

# layout as matrix
m <- matrix(
  c(1, 2, 3, 
    4, 5, 6), 2, byrow = T)
lay <- gglayout(m)
ggtable(plots = p[1:6], layout = lay)

# with blank space
m <- matrix(
  c(1, 2, 3,
    4, 0, 5), 2, byrow = T)
lay <- gglayout(m)
ggtable(plots = p[1:5], layout = lay)

# layout as list of col/row list
lay <- gglayout(row = list(1, 2, 3, 1, 2, 3), col = list(1, 1, 1, 2, 2, 2))
ggtable(plots = p[1:6], layout = lay)

# combine scatter plot and density plot
df <- data.frame(x = rnorm(100), y = rnorm(100))
p1 <- ggplot(df, aes(x, y)) + geom_point()
p2 <- ggplot(df, aes(x)) + geom_density()
p3 <- ggplot(df, aes(y)) + geom_density() + coord_flip()

ggtable(p1, p2, p3, layout = gglayout(matrix(c(2, 1, 0, 3), 2)))

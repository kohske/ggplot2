# list of plot
p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))

# automatic layout as square as possible
ggarrange(p[[1]], p[[2]], p[[3]])
ggarrange(plots = p[1:3])
ggarrange(plots = p[1:7])
ggarrange(plots = p[1:10])
ggarrange(plots = p[1:3] ,byrow = F)
ggarrange(plots = p[1:7] ,byrow = F)
ggarrange(plots = p[1:10] ,byrow = F)

# sepcify dimenstion
ggarrange(plots = p[1:5], dim = c(2, 3))
ggarrange(plots = p[1:5], dim = c(2, 3), byrow = F)
ggarrange(plots = p[1:5], dim = c(3, 2))
ggarrange(plots = p[1:5], dim = c(3, 2), byrow = F)

ggarrange(plots = p[1:5], nrow = 2)
ggarrange(plots = p[1:5], nrow = 2, byrow = F)
ggarrange(plots = p[1:5], nrow = 3)
ggarrange(plots = p[1:5], nrow = 3, byrow = F)
ggarrange(plots = p[1:5], ncol = 3)
ggarrange(plots = p[1:5], ncol = 3, byrow = F)
ggarrange(plots = p[1:5], ncol = 2)
ggarrange(plots = p[1:5], ncol = 2, byrow = F)

# layout as matrix
m <- matrix(
  c(1, 1, 1, 
    2, 2, 3, 
    4, 5, 5, 
    6, 5, 5), 4, byrow = T)
lay <- gglayout(m)
lay
ggarrange(plots = p[1:6], layout = lay)

m <- matrix(
  c(1, 1, 1, 1,
    2, 0, 0, 3,
    2, 4, 4, 3), 3, byrow = T)
lay <- gglayout(m)
lay
ggarrange(plots = p[1:4], layout = lay)


# layout as list of col/row list

lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 1))
lay
ggarrange(plots = p[1:6], layout = lay)

lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 3))
lay
ggarrange(plots = p[1:6], layout = lay)

# width/height
ggarrange(plots = p[1:6], widths = c(1, 1.5, 2), heights = c(1, 2))

m <- matrix(
  c(1, 2, 3, 
    4, 5, 6), 2, byrow = T)
lay <- gglayout(m, widths = c(1, 1.5, 2), heights = seq(1, 2))
ggarrange(plots = p[1:6], layout = lay)


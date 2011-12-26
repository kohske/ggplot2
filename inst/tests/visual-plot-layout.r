
# automatic layout as square as possible
p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))

layout.ggplot(p[[1]], p[[2]], p[[3]])
do.call("layout.ggplot", p[1:7])
do.call("layout.ggplot", p[1:10])

# sepcify dimenstion
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3), byrow = F)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(3, 2))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(3, 2), byrow = F)

layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 2)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 2, byrow = F)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3, byrow = F)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 3)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 3, byrow = F)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 2)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 2, byrow = F)

# specify layout
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2)))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3, ncol = 4,
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))

# width/height
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)),
  widths = c(2, 1, 1))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)),
  heights = c(2, 1))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)),
  widths = c(2, 1, 1), heights = c(2, 1))
layout.ggplot(p[[1]], p[[2]], p[[3]],
  nrow = 1, widths = c(1, 1.5, 2.5))

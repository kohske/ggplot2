# guide axis
# cart
qplot(1:3, 4:6) + 
  scale_x_continuous(guide = list(
    guide_axis(title = "x-bottom"),
    guide_axis(title = "x-top (x2)", position = "top", trans = function(x) x * 2))) +
  scale_y_continuous(guide = list(
    guide_axis(title = "y-left"),
    guide_axis(title = "y-right (x2)", position = "right", trans = function(x) x * 2)))


# polar (theta = x, r = y)
qplot(1:9, 11:19, geom = "line") +
  scale_y_continuous(guide = list(
    guide_axis(title = "r-left"),
    guide_axis(title = "r-top (x2)", position = "top", trans = function(x) x * 2),
    guide_axis(title = "r-right (x3)", position = "right", trans = function(x) x * 3),
    guide_axis(title = "r-bottom (x4)", position = "bottom", trans = function(x) x * 4)
    )) +
  coord_polar()

# polar (theta = y, r = x)
qplot(1:9, 11:19, geom = "line") +
  scale_x_continuous(guide = list(
    guide_axis(title = "r-left"),
    guide_axis(title = "r-top (x2)", position = "top", trans = function(x) x * 2),
    guide_axis(title = "r-right (x3)", position = "right", trans = function(x) x * 3),
    guide_axis(title = "r-bottom (x4)", position = "bottom", trans = function(x) x * 4)
    )) +
  coord_polar(theta = "y")

# flip
qplot(1:3, 4:6) + 
  scale_y_continuous(guide = list(
    guide_axis(title = "y-bottom"),
    guide_axis(title = "y-top (x2)", position = "top", trans = function(x) x * 2))) +
  scale_x_continuous(guide = list(
    guide_axis(title = "x-left"),
    guide_axis(title = "x-right (x2)", position = "right", trans = function(x) x * 2))) +
  coord_flip()


# guide range
x <- seq(0, 20, 0.1)
y <- sin(x * pi)

qplot(x, y, geom = "line", colour = I("skyblue")) + 
  scale_x_continuous(guide = list(
    guide_range(length = 0.5, unit = "a", location = c(0.5, 0)),
    guide_range(length = 1, unit = "b", location = c(5, 0), space = "data"),
    guide_range(position = "float", length = 0.5, label = "label", location = c(0.7, 0.7)),
    guide_range(position = "float", length = 3, label = "float", location = c(5, 0.5), space = "data"),
    guide_axis(position = "top"))) +
  scale_y_continuous(guide = list(
    guide_range(length = 0.2, unit = "a", location = c(0, 0.5)),
    guide_range(length = 0.3, unit = "b", location = c(0, 0.5), space = "data"),
    guide_range(position = "float", length = 0.1, label = "2 mm", location = c(0.3, 0.3), label_pos = "r"),
    guide_range(position = "float", length = 0.2, label = "1 mm", location = c(15, -0.5), space = "data"),
    guide_axis(position = "right")))

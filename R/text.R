text = function(x, ...) UseMethod("text")

text.sfc = function(x, ...){
  xy = sf::st_coordinates(x)
  text(x = xy[,1], y = xy[,2], ...)
}
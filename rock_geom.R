library(ggplot2)
library(grid)

# path = "C:\\Users\\Craig\\Desktop\\rocks"
# filenames = list.files(path, pattern="*.png", full.names=TRUE, recursive=TRUE)
# images_30 = lapply(filenames, readPNG)
# 
load("images.RData")

.rocklist = list()
for (i in 1:360)
  .rocklist[[paste0('r',i)]] = images[[i]]


rockGrob <- function(x, y, image, size=1, alpha=1){
  grob(x=x, y=y, image=image, size=size, cl = "rock")
}

drawDetails.rock <- function(x, recording=FALSE){
  scale = .01
  for(ii in seq_along(x$image)){
    image = .rocklist[[x$image[[ii]]]]
    sizes = dim(image)
    xsize = sizes[2]
    ysize = sizes[1]
    grid.raster(x$x[ii], x$y[ii], 
                width = x$size[ii]*unit(xsize*scale,"mm"), height = x$size[ii]*unit(ysize*scale,"mm"),
                image = image, interpolate=FALSE)
  }
}


Geomrock <- ggproto("Geomrock", Geom,
                    required_aes = c("x", "y", "image"),
                    default_aes = aes(size = 40),
                    
                    draw_key = function (data, params, size) 
                    {
                      rockGrob(0.5,0.5, image=data$image,  size=data$size)
                    },
                    
                    draw_group = function(data, panel_scales, coord) {
                      coords <- coord$transform(data, panel_scales)     
                      rockGrob(coords$x, coords$y, coords$image, coords$size)
                    }
)

geom_rock <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  layer(
    geom = Geomrock, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


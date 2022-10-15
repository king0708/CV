# Decomp image

# load pacakges ----------
library(magick)
library(tidyverse)
library(gganimate)
# read in image

birch <- image_read("birch-leaf-decomp.png") 

# Function to generate scaling dimensions for each mass remaining
img_red <- function(x, y, width, height){
     # x is a number
     # y is a vector of numbers
     # width is starting number of pixels for image width
     # height is the starting number of pixels for image height
     z <- length(y)
     #out <- matrix(NA, nrow = z, ncol = 1)
     out   <- vector(mode = "numeric")
     out.x <- vector(mode = "numeric")
     out.y <- vector(mode = "numeric")
     a     <- vector(mode = "numeric")
     b     <- vector(mode = "numeric")

     # for loop to calculate pixel width and height for each image frame
     for(i in 1:z){
          red <- y[i]
          out <- c(out, x*red)
          a <- (width/height)*(sqrt(out*(height/width))) # new width pixels
          b <- sqrt((height*out)/width) # new height pixels
          # out[i, 1] <- x*red
          out.x <- (width - a)/2 # offset for x & border
          out.y <- (height - b)/2 # offset for y & border 
          
     }
     out <- cbind(out, out.x, out.y, y, a, b)
     return(out)
}

img_areas <- img_red(64989, seq(from = 0.05, to = 1, by = 0.05), width = 261, height = 249)

image_crop(birch, geometry_area(234.91, 224.15, 10, 10))

image_crop(birch, geometry_area(img_areas[10,5], 
                                img_areas[10,6], 
                                img_areas[10,2], 
                                img_areas[10,3]))



# create multiple images for gif
for(j in 1:20){
     wid   <- img_areas[j, 5]
     ht    <- img_areas[j, 6]
     off.x <- img_areas[j, 2]
     off.y <- img_areas[j, 3]
     prop.rem <- as.character(img_areas[j,4])
     bord.geom <- paste(off.x, "x", off.y, sep = "")
     ord <- 21 - j
     
     image_write(
          image = image_border(
               image_crop(birch, geometry_area(wid, ht, off.x, off.y)), 
               color = "#FFFFFF", geometry = bord.geom),
          path = paste("images/", ord, "birch_", prop.rem, ".png", sep = ""), 
          format = "png"
          )
     
}


# create multiple images for gif
for(j in 1:20){
     wid   <- img_areas[j, 5]
     ht    <- img_areas[j, 6]
     off.x <- img_areas[j, 2]
     off.y <- img_areas[j, 3]
     prop.rem <- as.character(img_areas[j,4])
     bord.geom <- paste(off.x, "x", off.y, sep = "")
     ord <- 31 - j
     
     image_crop(birch, geometry_area(wid, ht, off.x, off.y)) %>%
          image_border(color = "#FFFFFF", geometry = bord.geom) %>%
          image_transparent("white") %>%
          image_write(path = paste("images/", ord, "birch_", prop.rem, ".png", sep = ""), 
                      format = "png")
     
}

## list file names and read in
imgs <- list.files(path = "images/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2, loop = 0)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "images/birch-decomp.gif")

image_read("images/01birch_1.png") %>% 
     image_transparent(color = "white")


# Create animation of decomposition curves 

t1<-seq(0,5, length.out=1000)

weibull <- function(x, alpha, beta){
     # x is time in years
     # alpha is shape parameter
     # beta is scale parameter
     prop.rem <- exp(-(x/beta)^alpha)
     return(prop.rem)
}

birch.dat <- data.frame(amb.amb = NA, amb.warmed = NA, h2.amb = NA, h2.warmed = NA)

param.dat <- data.frame(
     trt = c("amb-amb", "amb-warmed", "h2-amb", "h2-warmed"),
     alpha = c(0.833, 1.147, 0.945, 1.256),
     beta = c(2.414, 1.251, 2.809, 1.678)
)


for(j in 1:length(param.dat$trt)){
     alpha <- param.dat$alpha[j]
     beta <- param.dat$beta[j]
     
          for(i in 1:length(t1)){
               birch.dat[i, j] <- weibull(t1[i], alpha, beta)
          }
     }

birch.dat$year <- t1

birch.dat %>%
     gather(key = "trt", value = "prop.rem", -year) %>%
     separate(trt, into = c("env", "source"), sep = "\\.") -> birch.dat

ggplot() +
     xlim(0,10)+
     ylim(0,1)+
     geom_function(fun = weibull, args = list(alpha = 0.945, beta = 2.809), color = "black", size = 1) + 
     geom_function(fun = weibull, args = list(alpha = 1.256, beta = 1.678), color = "red", size = 1, linetype = 2)+
     theme_bw()

theme.gif <- theme_minimal()+
     theme(legend.position = "bottom", 
                    axis.text = element_text(size = 12),
                    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 14))

# birch.dat$prop.rem.end <- c(birch.dat$prop.rem[2:4000], birch.dat$prop.rem[4000])
# birch.dat$time.end <- c(birch.dat$year[2:4000], 5.005)

# try ggplotly to make smoother?
p.curves <- ggplot(birch.dat, aes(x = year, y = prop.rem, color = env, linetype = source)) +
     # geom_curve(aes(xend = time.end, yend = prop.rem.end), size = 1.5) + 
     geom_line(size = 1)+
     geom_point(aes(shape = source))+
     # geom_smooth()+
     theme.gif +
     scale_shape_discrete(guide = "none")+
     scale_color_manual(name = "Heat Treat.", values = c("black", "red"), labels = c("Amb", "Warm")) +
     scale_linetype_manual(name = "Litter \nSource", values = c(1, 3)) + 
     ylab("Prop. Mass Remaining") + 
     xlab("Years") +
     theme(legend.position = "bottom") +
     transition_reveal(year)


curve.gif <- animate(p.curves, nframes = 200, fps = 10)

anim_save("birch_decomp_curves.gif")

birch.dat %>% mutate(prop.lost = 1-prop.rem) -> birch.dat

p.race <- ggplot(birch.dat, aes(x = interaction(env, source), y = prop.lost, color = env, 
                      shape = source)) +
     geom_point(size = 3) +
     theme.gif +
     theme(panel.grid.major = element_blank())+
     scale_color_manual(name = "Heat Treat.", values = c("black", "red"), labels = c("amb", "warm")) +
     scale_linetype_manual(name = "Litter \nSource", values = c(1,2)) + 
     ylab("Prop. Mass Loss") + 
     scale_y_reverse() + 
     scale_x_discrete(name = "Treatment", 
                      labels = c("Amb. Temp.\nAmb. Litter", 
                                 "Warm Temp.\nAmb. Litter",
                                 "Amb. Temp.\nWarm Litter", 
                                 "Warm Temp.\nWarm Litter"))+
     scale_shape_discrete(name = "Litter \nSource") +
     transition_reveal(prop.lost) + 
     transition_components(time = year)+
     shadow_wake(wake_length = 0.1)

race.gif <- animate(p.race, nframes = 200, fps = 10)

# combine into one file to show side by side
# modified from : https://towardsdatascience.com/how-to-combine-animated-plots-in-r-734c6c952315


new_gif <- image_append(c(curve.gif[1], race.gif[1]), stack = FALSE)
for(i in 2:200){
     combined <- image_append(c(curve.gif[i], race.gif[i]), stack = FALSE)
     new_gif <- c(new_gif, combined)
}

image_write(new_gif, path = "curve_race_birch3.gif", quality = 85)

# TODO Make race animation more like a race - e.g. with fewer lines, 
# things that say "start", "halfway" and "finish" 
# TODO mark each animation with stops to highlight when specific points happen
# for ex., when warmed-warmed litter crosses amb. litter
# or when each treatment reaches 50% mass loss
# make prettier - e.g. better fonts, more relevant shapes (e.g. a leaf?)
# make same size - adjust distance from axis for plot on left
# TODO do Queru or poptr? 

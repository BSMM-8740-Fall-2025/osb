
#Loading required libraries

library(magrittr)
library(magick)
## Linking to ImageMagick 6.9.9.14
## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
## Disabled features: fontconfig, x11
library(dplyr)
##
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
##
##     filter, lag
## The following objects are masked from 'package:base':
##
##     intersect, setdiff, setequal, union
library(hexSticker)


img <- magick::image_read("images/logo.png")
img %>%
  image_convert("png") %>%
  image_resize("1080 x 200")%>%
  image_fill(color="#062047", point="+45") %>%
  image_annotate("d?ta", size=38, location = "+47+58", color="black") -> res

res

# wrap in plot to preview ie plot(sticker(...))
final_res <- hexSticker::sticker(mg, package="BSMM 8740", p_size=30,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.1,
                   s_height = 14,
                   filename="mde_icon_2.png",h_fill="#062047",h_color = "#062047")


img <- magick::image_read("images/background-bikeshare.png")

img %<>%
  image_resize("1080 x 200") %>%
  image_fill(color="#062047", point="+45")

img <- magick::image_read("images/logo.png")

final_res <- hexSticker::sticker(img, package="BSMM-8740", p_size=20, p_color = "#FFCE00",
                                 p_y = 1.4,
                                 s_x=1, s_y=0.6, s_width=0.8,
                                 s_height = 14,
                                 filename="mde_icon_2.png",h_fill="#005596",h_color = "#FFCE00")

plot(final_res)

"#FFCE00"
img <- magick::image_read("images/single_layer_nn.png")


# FINAL =========================
img <- magick::image_read("images/myplot.png")

final_res <- hexSticker::sticker(img, package="BSMM-8740", p_size=20, p_color = "#FFCE00",
                                 p_y = 1.45,
                                 s_x=1.05, s_y=0.725, s_width=1.25,
                                 s_height = 4,
                                 ,h_fill="#005596",h_color = "#FFCE00")

plot(final_res)

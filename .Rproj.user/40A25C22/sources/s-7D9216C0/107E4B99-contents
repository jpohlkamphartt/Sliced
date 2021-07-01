library("ggplot2")
library("ggthemes")
library(ggridges)
library(extrafont)

Sliced_theme<- function () { 
  ggpubr::theme_pubclean(base_size=15) %+replace% 
    theme(strip.background = element_blank(), 
          legend.position = "right",
          legend.key = element_blank())
  # 
  # theme_bw(base_size=13, base_family="Avenir") %+replace% 
  #   theme(
  #     panel.background  = element_blank(),
  #     plot.background = element_rect(fill="gray96", colour=NA), 
  #     legend.background = element_rect(fill="transparent", colour=NA),
  #     legend.key = element_rect(fill="transparent", colour=NA)
  #   )
}


Sliced_Divergent_Palette <- c("#4345FF","#774BFF","#A554FA","#CB5CF2","#E864EA","#E16DC5","#D975A8","#E16DA8","#EA64A8","#F25CA9","#FA54A9","#FF4BA9","#FF43A9")
Sliced_Qualitative_Palette <- c("#4aff92", "#0d2eb5", "#f05c72", "#fde18e", '#8ac6e8', "#eeb1b7", "#8afbfb", "#ffac22", "#927290", "#1e1920", "#f32bf6")
Sliced_Sequential_Palette <- c("#871779","#88219B","#542CAF","#3851C3","#479FCF","#57D9C9","#68E199","#72EC69","#B6F76B","#FFFE6F","#FFC074","#FF817A","#FF82BF")

slice_palettes<-list(
  Divergent=Sliced_Divergent_Palette,
  Qualitative=Sliced_Qualitative_Palette,
  Sequential=Sliced_Sequential_Palette
)

slice_pal<-function (palette = "Qualitative", reverse = FALSE, ...) 
{
  pal <- slice_palettes[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  grDevices::colorRampPalette(pal,interpolate = "spline",space = "Lab", ...)
}

scale_color_sliced<-function (palette = "Qualitative", discrete = TRUE, reverse = FALSE, 
                              ...) 
{
  pal <- slice_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("slice_", palette), 
                            palette = pal, ...)
  }
  else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_sliced<-function (palette = "Qualitative", discrete = TRUE, reverse = FALSE, 
                             ...) 
{
  pal <- slice_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("slice_", palette), 
                            palette = pal, ...)
  }
  else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

theme_set(Sliced_theme())

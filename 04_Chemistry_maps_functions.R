
## Plot functions ----

summ_variable <- function(param, data = df_chem){
  x <- as.data.frame(data)[,param]
  # For setting breaks
  print(summary(x))
  }

# The old one using FILL and pch = 21 (unless yearshape = TRUE)
# The new one (below) using COLOR and pch = 16 (unless yearshape = TRUE)
plot_map_abs_OLD <- function(param, breaks, log = TRUE, direction = 1, data = df_chem, yearshape = FALSE){
  legendtitle <- sub("-", "\n", param)
  if (yearshape){
    df <- data[,c("Lengdegrad", "Breddegrad", "Year", param)]
    df$Value <- as.data.frame(df)[,4]
  } else {
    df <- data[,c("Lengdegrad", "Breddegrad", param)]  
    df$Value <- as.data.frame(df)[,3]
  }
  
  if (direction == 1){
    df <- df %>% arrange(Value)
  } else {
    df <- df %>% arrange(desc(Value))
  }
  
  # For setting breaks
  # print(summary(df$Value))
  
  if (yearshape){
    gg <- ggplot(df, aes(Lengdegrad, Breddegrad, color = Value, shape = factor(Year))) + 
      annotation_map(map_norway, fill = "grey60") +
      geom_point(size = 3) + 
      scale_shape_discrete("År")
  } else {
    gg <- ggplot(df, aes(Lengdegrad, Breddegrad, fill = Value)) + 
      annotation_map(map_norway, fill = "grey60") +
      geom_point(pch = 21, size = 3)
  }
  if (!yearshape & log){
    gg <- gg + scale_fill_viridis(legendtitle, trans = "log", breaks = breaks, labels = breaks, direction = direction)
  } else if (!yearshape & !log)  {
    gg <- gg + scale_fill_viridis(legendtitle, breaks = breaks, labels = breaks, direction = direction)
  } else if (yearshape & log){
      gg <- gg + scale_color_viridis(legendtitle, trans = "log", breaks = breaks, labels = breaks, direction = direction)
  } else if (yearshape & !log)  {
    gg <- gg + scale_color_viridis(legendtitle, breaks = breaks, labels = breaks, direction = direction)
  }
  gg <- gg +
    coord_map("lambert", parameters = c(64, 12), ylim = c(58,71.5)) +
    theme_bw() + 
    theme(legend.position = c(.95, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          axis.title = element_blank())
  gg + theme(plot.margin = unit(c(0,1,0,1), "cm"))
}




plot_map_abs <- function(param, breaks, log = TRUE, direction = 1, data = df_chem, yearshape = FALSE, 
                         colorscale = "magma"){
  legendtitle <- sub("-", "\n", param)
  if (yearshape){
    df <- data[,c("Lengdegrad", "Breddegrad", "Year", param)]
  } else {
    df <- data[,c("Lengdegrad", "Breddegrad", param)]  
  }
  
  df$Value <- as.data.frame(df)[,param]
  if (direction == 1){
    df <- df %>% arrange(Value)
  } else {
    df <- df %>% arrange(desc(Value))
  }
  
  # For setting breaks
  # print(summary(df$Value))
  
  if (yearshape){
    gg <- ggplot(df, aes(Lengdegrad, Breddegrad, color = Value, shape = factor(Year))) + 
      annotation_map(map_norway, fill = "grey60") +
      geom_point(size = 3) + 
      scale_shape_discrete("År")
  } else {
    gg <- ggplot(df, aes(Lengdegrad, Breddegrad, color = Value)) + 
      annotation_map(map_norway, fill = "grey60") +
      geom_point(pch = 16, size = 3)
  }
  if (log){
    gg <- gg + scale_color_viridis(legendtitle, trans = "log", 
                                   breaks = breaks, labels = breaks, direction = direction, option = colorscale)
  } else  {
    gg <- gg + scale_color_viridis(legendtitle, 
                                   breaks = breaks, labels = breaks, direction = direction, option = colorscale)
  }
  gg <- gg +
    coord_map("lambert", parameters = c(64, 12), ylim = c(58,71.5)) +
    theme_bw() + 
    theme(legend.position = c(.95, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          axis.title = element_blank())
  gg + theme(plot.margin = unit(c(0,1,0,1), "cm")) +
    guides(shape = guide_legend(order = 2))
}

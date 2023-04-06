doughnut_plot <- function(df){
  library(dplyr)
  # Hole size
  hsize <- 7
  fraction <- df$value /sum(df$value)
  
  
  df <- df %>%
    mutate(x = hsize,
           csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  
  ggplot(df, aes(x = hsize, y = value, fill = Anlage)) +
    geom_col(color = "black", size=1) +
    scale_fill_brewer(palette = "YlGnBu") + # change fill to gold color palette
    coord_polar(theta = "y") +
    
    geom_label_repel(aes(label = paste(percent(fraction)), 
    ),
    position = position_stack(vjust=0.5),
    inherit.aes = TRUE,
    show.legend=FALSE,
    box.padding = 0,
    size=5, 
    color="gold3"  ) +
    guides(fill = guide_legend(title = "Anlage", title.position = "top"))+
    annotate("text", x = 0, y = 0, size = 20, color="navyblue", label = paste(abbreviate2(sum(df$value)), "CHF"))+
    theme_void()
}

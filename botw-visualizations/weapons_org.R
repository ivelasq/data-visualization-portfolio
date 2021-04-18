

fviz_nbclust(w_distdat, FUN = hcut, method = "wss")
fviz_nbclust(w_distdat, FUN = hcut, method = "silhouette")
library(factoextra)

# attempt 2 ---------------------------------------------------------------


createAngleHJustCols <- function(labeldf) {        
  nn <- length(labeldf$y)
  halfn <- floor(nn/2)
  firsthalf <- rev(90 + seq(0,360, length.out = nn))
  secondhalf <- rev(-90 + seq(0,360, length.out = nn))
  angle <- numeric(nn)
  angle[1:halfn] <- firsthalf[1:halfn]
  angle[(halfn+1):nn] <- secondhalf[(halfn+1):nn]
  
  hjust <- numeric(nn)
  hjust[1:halfn] <- 0
  hjust[(halfn+1):nn] <- 1
  
  return(list(angle = angle, hjust = hjust))
}

library(dendextend)
library(factoextra)

dend <- as.dendrogram(clusters_w)

ggd1 <- ggplot(dend %>%
                 set('branches_k_color', k = 10) %>%
                 set('branches_lwd', 1) %>%
                 set('labels_colors', k = 10) %>%
                 set('labels_cex', 1), 
               theme = theme_minimal(),
               horiz = TRUE)
ggd1 <- ggd1 + theme(panel.grid.major = element_blank(),
                     axis.text = element_blank(),
                     axis.title = element_blank())
ggd1 <- ggd1 + ylim(max(get_branches_heights(dend)), -3)
ggd1
gdend <- dendextend::as.ggdend(dend %>%
                                 set('branches_k_color', k = 10 %>%
                                 set('branches_lwd', 0.6) %>%
                                 set('labels_colors', k = 10) %>%
                                 set('labels_cex', 0.6))

gdend$labels$angle <- ifelse(horiz, 0, 90)
gdend$labels$hjust <- 0
gdend$labels$vjust <- 0.5

# if polar, change the angle and hjust so that the labels rotate
if(polarplot) {
  newvalues <- createAngleHJustCols(gdend$labels)
  gdend$labels$angle <- newvalues[['angle']]
  gdend$labels$hjust <- newvalues[['hjust']]
}

ggresult <- newggplot.ggdend(gdend, horiz = TRUE, offset_labels = -2) 
ggresult <- ggresult + ggtitle(plottitle)
ggresult <- ggresult + theme(plot.margin = margin(c(2,2,2,2),
                                                  axis.text = element_blank(),
                                                  plot.title = element_text(margin = margin(10,2,2,2)))
                             ggresult <- ggresult + ylim(max(get_branches_heights(dend)), -5)

# old ---------------------------------------------------------------------

# weapons_tab %>% 
#   ggplot(aes(x = 1, y = `AP*DUR`, color = Type, size = `AP*DUR` )) +
#   geom_quasirandom(bandwidth = .2) +
#   coord_flip()

# bubble chart  -----------------------------------------------------------

# packing <- circleProgressiveLayout(weapons_tab$`AP*DUR`, sizetype = 'area')
# dat <- bind_cols(weapons_tab, packing)
# dat.gg <- circleLayoutVertices(packing, npoints=50)
# 
# ggplot() + 
#   
#   # Make the bubbles
#   geom_polygon(data = dat, aes(x, y, group = id, fill = as.factor(id)), alpha = 0.6) +
#   
#   # Add text in the center of each bubble + control its size
#   geom_text(data = data, aes(x, y, size = `AP*DUR`, label = Name, color = "white")) +
#   scale_size_continuous(range = c(1,4)) +
#   
#   # General theme:
#   theme_void() + 
#   coord_equal() +
#   scale_colour_manual(values=c("#ffffff"))

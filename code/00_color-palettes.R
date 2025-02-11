# color palettes

library(RColorBrewer)

a <- RColorBrewer::brewer.pal(11, "RdYlBu")

av1 <-"#A50026"
av2 <-"#FDAE61"
av3 <-"#FFFFBF"
av4 <- "#ABD9E9"
av5 <- "#313695"

cv1 <- '#f0f921'
cv2 <- '#f89540'
cv3 <- '#cc4778'
cv4 <- '#7e03a8'
cv5 <- '#0d0887'

cv1 <- '#f0f921'
cv2 <- '#f89540'
cv3 <- '#cc4778'
cv4 <- '#7e03a8'
cv5 <- '#0d0887'
  
  
th1 <- theme(axis.title.y = element_text(size = rel(1.5)),
             axis.text.y = element_blank(),
             axis.text = element_text(size = rel(0.8)),
             axis.ticks.y = element_blank(),
             strip.background = element_rect(fill = "gray80", color = "black"),
             plot.title = element_text(hjust = 0.5, size = rel(2)), 
             strip.text.y.left = element_text(angle = 0),
             strip.text.x = element_text(size = rel(1.5)),
             panel.background = element_rect(fill = "white"),
             axis.text.x = element_text(angle = 45, hjust = 1))


th2 <- theme(axis.title.y = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(0.8)),
             plot.title = element_text(size = rel(2), hjust = 0.5), 
             strip.background = element_rect(fill = "white", color = "black"),
             strip.text = element_text(size = rel(1.5)),
             axis.text.y = element_text(face = c("italic", "italic", "italic", "italic", "italic", "italic", "bold")))

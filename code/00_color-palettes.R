# color palettes

cv1 <- '#f0f921'
cv2 <- '#f89540'
cv3 <- '#cc4778'
cv4 <- '#7e03a8'
cv5 <- '#0d0887'
  
  
th1 <- theme(axis.title.y = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(0.8)),
             plot.title = element_text(hjust = 0.5, size = rel(2)))


th2 <- theme(axis.title.y = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(0.8)),
             plot.title = element_text(size = rel(2), hjust = 0.5), 
             strip.background = element_rect(fill = "white", color = "black"),
             strip.text = element_text(size = rel(1.5)))

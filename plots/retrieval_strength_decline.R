library(ggplot2)
library(latex2exp)

#------------------------------------------------------------
# create ggplot apa style
# See https://ggplot2tutor.com/tutorials/apa_bar_chart
#------------------------------------------------------------
apatheme=theme_bw()+
  theme(
    plot.margin = unit(c(0.3,1,0,1), "cm"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(),
    axis.title = element_text(size = 12, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    
    text=element_text(family='Helvetica'),
    legend.title=element_blank(), 
    legend.position = c(0.70, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 1, l = 8, r = 8, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )

#------------------------------------------------------------
# plot retrieval strength decline function
#------------------------------------------------------------

# Generate x and y values for three sample memory items
x <- seq(0, 30, length.out = 1000)

## Memory item 1
s1 = 0.5
r1 = 0.8
y1 <- r1 * exp(-x * r1 * 1/s1)

## Memory item 2
s2 = 1
r2 = 0.5
y2 <- r2 * exp(-x * r2 * 1/s2)

## Memory item 3
s3 = 3
r3 = 0.3
y3 <- r3 * exp(-x * r3 * 1/s3)

## Create df
df <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3)

plot <- ggplot(df, aes(x))+
geom_line(aes(y = y1, color = "Memory item 1 (SS = 0.5, RS = 0.8)", linetype = "Memory item 1 (SS = 0.5, RS = 0.8)"), size=.6) +
geom_line(aes(y = y2, color = "Memory item 2 (SS = 1.0, RS = 0.5)", linetype = "Memory item 2 (SS = 1.0, RS = 0.5)"), size=.6) +
geom_line(aes(y = y3, color = "Memory item 3 (SS = 3.0, RS = 0.3)", linetype = "Memory item 3 (SS = 3.0, RS = 0.3)"), size=.7) +
scale_x_continuous(limits = c(0,15), expand = c(0, 0), breaks=seq(0,30,1)) +
scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks=seq(0,1,.1)) +
#Manually set color of lines to both be black; they are blue by default
scale_linetype_manual(name = "Legend", values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "solid", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dashed", "Memory item 3 (SS = 3.0, RS = 0.3)" = "dotted"))  +
scale_colour_manual(name = "Legend",  values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "red", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dodgerblue3", "Memory item 3 (SS = 3.0, RS = 0.3)" = "black")) +   
labs(x='d', y= TeX(r"($RS_{t+1}$)")) +
apatheme
  
plot

# Save the plot as a PDF file
ggsave(
  "plots/retrieval_strength_decline.pdf",
  plot,
  width = 6.27,
  height = 3,
  dpi = 300
)

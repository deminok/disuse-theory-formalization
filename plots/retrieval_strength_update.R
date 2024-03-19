library(ggplot2)
library(latex2exp)

#------------------------------------------------------------
# create ggplot apa style
# See https://ggplot2tutor.com/tutorials/apa_bar_chart
#------------------------------------------------------------
apatheme=theme_bw()+
  theme(
    plot.margin = unit(c(0.3,2,0,2), "cm"),
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
    legend.position = c(0.75, 0.2),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 1, l = 8, r = 8, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )

#------------------------------------------------------------
# plot retrieval strength update function after successful retrieval event
#------------------------------------------------------------

# set compression factor c
c = 3

# Generate x and y values for two sample memory items
rs <- seq(0, 1, length.out = 100)

## Memory item 1
s1 = 0.5
y1 <- 1/(c * s1 + 1) * (rs-1) + 1

## Memory item 2
s2 = 5
y2 <- 1/(c * s2 + 1) * (rs-1) + 1

## Create df
df <- data.frame(x = rs, y1 = y1, y2 = y2)

plot <- ggplot(df, aes(x))+
geom_line(aes(y = y1, color = "item 1 (SS = 0.5)", linetype = "item 1 (SS = 0.5)"), size=.6) +
geom_line(aes(y = y2, color = "item 2 (SS = 5)", linetype = "item 2 (SS = 5)"), size=.6) +
geom_abline(slope = 1, linetype="dotted") +
scale_x_continuous(limits = c(0,1), expand = expansion(add = c(0, 0)), breaks=seq(0,1,.25)) +
scale_y_continuous(limits = c(0,1), expand = expansion(add = c(0, 0)), breaks=seq(0,1,.25)) +
#Manually set color of lines to both be black; they are blue by default
scale_linetype_manual(name = "Legend", values = c("item 1 (SS = 0.5)" = "solid", "item 2 (SS = 5)" = "dashed"))  +
scale_colour_manual(name = "Legend",  values = c("item 1 (SS = 0.5)" = "red", "item 2 (SS = 5)" = "dodgerblue3")) +   
labs(x= TeX(r"($RS_{t}$)"), y= TeX(r"($RS_{t+1}$)")) +
apatheme


plot

# Save the plot as a PNG file
ggsave(
  "plots/retrieval_strength_update.pdf",
  plot,
  width = 6.27,
  height = 2.5,
  dpi = 300
)

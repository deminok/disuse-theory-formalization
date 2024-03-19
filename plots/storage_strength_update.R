library(ggplot2)
library(latex2exp)
library(cowplot)

#------------------------------------------------------------
# create ggplot apa style
# See https://ggplot2tutor.com/tutorials/apa_bar_chart
#------------------------------------------------------------
apatheme=theme_bw()+
  theme(
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
    legend.position = c(0.60, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 1, l = 8, r = 8, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )

#------------------------------------------------------------
# plot retrieval strength update function after successful retrieval event
#------------------------------------------------------------

# set compression factor b
b = 0.1

# Generate x and y values for two sample memory items
rs <- seq(0, 1, length.out = 100)

## Memory item 1
s1 = 0.5
y1 <- s1 + b / (s1 * rs)

## Memory item 2
s2 = 5
y2 <- s2 + b / (s2 * rs)

## Create df
df <- data.frame(x = rs, y1 = y1, y2 = y2)

plot1 <- ggplot(df, aes(x))+
geom_line(aes(y = y1, color = "item 1 (SS = 0.5)"), size=.6) +
  geom_hline(yintercept = s1, linetype="dashed") +
scale_x_continuous(limits = c(0,1), expand = expansion(add = c(0, .05)), breaks=seq(0,1,.25)) +
scale_y_continuous(limits = c(0,3), expand = expansion(add = c(0, .1)), breaks=seq(0,3,.5)) +
#Manually set color of lines to both be black; they are blue by default
#scale_linetype_manual(name = "Legend", values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "solid", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dashed", "Memory item 3 (SS = 3.0, RS = 0.3)" = "dotted"))  +
#scale_colour_manual(name = "Legend",  values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "red", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dodgerblue3", "Memory item 3 (SS = 3.0, RS = 0.3)" = "black")) +   
labs(x= TeX(r"($RS_{t}$)"), y= TeX(r"($SS_{t+1}$)")) +
apatheme

plot2 <- ggplot(df, aes(x))+
  geom_line(aes(y = y2, color = "item 2 (SS = 5)"), size=.6) +
  geom_hline(yintercept = s2, linetype="dashed") +
  scale_x_continuous(limits = c(0,1), expand = expansion(add = c(0, .05)), breaks=seq(0,1,.25)) +
  scale_y_continuous(limits = c(4.5,7.5), expand = expansion(add = c(0, .1)), breaks=seq(4,7.5,.5)) +
  #Manually set color of lines to both be black; they are blue by default
  #scale_linetype_manual(name = "Legend", values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "solid", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dashed", "Memory item 3 (SS = 3.0, RS = 0.3)" = "dotted"))  +
  #scale_colour_manual(name = "Legend",  values = c("Memory item 1 (SS = 0.5, RS = 0.8)" = "red", "Memory item 2 (SS = 1.0, RS = 0.5)" = "dodgerblue3", "Memory item 3 (SS = 3.0, RS = 0.3)" = "black")) +   
  labs(x= TeX(r"($RS_{t}$)"), y= TeX(r"($SS_{t+1}$)")) +
  apatheme

plots <- plot_grid(plot1, plot2, labels = "AUTO")
plots

# Save the plot as a PNG file
ggsave(
  "plots/storage_strength_update.pdf",
  plots,
  width = 6.27,
  height = 2.5,
  dpi = 300
)

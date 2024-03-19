set.seed(2024)

#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#' Compute the decline in retrieval strength after d days
#'
#' @param d number of days over which the decline takes place
#' @param ss the current retrieval strength of the memory item
#' @param rs the current storage strength of the memory item

decrease_rs <- function(d, ss, rs) {
  rs <-  rs * exp(-d * rs * 1/ss)
  rs
}

#' Compute the increase in storage strength after successful retrieval
#'
#' @param ss the current retrieval strength of the memory item
#' @param rs the current storage strength of the memory item
#' @param b compression factor that controls speed of increase in ss, default = 0.1

increase_ss <- function(ss, rs, b = 0.1) {
  ss <-  ss + b / (ss * rs)
  ss
}

#' Compute the increase in retrieval strength after successful retrieval
#'
#' @param ss the current retrieval strength of the memory item
#' @param rs the current storage strength of the memory item
#' @param c compression factor that controls speed of increase in rs, default = 0.4

increase_rs <- function(ss, rs, c = 0.4) {
  rs <-  1/(c * ss + 1) * (rs - 1) + 1
  rs
}

#------------------------------------------------------------
# Simulation
#------------------------------------------------------------
#' Compute updated memory item after an inter study interval of d days followed 
#' by successful retrieval
#'
#' @param d length of inter study interval (isi) in days
#' @param memory_item Name vector c(ss, rs) where ss and rs are the current levels
#' of storage and retrieval strength of that memory item

update_memory_item <- function(d, memory_item) {
  rs_after_isi <- decrease_rs(d, memory_item$ss, memory_item$rs)
  
  # retrieval is assumed to always be successful
  # ss remains unchanged after isi
  ss_after_retrieval <- increase_ss(memory_item$ss, rs_after_isi)
  rs_after_retrieval <- increase_rs(memory_item$ss, rs_after_isi)
  
  memory_item$ss <- ss_after_retrieval
  memory_item$rs <- rs_after_retrieval
  
  memory_item
}

#------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------

generate_memory_items <- function(n) {
  data.frame(
    id = seq_len(n),
    retrieval_strength = rbeta(n, shape1=12, shape2=3),
    #storage_strength = rlnorm(n, meanlog=-0.6, sdlog=.50)
    storage_strength = rf(n, df1 = 5, df2 = 15)
  )
}
#------------------------------------------------------------
# Simulation 
#------------------------------------------------------------
library(dplyr)
library(compute.es)

n <- 1000 # number of memory items for each group in the simulation

# Set compression params for retrieval and massed practice
b_massed = 0.05
b_retrieval = 0.2

c_massed = 3
c_retrieval = 5

#------------------------------------------------------------
# Experiment 1: Evaluate if the formalized model can reproduce the retrieval
# practice effect
#------------------------------------------------------------

# Group 1 does a massed practice session after initial exposure to the items
massed_practice_items <- generate_memory_items(n) |>
  # Calculate increases in storage and retrieval strength after  massed practice
  mutate(storage_strength_t2 = increase_ss(ss = storage_strength, rs = retrieval_strength, b = b_massed)) |>
  mutate(retrieval_strength_t2 = increase_rs(ss = storage_strength, rs = retrieval_strength, c = c_massed)) |>
  # Calculte retrieval strengths after 1 and after 30 days
  mutate(retention_1_day = decrease_rs(1, ss = storage_strength_t2, rs = retrieval_strength_t2)) |>
  mutate(retention_30_day = decrease_rs(30, ss = storage_strength_t2, rs = retrieval_strength_t2)) |>
  mutate(group = "Study")

# Group 2 does a retrieval practice session immediately after initial exposure
retrieval_practice_items <- generate_memory_items(n) |>
  # Calculate increases in storage and retrieval strength after  massed practice
  mutate(storage_strength_t2 = increase_ss(ss = storage_strength, rs = retrieval_strength, b = b_retrieval)) |>
  mutate(retrieval_strength_t2 = increase_rs(ss = storage_strength, rs = retrieval_strength, c = c_retrieval)) |>
  # Calculte retrieval strengths after 1 and after 30 days
  mutate(retention_1_day = decrease_rs(1, ss = storage_strength_t2, rs = retrieval_strength_t2)) |>
  mutate(retention_30_day = decrease_rs(30, ss = storage_strength_t2, rs = retrieval_strength_t2)) |>
  mutate(group = "Retrieval")

# Combine rentention intervals into data frames for t test
cols = c("group", "retention_1_day")
t1_retention_intervals <- rbind(
  massed_practice_items[cols], 
  retrieval_practice_items[cols]
)

# groups difference in retrieval strength after 1 day
t1 <- t.test(t1_retention_intervals$retention_1_day ~ t1_retention_intervals$group, alternative = "greater")
t1

ES1 <- tes(t1$statistic, n, n, level = 95, verbose=FALSE)
ES1$g # g for Hedge's g to compare to published effect sizes

# Compare standard deviations for both groups
sd(retrieval_practice_items$retention_1_day)
sd(massed_practice_items$retention_1_day)

#------------------------------------------------------------
# Experiment 2: Evaluate if the formalized model can reproduce the spacing effect
#------------------------------------------------------------
# Group 1 does 4 massed practice session after initial exposure to the items
massed_practice_items_2 <- generate_memory_items(n) |>
  # Calculate increases in storage and retrieval strength after  massed practice
  # 1. session
  mutate(storage_strength_t1 = increase_ss(ss = storage_strength, rs = retrieval_strength, b = b_massed)) |>
  mutate(retrieval_strength_t1 = increase_rs(ss = storage_strength, rs = retrieval_strength, c = c_massed)) |>
  # 2. session
  mutate(storage_strength_t2 = increase_ss(ss = storage_strength_t1, rs = retrieval_strength_t1, b = b_massed)) |>
  mutate(retrieval_strength_t2 = increase_rs(ss = storage_strength_t1, rs = retrieval_strength_t1, c = c_massed)) |>
  # 3. session
  mutate(storage_strength_t3 = increase_ss(ss = storage_strength_t2, rs = retrieval_strength_t2, b = b_massed)) |>
  mutate(retrieval_strength_t3 = increase_rs(ss = storage_strength_t2, rs = retrieval_strength_t2, c = c_massed)) |>
  # 4. session
  mutate(storage_strength_t4 = increase_ss(ss = storage_strength_t3, rs = retrieval_strength_t3, b = b_massed)) |>
  mutate(retrieval_strength_t4 = increase_rs(ss = storage_strength_t2, rs = retrieval_strength_t2, c = c_massed)) |>
  # Calculte retrieval strengths after 1 and after 30 days
  mutate(retention_1_day = decrease_rs(1, ss = storage_strength_t3, rs = retrieval_strength_t3)) |>
  mutate(group = "Massed")


# Group 2 does 4 spaced massed practice sessions with intervals 1, 3, 5, 7
spaced_practice_items <- generate_memory_items(n)|>
  # 1. retrieval session after 1 day
  mutate(retrieval_strength_t2 = decrease_rs(1, ss = storage_strength, rs = retrieval_strength)) |>
  mutate(storage_strength_t2 = increase_ss(ss = storage_strength, rs = retrieval_strength_t2, b = b_massed)) |>
  mutate(retrieval_strength_t2 = increase_rs(ss = storage_strength, rs = retrieval_strength_t2, c = c_massed)) |>
  # 2. retrieval session after 3 days
  mutate(retrieval_strength_t3 = decrease_rs(3, ss = storage_strength_t2, rs = retrieval_strength_t2)) |>
  mutate(storage_strength_t3 = increase_ss(ss = storage_strength_t2, rs = retrieval_strength_t3, b = b_massed)) |>
  mutate(retrieval_strength_t3 = increase_rs(ss = storage_strength_t2, rs = retrieval_strength_t3, c = c_massed)) |>
  # 3. retrieval session after 5 days
  mutate(retrieval_strength_t4 = decrease_rs(5, ss = storage_strength_t3, rs = retrieval_strength_t3)) |>
  mutate(storage_strength_t4 = increase_ss(ss = storage_strength_t3, rs = retrieval_strength_t4, b = b_massed)) |>
  mutate(retrieval_strength_t4 = increase_rs(ss = storage_strength_t3, rs = retrieval_strength_t4, c = c_massed)) |>
  # 4. retrieval session after 5 days
  mutate(retrieval_strength_t5 = decrease_rs(7, ss = storage_strength_t4, rs = retrieval_strength_t4)) |>
  mutate(storage_strength_t5 = increase_ss(ss = storage_strength_t4, rs = retrieval_strength_t5, b = b_massed)) |>
  mutate(retrieval_strength_t5 = increase_rs(ss = storage_strength_t4, rs = retrieval_strength_t5, c = c_massed)) |>
  # Calculate retrieval strengths after 1 and after 30 days
  mutate(retention_1_day = decrease_rs(1, ss = storage_strength_t3, rs = retrieval_strength_t3)) |>
  mutate(group = "Spaced")


t2_retention_intervals <- rbind(
  massed_practice_items_2[cols], 
  spaced_practice_items[cols]
)



t2 <- t.test(t2_retention_intervals$retention_1_day ~ t2_retention_intervals$group, alternative = "less")
t2




ES2 <- tes(t2$statistic, n, n, level = 95, verbose=FALSE)
ES2$g # g for Hedge's g to compare to published effect sizes

# Compare standard deviations for both groups
sd(massed_practice_items_2$retention_1_day)
sd(spaced_practice_items$retention_1_day)

#------------------------------------------------------------
# Plot simulated data: initial values of storage and retrieval strength
#------------------------------------------------------------
library(ggplot)
library(cowplot)

# create ggplot apa style
# See https://ggplot_initial_rstutor.com/tutorials/apa_bar_chart

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
    axis.text.y = element_text(margin = margin(r = 5)),
    text=element_text(family='Helvetica'),
    legend.title=element_blank(), 
    legend.position = c(0.80, 0.2),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 1, l = 8, r = 8, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )

# Plot storage strength and retrieval strength values after initial exposure
cols2 = c("storage_strength", "retrieval_strength")
simulated_items <- rbind(
  massed_practice_items[cols2], 
  retrieval_practice_items[cols2],
  spaced_practice_items[cols2]
)

plot_initial_ss <- ggplot(simulated_items, aes(y = storage_strength, x = "")) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 3), expand = expansion(add = c(0, 0)), breaks=seq(0,3,.5)) +
  labs(x= "", y= "Storage Strength") +
  apatheme +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

plot_initial_rs <- ggplot(simulated_items, aes(y = retrieval_strength, x = ""))+
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(add = c(0, 0)), breaks=seq(0,1,.2)) +
  labs(x= "", y= "Retrieval Strength") +
  apatheme +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

plot_initial_data <- plot_grid(plot_initial_ss, plot_initial_rs, labels = "AUTO")
plot_initial_data

# Save the plot as a PNG file
ggsave(
  "plots/simulated_items.pdf",
  plot_initial_data,
  width = 6.27,
  height = 2.5,
  dpi = 300
)

#------------------------------------------------------------
# Plot simulated data: reults of the two experiments
#------------------------------------------------------------

# Combine groups into data frames
cols3 = c("group", "retention_1_day")
data_ex_1 <- rbind(
  massed_practice_items[cols3],
  retrieval_practice_items[cols3]
)

data_ex_2 <- rbind(
  massed_practice_items_2[cols3],
  spaced_practice_items[cols3]
)

plot_ex_1 <- ggplot(data_ex_1, aes(y = retention_1_day, x = group)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(add = c(0, 0)), breaks=seq(0,1,.2)) +
  labs(x= "", y= "Retrieval Strength after 1 day") +
  apatheme

plot_ex_1

plot_ex_2 <- ggplot(data_ex_2, aes(y = retention_1_day, x = group)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(add = c(0, 0)), breaks=seq(0,1,.2)) +
  labs(x= "", y= "Retrieval Strength after 1 day") +
  apatheme

plot_ex_2

plot_experiments <- plot_grid(plot_ex_1, plot_ex_2, labels = "AUTO")
plot_experiments

# Save the plot as a PNG file
ggsave(
  "plots/simulated_experiments.pdf",
  plot_experiments,
  width = 6.27,
  height = 3.5,
  dpi = 300
)

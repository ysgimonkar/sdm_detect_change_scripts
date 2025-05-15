# Comparison of predictive performance plots

library(tidyverse)
library(plotrix)
library(see)
library(paletteer)
library(patchwork)

######################## Scenario 1 ################################
################################# RMSE ############################

load("sc1_model_diagnostics.RData")

res1 <- frmse %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'RMSE')

res2 <- frmse %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'RMSE')

res3 <- frmse %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'RMSE')

res4 <- frmse %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'RMSE')

df1 <- as.data.frame(rbind(res1, res2, res3, res4))

################################ TJUR ##############################

res1 <- ftjur %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = "Tjur's R2" )

res2 <- ftjur %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = "Tjur's R2")

res3 <- ftjur %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = "Tjur's R2")

res4 <- ftjur %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = "Tjur's R2")

df2 <- as.data.frame(rbind(res1, res2, res3, res4))

################################# AUC ############################

res1 <- fauc %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'AUC')

res2 <- fauc %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'AUC')

res3 <- fauc %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'AUC')

res4 <- fauc %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'AUC')

df3 <- as.data.frame(rbind(res1, res2, res3, res4))

df4 <- merge(df1,df2, all = TRUE)
df5 <- merge(df3, df4, all = TRUE)

df6 <- df5 %>%
  add_column(sc = "Scenario 1")

######################## SCENARIO 2 ################################
################################# RMSE ############################

load("sc2_model_diagnostics.RData")

res1 <- frmse %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'RMSE')

res2 <- frmse %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp5" | spp == "spp8" | spp == "spp11" | spp == "spp14"| spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'RMSE')

res3 <- frmse %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp10" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'RMSE')

res4 <- frmse %>%
  filter(spp == "spp1" | spp == "spp12" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'RMSE')

df7 <- as.data.frame(rbind(res1, res2, res3, res4))

################################ TJUR ##############################

res1 <- ftjur %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = "Tjur's R2")

res2 <- ftjur %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp5" | spp == "spp8" | spp == "spp11" | spp == "spp14"| spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = "Tjur's R2")

res3 <- ftjur %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp10" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = "Tjur's R2")

res4 <- ftjur %>%
  filter(spp == "spp1" | spp == "spp12" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = "Tjur's R2")

df8 <- as.data.frame(rbind(res1, res2, res3, res4))

################################# AUC ############################

res1 <- fauc %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'AUC')

res2 <- fauc %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp5" | spp == "spp8" | spp == "spp11" | spp == "spp14"| spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'AUC')

res3 <- fauc %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp10" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'AUC')

res4 <- fauc %>%
  filter(spp == "spp1" | spp == "spp12" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'AUC')

df9 <- as.data.frame(rbind(res1, res2, res3, res4))

df10 <- merge(df7,df8, all = TRUE)
df11 <- merge(df9, df10, all = TRUE)

df12 <- df11 %>%
  add_column(sc = "Scenario 2")
  
######################## SCENARIO 3 ################################
################################# RMSE ############################

load("sc3_model_diagnostics.RData")

res1 <- frmse %>%
  filter(spp == "spp5" | spp == "spp12" | spp == "spp14" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'RMSE')

res2 <- frmse %>%
  filter(spp == "spp1" | spp == "spp3" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'RMSE')

res3 <- frmse %>%
  filter(spp == "spp2" | spp == "spp4" | spp == "spp6" | spp == "spp8" | spp == "spp11" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'RMSE')

res4 <- frmse %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp10" | spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'RMSE')

df13 <- as.data.frame(rbind(res1, res2, res3, res4))

################################ TJUR ##############################

res1 <- ftjur %>%
  filter(spp == "spp5" | spp == "spp12" | spp == "spp14" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = "Tjur's R2")

res2 <- ftjur %>%
  filter(spp == "spp1" | spp == "spp3" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = "Tjur's R2")

res3 <- ftjur %>%
  filter(spp == "spp2" | spp == "spp4" | spp == "spp6" | spp == "spp8" | spp == "spp11" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = "Tjur's R2")

res4 <- ftjur %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp10" | spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = "Tjur's R2")

df14 <- as.data.frame(rbind(res1, res2, res3, res4))

################################# AUC ############################

res1 <- fauc %>%
  filter(spp == "spp5" | spp == "spp12" | spp == "spp14" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'AUC')

res2 <- fauc %>%
  filter(spp == "spp1" | spp == "spp3" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'AUC')

res3 <- fauc %>%
  filter(spp == "spp2" | spp == "spp4" | spp == "spp6" | spp == "spp8" | spp == "spp11" | spp == "spp13") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'AUC')

res4 <- fauc %>%
  filter(spp == "spp7" | spp == "spp9" | spp == "spp10" | spp == "spp15" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'AUC')

df15 <- as.data.frame(rbind(res1, res2, res3, res4))

df16 <- merge(df13,df14, all = TRUE)
df17 <- merge(df15, df16, all = TRUE)

df18 <- df17 %>%
  add_column(sc = "Scenario 3")

######################## SCENARIO 4 ################################
################################# RMSE ############################

load("sc4_model_diagnostics.RData")

res1 <- frmse %>%
  filter(spp == "spp4" | spp == "spp7" | spp == "spp11" | spp == "spp13" | spp == "spp15") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'RMSE')

res2 <- frmse %>%
  filter(spp == "spp1" | spp == "spp5" | spp == "sp8" | spp == "spp9" | spp == "sp10" | spp == "spp12") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'RMSE')

res3 <- frmse %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'RMSE')

res4 <- frmse %>%
  filter(spp == "spp3" | spp == "spp14" | spp == "spp17" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'RMSE')

df19 <- as.data.frame(rbind(res1, res2, res3, res4))

################################ TJUR ##############################

res1 <- ftjur %>%
  filter(spp == "spp4" | spp == "spp7" | spp == "spp11" | spp == "spp13" | spp == "spp15") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = "Tjur's R2")

res2 <- ftjur %>%
  filter(spp == "spp1" | spp == "spp5" | spp == "sp8" | spp == "spp9" | spp == "sp10" | spp == "spp12") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = "Tjur's R2")

res3 <- ftjur %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = "Tjur's R2")

res4 <- ftjur %>%
  filter(spp == "spp3" | spp == "spp14" | spp == "spp17" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = "Tjur's R2")

df20 <- as.data.frame(rbind(res1, res2, res3, res4))

################################# AUC ############################

res1 <- fauc %>%
  filter(spp == "spp4" | spp == "spp7" | spp == "spp11" | spp == "spp13" | spp == "spp15") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1",
             type = 'AUC')

res2 <- fauc %>%
  filter(spp == "spp1" | spp == "spp5" | spp == "sp8" | spp == "spp9" | spp == "sp10" | spp == "spp12") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2",
             type = 'AUC')

res3 <- fauc %>%
  filter(spp == "spp2" | spp == "spp6" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3",
             type = 'AUC')

res4 <- fauc %>%
  filter(spp == "spp3" | spp == "spp14" | spp == "spp17" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4",
             type = 'AUC')

df21 <- as.data.frame(rbind(res1, res2, res3, res4))

df22 <- merge(df19,df20, all = TRUE)
df23 <- merge(df21, df22, all = TRUE)

df24 <- df23 %>%
  add_column(sc = "Scenario 4")


###### merge ####

df25 <- merge(df6,df12, all = TRUE)
df26 <- merge(df18, df25, all = TRUE)
df27 <- merge(df24, df26, all = TRUE)

####### use df27 #####

# Create a data frame for annotations
annotation_data <- data.frame(
  x = rep(1.5,16),
  meanx = rep(seq(0.2, 0.8, 0.2),4),
  label = rep(seq(0.2, 0.8, 0.2), 4),
  facet_column = c(rep("Scenario 1",4), rep("Scenario 2",4), rep("Scenario 3",4), rep("Scenario 4",4))  # Replace 'facet_column' with your actual facetting column
)

cols <- c("blue", "deeppink", "#00ff00", "seagreen", "orange", "dodgerblue")


response1 <- df27 %>%
  filter(res == "Response 1") %>%
  ggplot(aes(
    x = type,
    y = meanx,
    color = models,
    group = models,
    fill = models
  )) +
  geom_text(data = annotation_data, aes(x = x, y = meanx, label = label), inherit.aes = FALSE, col = "grey60", size = 5, parse = TRUE) +
  scale_x_discrete() +
  geom_polygon(linewidth = 1.5, alpha = 0.05) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  coord_radar() +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey70"),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size=25),
        legend.position="none") +
  xlab("") +
  ylab("") +
  labs(color = "Models", fill = "Models") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(~sc, ncol = 4) +
  ggtitle("Response 1")

response2 <- df27 %>%
  filter(res == "Response 2") %>%
  ggplot(aes(
    x = type,
    y = meanx,
    color = models,
    group = models,
    fill = models
  )) +
  geom_text(data = annotation_data, aes(x = x, y = meanx, label = label), inherit.aes = FALSE, col = "grey60", size = 5, parse = TRUE) +
  scale_x_discrete() +
  geom_polygon(linewidth = 1.5, alpha = 0.05) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  coord_radar() +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey70"),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size=05),
        legend.position="none") +
  xlab("") +
  ylab("") +
  labs(color = "Models", fill = "Models") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(~sc, ncol = 4) +
  ggtitle("Response 2")

response3 <- df27 %>%
  filter(res == "Response 3") %>%
  ggplot(aes(
    x = type,
    y = meanx,
    color = models,
    group = models,
    fill = models
  )) +
  geom_text(data = annotation_data, aes(x = x, y = meanx, label = label), inherit.aes = FALSE, col = "grey60", size = 5, parse = TRUE) +
  scale_x_discrete() +
  geom_polygon(linewidth = 1.5, alpha = 0.05) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  coord_radar() +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey70"),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size=25),
        legend.position="none") +
  xlab("") +
  ylab("") +
  labs(color = "Models", fill = "Models") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(~sc, ncol = 4) +
  ggtitle("Response 3")


response4 <- df27 %>%
  filter(res == "Response 4") %>%
  ggplot(aes(
    x = type,
    y = meanx,
    color = models,
    group = models,
    fill = models
  )) +
  geom_text(data = annotation_data, aes(x = x, y = meanx, label = label), inherit.aes = FALSE, col = "grey60", size = 5, parse = TRUE) +
  scale_x_discrete() +
  geom_polygon(linewidth = 1.5, alpha = 0.05) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  coord_radar() +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey70"),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size=25),
        legend.position="none") +
  xlab("") +
  ylab("") +
  labs(color = "Models", fill = "Models") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(~sc, ncol = 4) +
  ggtitle("Response 4")


response1
response2
response3
response4


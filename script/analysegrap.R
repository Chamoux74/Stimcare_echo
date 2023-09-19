dfechobase <- rbind(dfmeanbasepatch, dfmeanbaseplacebo)

dfechobase %>% filter(
  grepl("PRE" , timing)) %>% group_by(condition) %>% shapiro_test(meandiameter)

dfechobase %>% filter(
  grepl("PRE" , timing)) %>% group_by(condition) %>% identify_outliers(meandiameter)

dfechobase %>% filter(
  grepl("PRE" , timing)) %>% ggplot(aes(x = condition , y = meandiameter)) +
  geom_point(aes(color = sujet))+
    geom_boxplot(alpha = 0.3) +
  geom_line(
    aes(
      x = condition ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.7 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
    stat_summary(
      fun.y = mean ,
      shape = 23  ,
      fill = "black" ,
      size = 1
    ) +
    stat_compare_means(method = "t.test" , paired = TRUE
    ) +
  facet_grid(cols = vars(test_type))

dfechofmd <- rbind(dfpatch, dfplacebo)

dfechofmd %>% filter(grepl("BASEPOST48" , instant)) %>% group_by(condition) %>% shapiro_test(SRpic)

dfechofmd %>% filter(grepl("BASEPOST" , instant)) %>% group_by(condition) %>% identify_outliers(time_to_peak)

plot <- dfechofmd %>% ggplot(aes(x = condition , y = FMD)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_boxplot(
    aes(fill = condition , color = condition),
    alpha = 0.4) +
      #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
      stat_summary(
        aes(group = condition),
        fun.y = mean ,
        shape = 23  ,
        fill = "black" ,
        size = 1 ,
        # position = position_dodge2(width = 0.75,
        #                            preserve = "single"
        ) +
  stat_compare_means(method = "t.test" , paired = TRUE
  ) + facet_wrap(vars(instant))

plot

test1 %>% wilcox_test(FMD ~ condition ,
                      paired = TRUE,
                      p.adjust.method = "bonferroni")

test1 %>% t_test(peakdiameter ~condition, paired = TRUE, p.adjust.method = "bonferroni")


plot

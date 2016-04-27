graph <- list()
# benzo by provider
graph$benzo <- ggplot(data = agg$benzo$provider,
  aes(x = provider, y = num_Benzo_prescribed, ymax = 1.1*num_Benzo_prescribed))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1)+
  theme_light()+theme(axis.title.x = element_text(colour = "grey30"),
                  axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title="Number of Benzos Currently Prescribed",
       x="staff", y="number of prescriptions")+
  geom_text(data = agg$benzo$provider, aes(x = provider,
    y = num_Benzo_prescribed, label = num_Benzo_prescribed),
    hjust = -.1, size = 3)
# ggsave(filename="benzo barplot.png", plot=p_benzo, path=file.path(baseWD, resultsWD), width=5, height=5, units="in")
# stimulant by provider
graph$stim <- ggplot(data = agg$stim$provider, aes(x = provider,
  y = num_Stimulants_prescribed, ymax = 1.1*num_Stimulants_prescribed)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1) +
  theme_light()+theme(axis.title.x = element_text(colour = "grey30"),
                  axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title = "Number of Stimulants Currently Prescribed", x = "staff",
    y = "number of prescriptions") +
  geom_text(data = agg$stim$provider, aes(x = provider,
    y = num_Stimulants_prescribed, label = num_Stimulants_prescribed),
    hjust = -.1, size = 3)
# ggsave(filename="stimulants barplot.png", plot=p_stim, path=file.path(baseWD, resultsWD), width=5, height=4, units="in")
# benzo_stim by provider
graph$benzo_stim <- ggplot(data = agg$benzo_stim$provider, aes(x = provider,
  y = num_benzo_stim_prescribed, ymax = 1.1*num_benzo_stim_prescribed))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1)+
  theme_light()+theme(axis.title.x = element_text(colour = "grey30"),
                  axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title = "Number of Benzos and Stimulants Currently Prescribed",
       x = "staff", y = "number of prescriptions")+
  geom_text(data = agg$benzo_stim$provider, aes(x=provider, y=num_benzo_stim_prescribed,
    label=num_benzo_stim_prescribed), hjust = -.1, size = 3)
# ggsave(filename="benzo_stim barplot.png", plot=p_benzo_stim, path=file.path(baseWD, resultsWD), width=5, height=3, units="in")

# combined vendors, current fy, by provider
# only allowing one fiscal year at present - JDD 4/27/2016
stopifnot(agg$mm_ir$ven_comb[span_type == "fy", length(unique(span_label))] == 1)
graph$mm_ir_ven_fy <- ggplot(data = agg$mm_ir$ven_comb[span_type == "fy"],
       aes(x = vendor, y = pct_mm_ir, ymax = 1.2*pct_mm_ir)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  coord_flip() + theme_light() +
  geom_text(aes(x = vendor, y = pct_mm_ir, label = gg_lab),
            hjust = -0.1, na.rm = TRUE) +
  geom_text(aes(x = vendor, y  = 0, label = num_IRs), hjust = 1, na.rm = TRUE) +
  labs(x = "provider", y = "percent consumers w/ missed medication IRs",
    title = expression(atop("Missed Med IRs: Current Fiscal Year",
                            atop(italic("IR total before bar"), ""))))
# combined vendors, all qtrs, by provider
agg$mm_ir$ven_comb[, span_label := factor(span_label,
  levels = agg$mm_ir$ven_comb[, rev(sort(unique(span_label)))])]
graph$mm_ir_ven_qtr <- ggplot(data = agg$mm_ir$ven_comb[span_type == "qtr"],
    aes(x = vendor, y = pct_mm_ir, ymax = 1.2*pct_mm_ir, fill = span_label)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(0.5), width = 0.5) +
  coord_flip() + theme_light() +
  geom_text(aes(x = vendor, y = pct_mm_ir, label = gg_lab),
    hjust = -0.1, na.rm = TRUE, position = position_dodge(0.5)) +
  geom_text(aes(x = vendor, y  = 0, label = num_IRs), hjust = 1, na.rm = TRUE,
            position = position_dodge(0.5)) +
  labs(x = "provider", y = "percent consumers w/ missed medication IRs",
       title = expression(atop("Missed Med IRs: Fiscal Quarters",
                               atop(italic("IR total before bar"), ""))))
agg$mm_ir$ven_comb[, span_label := as.character(span_label)]

# only allowing one fiscal year at present - JDD 4/27/2016
stopifnot(agg$mm_ir$con_fy[, length(unique(fy))] == 1)
graph$mm_ir_con_fy <- ggplot(data = agg$mm_ir$con_fy, aes(x = vendor, y = num_IRs) )+
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.5) +
  coord_flip() + theme_light() +
  labs(title = expression(atop("Indiviual Consumers by Vendor (jittered)", atop(italic("Current Fiscal Year")))),
       x = "vendor", y = "number of Missed Med IRs")

graph$mm_ir_con_qtr <- ggplot(data = agg$mm_ir$con_qtr, aes(x = vendor, y = num_IRs, color = qtr) )+
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.5) +
  theme_light() + facet_wrap(~qtr, scales = "free") + theme(axis.text.x = element_text(angle = 90)) +
  labs(title = expression(atop("Indiviual Consumers by Vendor (jittered)", atop(italic("Current Fiscal Quarters")))),
       x = "vendor", y = "number of Missed Med IRs")



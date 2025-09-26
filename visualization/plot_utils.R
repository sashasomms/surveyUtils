# Plotting functions
## Sasha L Sommerfeldt

make_hist <- function(var_x, lab_x, df, binwidth = 1, barcolor = '#00535F') {
  ggplot(df, aes(eval(parse(text = var_x)))) +
    geom_histogram(col = "black", fill = barcolor, binwidth = binwidth) +
    labs(x = lab_x, y = "Number of Participants") +
    theme_bw(base_size = 14) +
    theme(axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 24),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
}

make_scatter <- function(var_x, lab_x, var_y, lab_y, df, linecolor = "#71B6B3") {
  ggplot(df, aes(x = eval(parse(text = var_x)), y = eval(parse(text = var_y)))) +
    geom_point(size = 4, alpha = .6, position = "jitter") +
    geom_smooth(method="lm", se=F, color = linecolor) +
    theme_bw(base_size = 24) +  
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color="black"),
          axis.ticks = element_line(color="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA)) + 
    xlim(min(df[var_x]), max(df[var_x])) + 
    ylim(min(df[var_y]), max(df[var_y])) +
    labs(x = lab_x, y = lab_y)
}

make_group_scatter <- function(var_x, lab_x, var_y, lab_y, df) {
  jitter = position_jitter(width = 0.1, height = 0.1)
  ggplot(df, aes(x = eval(parse(text = var_x)), y = eval(parse(text = var_y)), colour=as.factor(group_label), group = as.factor(group_label))) +
    geom_point(size = 3, alpha = .6, position = jitter) +
    scale_color_manual(name ="Group",             
                       labels=c("Control", "Mindfulness"), values=c("#519416","#941651")) + 
    theme_bw(base_size = 24) +  
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color="black"),
          axis.ticks = element_line(color="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none") + 
    ylim(min(df[var_y]), max(df[var_y])) +
    labs(x = lab_x, y = lab_y)
}

make_group_bar <- function(var_x, lab_x, var_y, lab_y, df) {
  jitter = position_jitter(width = 0.1, height = 0.1)
  ggplot(df, aes(x = eval(parse(text = var_x)), y = eval(parse(text = var_y)), colour=as.factor(group_label), group = as.factor(group_label))) +
    #geom_bar(stat = "identity", position = position_dodge()) +
    geom_point(size = 3, alpha = .6, position = jitter) +
    stat_summary(aes(y = eval(parse(text = var_y)), group = "Contol"), fun = "mean", geom = "line", size = 2) + #stat_summary is a way to compute and then plot basic statistics, such as the mean
    #stat_summary(aes(y = IAT, group = 1), color = "black", fun.y = "mean", geom = "point", size = 3) +
    scale_color_manual(name = "Group",             
                       labels = c("Control", "Mindfulness"), values = c("#519416","#941651")) + 
    theme_bw(base_size = 24) +  
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color="black"),
          axis.ticks = element_line(color="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none") + 
    ylim(min(df[var_y]), max(df[var_y])) +
    labs(x = lab_x, y = lab_y)
}


make_group_time_plot <- function(var_x, lab_x, var_y, lab_y, df) {
  jitter = position_jitter(width = 0.1, height = 0.1)
  ggplot(df, aes(x = eval(parse(text = var_x)), y = eval(parse(text = var_y)), colour = as.factor(group_c), group = ID)) +
    geom_line(alpha = .3) +
    geom_point(size = .5, alpha = .4) +
    stat_summary(aes(group = as.factor(group_c)), fun = "mean", geom = "line", size = 2, alpha = .95) +
    stat_summary(aes(group = as.factor(group_c)), fun = "mean", geom = "point", shape = 17, size = 5, alpha = .95) +
    scale_color_manual(name = "Group",             
                       labels = c("Control", "Mindfulness"), values = c("#519416","#941651")) + 
    theme_bw(base_size = 24) +  
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color="black"),
          axis.ticks = element_line(color="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none") + 
    ylim(min(df[var_y]), max(df[var_y])) +
    labs(x = lab_x, y = lab_y)
}


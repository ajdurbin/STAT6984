library(ggplot2)
baseball <- read.csv("baseball.csv")
ggplot(data = baseball, mapping = aes(x = o_Eye, y = Winning_Percentage,
                                      color = League)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

ggplot(data = baseball, mapping = aes(x = o_Average, y = Winning_Percentage,
                                      color = League)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

ggplot(data = baseball, mapping = aes(x = o_Power, y = Winning_Percentage,
                                      color = League)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

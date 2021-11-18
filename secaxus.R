# #####
# date<-c(as.Date("2020-09-15", "%Y-%m-%d"),as.Date("2020-09-14", "%Y-%m-%d"),
#         as.Date("2020-09-13", "%Y-%m-%d"),as.Date("2020-09-12", "%Y-%m-%d"),
#         as.Date("2020-09-15", "%Y-%m-%d"),as.Date("2020-09-14", "%Y-%m-%d"),
#         as.Date("2020-09-13", "%Y-%m-%d"),as.Date("2020-09-12", "%Y-%m-%d"),
#         as.Date("2020-09-16", "%Y-%m-%d"),as.Date("2020-09-17", "%Y-%m-%d"),
#         as.Date("2020-09-18", "%Y-%m-%d"),as.Date("2020-09-19", "%Y-%m-%d"),
#         as.Date("2020-09-20", "%Y-%m-%d"),as.Date("2020-09-21", "%Y-%m-%d"),
#         as.Date("2020-09-22", "%Y-%m-%d"),as.Date("2020-09-23", "%Y-%m-%d"),
#         as.Date("2020-09-24", "%Y-%m-%d"),as.Date("2020-09-25", "%Y-%m-%d"),
#         as.Date("2020-09-26", "%Y-%m-%d"),as.Date("2020-09-27", "%Y-%m-%d"),
#         as.Date("2020-09-28", "%Y-%m-%d"),as.Date("2020-09-29", "%Y-%m-%d"),
#         as.Date("2020-09-30", "%Y-%m-%d"),as.Date("2020-10-01", "%Y-%m-%d"))
# value<-c(5,6,7,8,-3,-5,6,8,5,6,7,8,-3,-5,6,8,5,6,7,8,-3,-5,6,8)
# df<-data.frame(date,value)
# df2<-data.frame(table(df$date))
# 
# 
# library(data.table)
# 
# adaptiveparam2 <- c(1:6, rep(7, nrow(df2)-6))
# df2$rollmean <- frollmean(df2$Freq, n=adaptiveparam2, adaptive = T)
# 
# df2
# 
# ggplot(within(df2, Var1 <- as.Date(Var1)), aes(x=Var1)) +
#   geom_bar( aes(y=Freq), stat="identity", size=.1, 
#             fill="steelblue", color="black", alpha=.4) + 
#   geom_line( aes(y=rollmean), size=2, color="red") +
#   scale_y_continuous(name = "Temperature (Celsius °)",
#                      sec.axis = sec_axis( ~.,name="Price ($)")) +   
#   theme(axis.title.y = element_text(color = "steelblue", size=13),
#         axis.title.y.right = element_text(color = "red", size=13)) +
#   ggtitle("Temperature down, price up")
# 
# df2
# 
# g <- ggplot(within(df2, Var1 <- as.Date(Var1)), aes(x=Var1)) +
#   geom_bar( aes(y=Freq), stat="identity", size=.1, 
#             fill="steelblue", color="black", alpha=.4) +
#   geom_line( aes(y=rollmean), size=2, color="red") +
#   scale_y_continuous(name = "Temperature (Celsius °)") +   
#   theme(axis.title.y = element_text(color = "steelblue", size=13),
#         axis.title.y.right = element_text(color = "red", size=13)) +
#   ggtitle("Temperature down, price up")
# 
# ggplotly(g) %>%
#   add_lines(x= ~Var1, y=  ~rollmean, colors = ~rollmean, yaxis="y2", 
#             data=df2, showlegend=FALSE, inherit=TRUE)  %>%
#   layout(yaxis2 = list(
#     tickfont = list(size=11.7),
#     titlefont=list(size=14.6, col = "red"),
#     overlaying = "y",
#     nticks = 5,
#     side = "right",
#     title = "Price ($)"
#   ))

# adasd ####

burger<- tibble(
  Day = rep(1:4,2),
  Dose = c(0.5,0.6,0.4,0.3,0.5,0.6,0.4,0.3),
  Wight = c(0.95,0.92,0.93,0.98,0.95,0.92,0.93,0.98))

ggplot(data = burger, aes(x = Day, y = Dose))+
  geom_bar(aes(y = Wight * (0.98/0.6) - 1), stat = "identity") +
  geom_line(size = 2) +
  geom_point(aes(colour = Day))  +
  scale_y_continuous(name = "Dose", 
                     sec.axis = sec_axis(trans =  ~(.+1)*(0.6/0.98),
                                         name = "Wight")) +
  coord_cartesian(ylim = c(0.3, 0.6))
df
limits = c(4,8)

breaks = c(5,6,7)

ay <- list(
  side = "left",
  showticklabels = TRUE,
  range = c(min(df$value),max(df$value)),
  tickmode = "array", 
  tickvals = breaks)


ay <- list(
  tickfont = list(size=11.7),
  titlefont=list(size=14.6),
  overlaying = "y",
  nticks = 5,
  side = "right",
  title = "Second y axis"
)

ggplotly(Juros_cambio_plot) %>%
  add_lines(x=~date, y=~value, colors=NULL, yaxis="y2", 
            data=df, showlegend=FALSE, inherit=FALSE) %>%
  layout(yaxis2 = ay)



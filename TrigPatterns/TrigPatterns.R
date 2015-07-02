#ORINGINAL CODE BY Antonio S. Chinchón
#https://aschinchon.wordpress.com/2015/07/01/trigonometric-pattern-design/

#Only alterations from the original code are color and 
#the specific formula that creates the design
#I'm just having fun with formulas and color schemes here

library("wesanderson")
library("magrittr")
library("ggplot2")
library("pracma")

#picking my colors
m <- (wes_palette("GrandBudapest")[c(3)])
n <- (wes_palette("GrandBudapest")[c(2)])

#Trigonometric Pattern Creation
RecurrencePlot = function(from, to, col1, col2) {
  opt = theme(legend.position  = "none",
              panel.background = element_blank(),
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank()) 
  seq(from, to, by = .1) %>% expand.grid(x=., y=.) %>% 
    ggplot( ., aes(x=x, y=y, fill=erf(sec(x)-sec(y)^2))) + geom_tile() + 
    scale_fill_gradientn(colours=colorRampPalette(c(col1, col2))(2)) + opt}
RecurrencePlot(from = -5*pi, to = 5*pi, col1 = m, col2= n)

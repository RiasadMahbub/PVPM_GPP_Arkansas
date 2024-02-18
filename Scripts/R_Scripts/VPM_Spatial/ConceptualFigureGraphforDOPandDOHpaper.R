x <- c(1:1000)
ylo <- 0
yhi <- 1
h <- -1
k <- 30
y <- ylo+((yhi-ylo)*x^h)/(k^h+x^h)
#linear plot
ggplot(data.frame(x,y), aes(x,y))+
  geom_point(color="blue")+
  ylab("Growing season length")+
  xlab("cumulative climatological effect")
#log plot
ggplot(data.frame(x,y), aes(x,y))+
  geom_point(color="blue")+
  scale_x_log10()+
  labs(x="Log10(x)")


x <- c(0:100)

ylo <- 0
yhi <- 1
lambda <-200
phi <- 0.5

lambda2 <-150
phi2 <- 0.25

y <- ylo+(yhi-ylo)*sin(2*(pi/lambda)*x)
y2<- ylo+(yhi-ylo)*sin(2*(pi/lambda2)*x)

df <- data.frame(x, y, y2)

ggplot(data = df, aes(x)) +
  geom_point(aes(y = y, color = "blue", label = "Received Optimum" )) +
  geom_point(aes(y = y2, color = "red")) +
  ylim(0, 1)+
  xlab("Growing season length")+
  ylab("VI")

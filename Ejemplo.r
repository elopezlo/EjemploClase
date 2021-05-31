require(gganimate)
x <- seq(-4, 4, 0.1)
Y <- dnorm(x)
df <- seq(2, 50, 2)

Dt <- purrr::map_df(df, function(xi)data.frame(X = x, Y = dt(x = x, df = xi),
         Gl = xi, Dist = "t-studen") )

Dt  <- rbind(data.frame(X = x, Y = Y, Gl = 0, Dist = "Normal"), Dt)

g <- ggplot(Dt, aes(x = X, y = Y, colour = Dist, 
     group = Gl)) + geom_line() + geom_area(alpha = 0.4) +
     theme_bw(20) + theme(legend.position="top") + labs(colour = "", x = "x",
     y = "") 

   
p <- g + transition_reveal(Gl)
+ 
    shadow_wake(wake_length = 0.1, alpha = FALSE) + 
    ease_aes('linear')
gg_animate(p)
gg_animate(p, 100, 10)


normCurves <-
  data.frame(x = rep(seq(-6, 18, length.out = 1001)
    , times = 7)
    , myMean = rep(seq(0, 12, 2)
      , each = 1001))

toAnimate <-
  ggplot(normCurves
    , aes(x
      , group = myMean
      , frame = myMean)) +
  geom_line(aes(y = density)) +
  geom_area(aes(y = toHighlight)
    , position = "identity") +
  ggtitle("Mean = ")



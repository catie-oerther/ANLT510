server = function(input, output, session) {

  x_i <- reactive({seq(1, 6, by = 0.5)})
  noise <- reactive({rnorm(length(x_i()), 0, input$sigma)})
  y_i <- reactive({5 * x_i() + 3 + noise()})


  output$sumd <- renderUI({
    HTML(glue::glue("<h3>&Sigma;(distance)&emsp;= {round(sum(noise()),digits=5)}</h3>"))})
  output$sumda <- renderUI({
    HTML(glue::glue("<h3 style='color: red;'>&Sigma;(|distance|) = {round(sum(abs(noise())),digits=5)}</h3>"))})
  output$sumd2 <- renderUI({
    HTML(glue::glue("<h3 style='color:#00e500;'>&Sigma;(distance)<sup>2</sup>&nbsp= {round(sum(noise()^2),digits=5)}</h3>"))})

  output$plot3d <- renderPlot({

func_absolute <- function(params,x,y) {

     m <- params[1]
     b <- params[2]

     return(sum(abs(y - m * x - b)))

}

func_convex <- function(params,x,y) {

     m <- params[1]
     b <- params[2]

     return(sum((y - m * x - b)^2))

}

fit_absolute <- optim(par = c(1,1),
                      fn = func_absolute,
                      x = x_i(),
                      y = y_i(),
                      control = list(fnscale = 1))$par

fit_convex <- optim(par = c(1,1),
                    fn = func_convex,
                    x = x_i(),
                    y = y_i(),
                    control = list(fnscale = 1))$par

par(cex.axis = 1.1, cex.lab = 1.1, font = 2, las = 1, lwd = 2,mfrow = c(1,2))

plot(NA,
      xlim = c(0,6),
      ylim = c(0,40),
      ylab = expression(y == m[absolute]%*%x + b[absolute]))

if(as.character(input$showline) == "Yes"){

curve(fit_absolute[1] * x + fit_absolute[2],
      col = "red",
      add = T)

segments(x0 = x_i(),
         y0 = fit_absolute[1] * x_i() + fit_absolute[2],
         x1 = x_i(),
         y1 = y_i())

}

points(x = x_i(),
       y = y_i(),
       pch = 16,
       col = 4,
       cex = 1.5)

text(x = c(0.5,1.25), y = rep(32.5+5,2), c("slope  =", round(fit_absolute[1], digits = 5) ))
text(x = c(0.5,1.25), y = rep(27.5+5,2), c("intcpt =", round(fit_absolute[2], digits = 5) ))



plot(NA,
      xlim = c(0,6),
      ylim = c(0,40),
      ylab = expression(y == m[convex]%*%x + b[convex]))

if(as.character(input$showline) == "Yes"){

curve(fit_convex[1] * x + fit_convex[2],
      col = 'green',
      add = T)

segments(x0 = x_i(),
         y0 = fit_convex[1] * x_i() + fit_convex[2],
         x1 = x_i(),
         y1 = y_i())

}

points(x = x_i(),
       y = y_i(),
       pch = 16,
       col = 4,
       cex = 1.5)

text(x = c(0.5,1.25), y = rep(32.5+5,2), c("slope  =",round(fit_convex[1], digits = 5) ))
text(x = c(0.5,1.25), y = rep(27.5+5,2), c("intcpt =",round(fit_convex[2], digits = 5) ))

})
}

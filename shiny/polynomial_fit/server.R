server = function(input, output, session) {

  x_i <- reactive({seq(0, 6, by = 0.5)})
  noise <- reactive({rnorm(length(x_i()), 0, input$sigma2)})
  y_i <- reactive({5 * x_i() + 3 + noise()})


  # output$sumd <- renderUI({
  #   HTML(glue::glue("<h3>&Sigma;(distance)&emsp;= {round(sum(noise()),digits=5)}</h3>"))})
  # output$sumda <- renderUI({
  #   HTML(glue::glue("<h3 style='color: red;'>&Sigma;(|distance|) = {round(sum(abs(noise())),digits=5)}</h3>"))})
  # output$sumd2 <- renderUI({
  #   HTML(glue::glue("<h3 style='color:#00e500;'>&Sigma;(distance)<sup>2</sup>&nbsp= {round(sum(noise()^2),digits=5)}</h3>"))})

  output$plot3d2 <- renderPlot({

    N = input$degree + 1

terms = character(N)

for(i in 1:N){

    terms[i] = glue::glue("const[{i}] * x ^ ({i} - 1)")

}

fun <- function(y, x,const) {

  Fun <- glue::glue("sum( (y - ( {paste(terms, collapse = ' + ')} ) ) ^ 2)")

  return(eval(parse(text = Fun)))

  }

res = nlminb(start = rnorm(N),
              objective = fun,
              x = x_i(),
              y = y_i())[1:5]

plt_fun <- function(x,const) return(eval(parse(text = paste(terms, collapse = ' + '))))

par(cex.axis = 1.1, cex.lab = 1.1, font = 2, las = 1, lwd = 2)

curve(plt_fun(x,res$par),
      xlim = c(0,6),
      ylim = c(0,35),
      n = 1000,
      lwd = 2)

points(x_i(),
       y_i(),
       col = 4,
       cex = 1.5,
       pch = 16)

params = round(res$par, digits = 3)
n_params = length(params)
powers = 0:n_params
trms = character(n_params)
trms[1] = paste(params[1])
for(i in 2:n_params){

  trms[i] = glue::glue("{params[i]}*x^{powers[i]}")

}

text(x = 0,
     y = 0.5,
     parse(text = paste(trms, collapse = " + ")),
     cex = 1.5,
     font = 2,
     adj = 0)
text(x = c(0.75,0.75),
     y = c(30,27),
     c(expression(bold(underline("SSE"))), round(res$objective,4)),
     cex = c(1.5,1.5),
     font = 2,
     adj = 0)

})
}

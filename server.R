
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
p <- plot_ly()

shinyServer(function(input, output) {
  
  ### bernoulli
  output$bern.dist <- renderText(
    paste0("B(",input$bern.p,")")
  )
  output$bern.plot <- renderPlot({
    par(mfrow=c(1,2))
    plot(x=c(0,1), y=c(1-input$bern.p, input$bern.p), xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    plot(x=c(0,1), y=cumsum(c(1-input$bern.p, input$bern.p)), xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
  })
  
  ### binomial
  output$binom.dist <- renderText(
    paste0("Binom(",input$binom.n,",",input$binom.p,")")
  )
  output$binom.plot <- renderPlot({
    par(mfrow=c(1,2))
    plot(x=0:input$binom.n, y=dbinom(0:input$binom.n, size=input$binom.n, prob=input$binom.p), 
         xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    if (input$norm.approx.binom){
      curve(dnorm(x, input$binom.n*input$binom.p, sqrt(input$binom.n*input$binom.p*(1-input$binom.p))), col="red", lty=3, add=TRUE)
    }
    plot(x=0:input$binom.n, y=cumsum(dbinom(0:input$binom.n, size=input$binom.n, prob=input$binom.p)), 
         xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
    if (input$norm.approx.binom){
      curve(pnorm(x, input$binom.n*input$binom.p, sqrt(input$binom.n*input$binom.p*(1-input$binom.p))), col="red", lty=3, add=TRUE)
    }
  })
  
  ### poisson
  output$pois.dist <- renderText(
    paste0("Pois(",input$pois.lambda,")")
  )
  output$pois.plot <- renderPlot({
    par(mfrow=c(1,2))
    upper <- qpois(0.999, input$pois.lambda)
    plot(x=0:upper, y=dpois(0:upper, lambda=input$pois.lambda), xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    if (input$norm.approx.pois){
      curve(dnorm(x, input$pois.lambda, sqrt(input$pois.lambda)), col="red", lty=3, add=TRUE)
    }
    plot(x=0:upper, y=ppois(0:upper, lambda=input$pois.lambda), xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
    if (input$norm.approx.pois){
      curve(pnorm(x, input$pois.lambda, sqrt(input$pois.lambda)), col="red", lty=3, add=TRUE)
    }
  })
  
  ### geometric
  output$geom.dist <- renderText(
    paste0("Geom(",input$geom.p,")")
  )
  output$geom.plot <- renderPlot({
    par(mfrow=c(1,2))
    upper <- qgeom(0.999, input$geom.p)
    plot(x=0:upper, y=dgeom(0:upper, prob=input$geom.p), xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    plot(x=0:upper, y=pgeom(0:upper, prob=input$geom.p), xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
  })
  
  ### negative binomial
  output$nb.dist <- renderText(
    paste0("NB(",input$nb.r,",",input$nb.p,")")
  )
  output$nb.plot <- renderPlot({
    par(mfrow=c(1,2))
    upper <- qnbinom(0.999, size=input$nb.r, prob=input$nb.p)
    plot(x=0:upper, y=dnbinom(0:upper, size=input$nb.r, prob=input$nb.p), xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    plot(x=0:upper, y=cumsum(dnbinom(0:upper, size=input$nb.r, prob=input$nb.p)), xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
  })
  
  ### hypergeometric
  output$hg.dist <- renderText(
    paste0("HyperGeom(",input$hg.N,",",input$hg.K,",",input$hg.n,")")
  )
  output$hg.plot <- renderPlot({
    par(mfrow=c(1,2))
    N <- input$hg.N ; K <- input$hg.K ; n <- input$hg.n
    lower <- max(0, n+K-N)
    upper <- min(K, n)
    # rhyper(x= # of white balls, m= # of white balls, n= # of black balls, k= # of balls drawn)
    plot(x=lower:upper, y=dhyper(x=lower:upper, m=K, n=N-K, k=n), xlab="", ylab="", type="h", col="blue", lwd=2, main="pmf")
    plot(x=lower:upper, y=cumsum(dhyper(x=lower:upper, m=K, n=N-K, k=n)), xlab="", ylab="", type="s", col="blue", lwd=2, main="cdf")
  })
  
  ### continuous ###
  
  ### uniform
  output$unif.dist <- renderText(
    paste0("Uniform(",input$unif.a,",",input$unif.b,")")
  )
  output$unif.plot <- renderPlot({
    lower <- input$unif.a
    upper <- input$unif.b
    par(mfrow=c(1,2))
    curve(dunif(x, input$unif.a, input$unif.b), xlim = c(lower, upper), xlab="", ylab="", main="pdf", col="blue")
    curve(punif(x, input$unif.a, input$unif.b), xlim = c(lower, upper), xlab="", ylab="", main="cdf", col="blue")
  })
  
  ### normal
  output$unif.dist <- renderText(
    paste0("Normal(",input$norm.mu,",",input$norm.sigma,")")
  )
  output$norm.plot <- renderPlot({
    lower <- qnorm(0.001, input$norm.mu, input$norm.sigma)
    upper <- qnorm(0.999, input$norm.mu, input$norm.sigma)
    par(mfrow=c(1,2))
    curve(dnorm(x, input$norm.mu, input$norm.sigma), xlim = c(lower, upper), xlab="", ylab="", main="pdf", col="blue")
    curve(pnorm(x, input$norm.mu, input$norm.sigma), xlim = c(lower, upper), xlab="", ylab="", main="cdf", col="blue")
  })
  
  ### t
  output$unif.dist <- renderText(
    paste0("t(",input$t.nu,")")
  )
  output$t.plot <- renderPlot({
    lower <- qt(0.01, input$t.nu)
    upper <- qt(0.99, input$t.nu)
    par(mfrow=c(1,2))
    curve(dt(x, input$t.nu), xlim = c(lower, upper), ylim=c(0,dnorm(0,0,1)), xlab="", ylab="", main="pdf", col="blue")
    if (input$norm.approx.t){
      curve(dnorm(x, 0, 1), col="red", lty=3, add=TRUE)
    }
    curve(pt(x, input$t.nu), xlim = c(lower, upper), ylim=c(0,1), xlab="", ylab="", main="cdf", col="blue")
    if (input$norm.approx.t){
      curve(pnorm(x, 0, 1), col="red", lty=3, add=TRUE)
    }
  })
  
  ### exponential
  output$unif.dist <- renderText(
    paste0("Exponential(",input$exp.lambda,")")
  )
  output$exp.plot <- renderPlot({
    lower <- 0
    upper <- qexp(0.999, input$exp.lambda)
    par(mfrow=c(1,2))
    curve(dexp(x, input$exp.lambda), xlim = c(lower, upper), xlab="", ylab="", main="pdf", col="blue")
    curve(pexp(x, input$exp.lambda), xlim = c(lower, upper), xlab="", ylab="", main="cdf", col="blue")
  })
  
  ### gamma
  output$unif.dist <- renderText(
    paste0("Gamma(",input$gamma.alpha,",",input$gamma.beta,")")
  )
  output$gamma.plot <- renderPlot({
    lower <- 0
    upper <- qgamma(0.999, input$gamma.alpha, input$gamma.beta)
    par(mfrow=c(1,2))
    curve(dgamma(x, input$gamma.alpha, input$gamma.beta), xlim = c(lower, upper), xlab="", ylab="", main="pdf", col="blue")
    curve(pgamma(x, input$gamma.alpha, input$gamma.beta), xlim = c(lower, upper), xlab="", ylab="", main="cdf", col="blue")
  })
  
  ### chi-sq
  output$chisq.dist <- renderText(
    paste0("Chi-sq(",input$chisq.k,")")
  )
  output$chisq.plot <- renderPlot({
    lower <- 0
    upper <- qchisq(0.999, input$chisq.k)
    par(mfrow=c(1,2))
    curve(dchisq(x, input$chisq.k), xlim = c(lower, upper), xlab="", ylab="", main="pdf", col="blue")
    curve(pchisq(x, input$chisq.k), xlim = c(lower, upper), xlab="", ylab="", main="cdf", col="blue")
  })
  
  ### beta
  output$beta.dist <- renderText(
    paste0("Beta(",input$beta.a,",",input$beta.b,")")
  )
  output$beta.plot <- renderPlot({
    par(mfrow=c(1,2))
    curve(dbeta(x, input$beta.a, input$beta.b), xlim = c(0,1), xlab="", ylab="", main="pdf", col="blue")
    curve(pbeta(x, input$beta.a, input$beta.b), xlim = c(0,1), xlab="", ylab="", main="cdf", col="blue")
  })
  
  
  ###
  output$bernoulli <- renderUI(withMathJax(paste0("Probability mass function: 
                                                  $$f(k;p)=p^k(1-p)^k$$")))
  output$binomial <- renderUI(withMathJax(helpText("Probability mass function: 
                                                   $$f(k;n,p)={n \\choose k}p^k(1-p)^{n-k}$$")))
  output$poisson <- renderUI(withMathJax(paste0("Probability mass function: 
                                                $$f(k;\\lambda)=e^{-\\lambda}\\frac{\\lambda^{k}}{k!}$$")))
  output$geometric <- renderUI(withMathJax(paste0("Probability mass function: 
                                                  $$f(k;p)=(1-p)^kp$$")))
  output$nb <- renderUI(withMathJax(paste0("Probability mass function: 
                                           $$f(k;r,p)={k+r-1 \\choose k}p^k(1-p)^r$$")))
  output$hg <- renderUI(withMathJax(paste0("Probability mass function: 
                                           $$f(k;N,K,n)=\\frac{{K \\choose k}{N-K \\choose n-k}}{{N \\choose n}}$$")))
  
  output$uniform <- renderUI(withMathJax(paste0("Probability density function: $$f(x;a,b)=
                                                \\begin{cases} 
                                                \\frac{1}{b-a} \\textrm{ , } a\\leq x \\leq b \\\\ 
                                                0 \\textrm{ , otherwise}
                                                \\end{cases}$$")))
  output$normal <- renderUI(withMathJax(paste0("Probability density function: 
                                               $$f(x; \\mu,\\sigma^2)=\\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$")))
  output$t <- renderUI(withMathJax(paste0("Probability density function: 
                                          $$f(x:\\nu)=\\frac{\\Gamma(\\frac{\\nu+1}{2})}{\\sqrt{\\nu\\pi}\\Gamma(\\frac{\\nu}{2})} \\left(1+\\frac{x^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}$$")))
  output$exponential <- renderUI(withMathJax(paste0("$$Exp(\\lambda) = Gamma(1,\\lambda) $$ 
                                                    Probability density function: 
                                                    $$f(x; \\lambda)=\\lambda e^{-\\lambda x}$$")))
  output$gamma <- renderUI(withMathJax(paste0("Probability density function: 
                                              $$f(x; \\alpha,\\beta)=\\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{\\alpha-1}e^{-\\beta x}$$")))
  output$chisq <- renderUI(withMathJax(paste0("$$\\chi^2(k) = Gamma(\\frac{k}{2})$$ 
                                              Probability density function: 
                                              $$f(x; k)=\\frac{1}{2^{k/2}\\Gamma(k/2)}x^{\\frac{k}{2}-1}e^{-\\frac{x}{2}}$$")))
  output$beta <- renderUI(withMathJax(paste0("Probability density function: 
                                             $$f(x; \\alpha,\\beta)=\\frac{\\Gamma(\\alpha)\\Gamma(\\beta)}{\\Gamma(\\alpha+\\beta)}x^{\\alpha-1}(1-x)^{\\beta-1}$$")))
  

})

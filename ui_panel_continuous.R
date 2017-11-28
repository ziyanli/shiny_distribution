shiny_panel_continuous <- tabsetPanel(
  tabPanel(
    "Uniform",
    h5("Uniform distribution"),
    p("A probability distribution where all intervals of same length are equally probable. 
      The support is defined by the minimum value and maximum value ( [a,b] )."),
    uiOutput("uniform"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("unif.a", "a (minumum)", value = 0, min = -Inf, max = Inf),
        numericInput("unif.b", "b (maximum)", value = 1, min = -Inf, max = Inf)
        
      ),
      mainPanel(
        textOutput("unif.dist"),
        plotOutput("unif.plot")
      )
    )
  ),
  tabPanel(
    "Normal",
    h5("Normal distribution"),
    p(""),
    uiOutput("normal"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("norm.mu", "mu (mean)", value = 0, min = -Inf, max = Inf),
        numericInput("norm.sigma", "sigma (standard deviation)", value = 1, min = -Inf, max = Inf)
      ),
      mainPanel(
        textOutput("norm.dist"),
        plotOutput("norm.plot")
      )
    )
  ),
  tabPanel(
    "Student's t",
    h5("Student's t-distribution"),
    p("The probability distribution that people use to estimate the mean of a normally distributed population 
      when the sample size is small and population standard deviation is unknown."),
    uiOutput("t"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("t.nu", "degree of freedom", value = 1, min = 0, max = Inf),
        checkboxInput("norm.approx.t", "add standard normal", value=FALSE)
      ),
      mainPanel(
        textOutput("t.dist"),
        plotOutput("t.plot")
      )
    )
  ),
  
  tabPanel(
    "Exponential",
    h5("Exponential distribution"),
    p("A probability distirbution that describe the waiting time between events in a Poisson Process, 
      where events occur continuously and independently at a constant average rate. Exponential distribution is a special case of Gamma distribution, 
      and it has a key property of being 'memoryless'."),
    uiOutput("exponential"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("exp.lambda", "lambda (rate)", value = 1, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("exp.dist"),
        plotOutput("exp.plot")
      )
    )
  ),
  tabPanel(
    "Gamma",
    h5("Gamma distribution"),
    p("A continuous probability distribution with a 'shape' parameter (denoted by alpha) and a 'rate' parameter (denoted by beta). 
      Exponential distribution and Chi-sq distribution are both special cases of Gamma distribution."),
    uiOutput("gamma"),
    p("*Note that there are different parametrization."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("gamma.alpha", "alpha (shape)", value = 1, min = 0, max = 50),
        numericInput("gamma.beta", "beta (rate)", value = 1, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("gamma.dist"),
        plotOutput("gamma.plot")
      )
    )
  ),
  tabPanel(
    "Chi-sq",
    h5("Chi-squared distribution"),
    p("Probability distribution of a sum of squares of k independent standard normal random variables. 
      Chi-square distribution is a special case of Gamma distribution and is widely used in hypothesis testing."),
    uiOutput("chisq"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("chisq.k", "degree of freedom", value = 1, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("chisq.dist"),
        plotOutput("chisq.plot")
      )
    )
  ),
  tabPanel(
    "Beta",
    h5("Beta distribution"),
    p("A continuous probability distribution defined on the interval [0,1], parametrized by two positive shape parameters (denoted by alpha and beta). 
      We can use Beta distribution to model random variables limited to the interval [0,1], for instance, rate or proportion. 
      It is widely used in Bayesian inference, adopted as the conjugate prior of various common distributions."),
    uiOutput("beta"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("beta.a", "alpha (shape)", value = 1, min = 0, max = Inf),
        numericInput("beta.b", "beta (shape)", value = 1, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("beta.dist"),
        plotOutput("beta.plot")
      )
    )
  )
  
  
)

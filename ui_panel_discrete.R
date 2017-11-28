shiny_panel_discrete <- tabsetPanel(
  tabPanel(
    "Bernoulli",
    h5("Bernoulli distribution"),
    p("A random variable that takes value 1 with probability p and value 0 with probability (1-p)."),
    uiOutput("bernoulli"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        sliderInput("bern.p", "p (probability)", value = 0.5, min = 0, max = 1)
      ),
      mainPanel(
        textOutput("bern.dist"),
        plotOutput("bern.plot")
      )
    )
  ),
  
  tabPanel(
    "Binomial",
    h5("Binomial distribution"),
    p("Number of successes in a sequence of independent and identical Bernoulli experiments. 
      'p' is the probability of success and 'n' is the number of trials."),
    uiOutput("binomial"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("binom.n", "n (size)", value = 30, min = 0, max = Inf),
        sliderInput("binom.p", "p (probability)", value = 0.5, min = 0, max = 1),
        checkboxInput("norm.approx.binom", "normal approximation", value=FALSE)
      ),
      mainPanel(
        textOutput("binom.dist"),
        plotOutput("binom.plot")
      )
    )
  ),
  
  tabPanel(
    "Poisson",
    h5("Poisson distribution"),
    p("The probability distribution for the occurances of some rare event in a fixed interval of time or space if the even 
      t occurs with a known rate (denoted by lambda) and independently of the time since last event."),
    uiOutput("poisson"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("pois.lambda", "lambda (rate)", value = 1, min = 0, max = Inf),
        checkboxInput("norm.approx.pois", "normal approximation", value=FALSE)
      ),
      mainPanel(
        textOutput("pois.dist"),
        plotOutput("pois.plot")
      )
    )
  ),
  
  tabPanel(
    "Geometric",
    h5("Geometric distribution"),
    p("The probability distribution of number of Bernoulli trials needed to get one success.  
      (The probability distribution of number of failures before the first success).
      'p' is the probability of success."),
    uiOutput("geometric"),
    p("*Note that there is an alternative to define a Geometric distribution."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        sliderInput("geom.p", "p (probability)", value = 0.5, min = 0, max = 1)
      ),
      mainPanel(
        textOutput("geom.dist"),
        plotOutput("geom.plot")
      )
    )
  ),
  
  tabPanel(
    "Negative Binomial",
    h5("Negative binomial distribution"),
    p("The probability distribution of number of Bernoulli trials needed before a specified number of successes (denoted by r). 
      'p' is the probability of success, 'r' is the number of successes." ),
    uiOutput("nb"),
    p("*Note that there are alternatives to define a Negative binomial distribution."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        sliderInput("nb.p", "p (probability)", value = 0.5, min = 0, max = 1),
        numericInput("nb.r", "r (number of successes)", value = 1, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("nb.dist"),
        plotOutput("nb.plot")
      )
    )
  ),
  
  tabPanel(
    "Hypergeometric",
    h5("Hypergeometric distribution"),
    p("Consider a finite population with size N where K objects has come certain features. 
      If we draw a sample od size n without replacement, the probability distribution of number of featured objects in our sample is a hypergeometric distribution." ),
    uiOutput("hg"),
    p("*Note that there are alternatives to define a Negative binomial distribution."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        "Parameters",
        hr(),
        numericInput("hg.N", "N (population size)", value = 5, min = 0, max = Inf),
        numericInput("hg.K", "K (number of featured objects in population)", value = 3, min = 0, max = Inf),
        numericInput("hg.n", "n (sample size)", value = 2, min = 0, max = Inf)
      ),
      mainPanel(
        textOutput("hg.dist"),
        plotOutput("hg.plot")
      )
    )
  )
  
)

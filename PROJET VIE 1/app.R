library(shiny)
library(ggplot2)
source("R/actuarial_functions.R")
ui <- fluidPage(
    titlePanel(
        withMathJax(
            "Fonctions de perte \\(L(T)\\) et \\(F_L(l)\\)"
        )
    ),

    sidebarLayout(
        sidebarPanel(
            selectInput(
                "contract",
                "Contrat",
                c("Assurance", "Rente")
            ),

            selectInput(
                "law",
                "Loi de mortalité",
                c("Exponentielle", "De Moivre")
            ),

            numericInput(
                "delta",
                withMathJax("Force d'intérêt \\(\\delta\\)"),
                0.1,
                0.0001,
                1,
                0.005
            ),

            conditionalPanel(
                "input.law == 'Exponentielle'",

                numericInput(
                    "mu",
                    withMathJax("Force de mortalité \\(\\mu\\)"),
                    0.04,
                    0.0001,
                    1,
                    0.005
                )
            ),

            conditionalPanel(
                "input.law == 'De Moivre'",

                numericInput(
                    "omega",
                    withMathJax("Âge limite \\(\\omega\\)"),
                    100,
                    1,
                    150,
                    1
                ),

                numericInput(
                    "x",
                    withMathJax("Âge actuel \\(x\\)"),
                    40,
                    0,
                    149,
                    1
                )
            ),

            numericInput(
                "b",
                withMathJax("Prestation ou taux annuel \\(b\\)"),
                1000,
                1,
                1e8,
                1000
            ),

            numericInput(
                "m",
                withMathJax("Période différée \\(m\\)"),
                20,
                0,
                100,
                1
            ),

            numericInput(
                "n",
                withMathJax("Période temporaire \\(n\\)"),
                25,
                1,
                100,
                1
            ),

            numericInput(
                "h",
                withMathJax("Période de paiement des primes \\(h\\)"),
                15,
                1,
                100,
                1
            ),

            radioButtons(
                "premium_mode",
                withMathJax("Prime \\(\\Pi\\)"),
                c(
                    "Équivalence" = "eq",
                    "Manuelle" = "manual"
                ),
                inline = TRUE
            ),

            conditionalPanel(
                "input.premium_mode == 'manual'",

                numericInput(
                    "pi_manual",
                    withMathJax("Prime annuelle \\(\\Pi\\)"),
                    2,
                    0.01,
                    1e8,
                    100
                )
            ),

            actionButton(
                "calc",
                "Calculer",
                class = "btn-primary"
            )
        ),

        mainPanel(
            uiOutput("validation"),

            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4(
                            withMathJax(
                                "Prime d'équivalence calculée \\(\\Pi^{\\mathrm{eq}}\\)"
                            )
                        ),
                        textOutput("premium_eq")
                    )
                ),

                column(
                    6,
                    wellPanel(
                        h4(
                            withMathJax(
                                "Tranche valide de la prime \\(\\Pi\\)"
                            )
                        ),
                        textOutput("premium_range")
                    )
                )
            ),

            tabsetPanel(
                tabPanel(
                    withMathJax("\\(L(T)\\)"),
                    plotOutput(
                        "loss_plot",
                        height = 500
                    )
                ),

                tabPanel(
                    withMathJax("\\(F_L(l)\\)"),
                    plotOutput(
                        "cdf_plot",
                        height = 500
                    )
                )
            )
        )
    )
)
server <- function(input,output,session){
    p <- eventReactive(input$calc,{
        pi_eq <- premium_equivalence(input$contract,input$b,input$delta,input$m,input$n,input$h,input$law,input$mu,input$omega,input$x)
        pi <- if(input$premium_mode=="manual") input$pi_manual else pi_eq
        range <- valid_premium_interval(input$contract,input$b,input$delta,input$m,input$n,input$h)
        e <- validate_parameters(input$contract,input$law,input$delta,input$mu,input$omega,input$x,input$m,input$n,input$h,input$b,pi)
        if(is.finite(pi) && (pi <= range$lower || pi >= range$upper))
            e <- c(e, "La prime choisie ne se trouve pas dans la tranche valide.")
        list(contract=input$contract,law=input$law,delta=input$delta,mu=input$mu,omega=input$omega,x=input$x,b=input$b,m=input$m,n=input$n,h=input$h,pi=pi,pi_eq=pi_eq,range=range,e=e)
    },ignoreNULL=FALSE)
    output$validation <- renderUI({if(length(p()$e))div(class="alert alert-danger",tags$ul(lapply(p()$e,tags$li)))})
    output$premium_eq <- renderText({format(round(p()$pi_eq,4),big.mark=" ",decimal.mark=",")})
    output$premium_range <- renderText({
        z <- p()$range
        lo <- format(round(z$lower,4),big.mark=" ",decimal.mark=",")
        if(is.infinite(z$upper)) paste0("Π > ",lo) else {
            hi <- format(round(z$upper,4),big.mark=" ",decimal.mark=",")
            paste0(lo," < Π < ",hi)
        }
    })
    output$loss_plot <- renderPlot({
        z<-p();req(!length(z$e));H<-if(z$law=="Exponentielle")max(z$m+z$n,z$h)+max(10,3/z$mu)else z$omega-z$x
        cuts <- sort(unique(c(0, z$h, z$m, z$m + z$n, H)))
        cuts <- cuts[cuts >= 0 & cuts <= H]
        eps <- max(H, 1) * 1e-7
        pieces <- lapply(seq_len(length(cuts) - 1), function(i) {
            left <- cuts[i]
            right <- cuts[i + 1]
            tt <- seq(left + if(i > 1) eps else 0, right - eps, length.out = 500)
            data.frame(T = tt, L = loss_value(tt, z$contract, z$b, z$pi, z$delta, z$m, z$n, z$h), piece = i)
        })
        d <- do.call(rbind, pieces)
        # Bornes de l'assurance : m < T <= m+n.
        # À m, la branche sans prestation est fermée et la branche avec prestation est ouverte.
        # À m+n, la branche avec prestation est fermée et la branche sans prestation est ouverte.
        endpoints <- NULL
        if(z$contract == "Assurance") {
            endpoint_list <- list()

            if(z$m >= 0 && z$m < H) {
                no_benefit_m <- -z$pi * annuity_cont(pmin(z$m, z$h), z$delta)
                benefit_after_m <- z$b * exp(-z$delta * z$m) -
                    z$pi * annuity_cont(pmin(z$m, z$h), z$delta)
                endpoint_list[[length(endpoint_list) + 1]] <- data.frame(
                    T = c(z$m, z$m),
                    L = c(no_benefit_m, benefit_after_m),
                    closed = c(TRUE, FALSE)
                )
            }

            upper <- z$m + z$n
            if(upper > 0 && upper < H) {
                benefit_at_upper <- z$b * exp(-z$delta * upper) -
                    z$pi * annuity_cont(pmin(upper, z$h), z$delta)
                no_benefit_after <- -z$pi * annuity_cont(pmin(upper, z$h), z$delta)
                endpoint_list[[length(endpoint_list) + 1]] <- data.frame(
                    T = c(upper, upper),
                    L = c(benefit_at_upper, no_benefit_after),
                    closed = c(TRUE, FALSE)
                )
            }

            if(length(endpoint_list)) endpoints <- do.call(rbind, endpoint_list)
        }
        g <- ggplot(d, aes(T, L, group = piece)) +
            geom_line(
                linewidth = 1,
                color = "#136f8a"
            )

        if (!is.null(endpoints)) {
            g <- g +
                geom_point(
                    data = endpoints[endpoints$closed, ],
                    aes(T, L),
                    inherit.aes = FALSE,
                    shape = 16,
                    size = 3,
                    color = "#136f8a"
                ) +
                geom_point(
                    data = endpoints[!endpoints$closed, ],
                    aes(T, L),
                    inherit.aes = FALSE,
                    shape = 21,
                    size = 3,
                    stroke = 1,
                    fill = "white",
                    color = "#136f8a"
                )
        }

        g +
            labs(
                x = "T",
                y = "L(T)"
            ) +
            scale_x_continuous(
                breaks = function(x) pretty(x, n = 12),
                minor_breaks = function(x) pretty(x, n = 24)
            ) +
            scale_y_continuous(
                breaks = function(x) pretty(x, n = 12),
                minor_breaks = function(x) pretty(x, n = 24),
                labels = scales::label_number(
                    big.mark = " ",
                    decimal.mark = ","
                )
            ) +
            theme_minimal(base_size = 13)
    })

    output$cdf_plot <- renderPlot({
        z <- p()
        req(!length(z$e))

        d <- loss_distribution(
            z$contract,
            z$b,
            z$pi,
            z$delta,
            z$m,
            z$n,
            z$h,
            z$law,
            z$mu,
            z$omega,
            z$x
        )

        valide <- is.finite(d$loss) &
            is.finite(d$weight) &
            d$weight > 0

        loss <- d$loss[valide]
        weight <- d$weight[valide]

        ordre <- order(loss)
        loss <- loss[ordre]
        weight <- weight[ordre]

        id <- match(loss, unique(loss))
        loss_unique <- loss[!duplicated(loss)]

        poids_groupes <- as.numeric(rowsum(weight, id, reorder = FALSE))
        frequences <- tabulate(id, nbins = length(loss_unique))

        poids_groupes <- poids_groupes / sum(poids_groupes)

        cdf_apres <- cumsum(poids_groupes)
        cdf_avant <- c(0, head(cdf_apres, -1))

        poids_typique <- median(weight, na.rm = TRUE)

        seuil_masse <- max(
            20 * poids_typique,
            1e-8
        )

        est_masse <- frequences > 1 |
            poids_groupes > seuil_masse

        masses <- data.frame(
            loss = loss_unique[est_masse],
            cdf_avant = cdf_avant[est_masse],
            cdf_apres = cdf_apres[est_masse]
        )

        l_min <- min(loss_unique)
        l_max <- max(loss_unique)
        etendue_l <- l_max - l_min

        if (!is.finite(etendue_l) || etendue_l <= 0) {
            etendue_l <- max(abs(c(l_min, l_max)), 1)
        }

        limite_gauche <- min(
            0,
            l_min - 0.10 * etendue_l
        )

        limite_droite <- max(
            0,
            l_max + 0.05 * etendue_l
        )

        groupe <- cumsum(est_masse) + 1L

        courbe_cdf <- data.frame(
            loss = loss_unique,
            cdf = cdf_apres,
            groupe = groupe
        )

        if (any(est_masse)) {
            points_avant <- data.frame(
                loss = loss_unique[est_masse],
                cdf = cdf_avant[est_masse],
                groupe = groupe[est_masse] - 1L
            )

            courbe_cdf <- rbind(
                courbe_cdf,
                points_avant
            )
        }

        courbe_cdf <- rbind(
            data.frame(
                loss = limite_gauche,
                cdf = 0,
                groupe = 1L
            ),
            courbe_cdf,
            data.frame(
                loss = limite_droite,
                cdf = 1,
                groupe = max(groupe)
            )
        )

        courbe_cdf <- courbe_cdf[
            order(
                courbe_cdf$groupe,
                courbe_cdf$loss,
                courbe_cdf$cdf
            ),
        ]

        max_points <- 5000L

        if (nrow(courbe_cdf) > max_points) {
            points_obligatoires <- which(
                courbe_cdf$loss %in% masses$loss |
                    courbe_cdf$loss == limite_gauche |
                    courbe_cdf$loss == limite_droite
            )

            points_reduits <- unique(
                round(
                    seq(
                        1,
                        nrow(courbe_cdf),
                        length.out = max_points
                    )
                )
            )

            courbe_cdf <- courbe_cdf[
                sort(unique(c(points_reduits, points_obligatoires))),
            ]
        }

        graphique <- ggplot() +
            geom_line(
                data = courbe_cdf,
                aes(
                    x = loss,
                    y = cdf,
                    group = groupe
                ),
                linewidth = 0.8,
                lineend = "butt",
                linejoin = "round",
                color = "#4b2e83"
            )

        if (nrow(masses) > 0) {
            graphique <- graphique +
                geom_point(
                    data = masses,
                    aes(
                        x = loss,
                        y = cdf_avant
                    ),
                    inherit.aes = FALSE,
                    shape = 21,
                    size = 3,
                    stroke = 0.9,
                    fill = "white",
                    color = "#4b2e83"
                ) +
                geom_point(
                    data = masses,
                    aes(
                        x = loss,
                        y = cdf_apres
                    ),
                    inherit.aes = FALSE,
                    shape = 16,
                    size = 3,
                    color = "#4b2e83"
                )
        }

        graphique +
            geom_vline(
                xintercept = 0,
                linewidth = 0.5,
                color = "black"
            ) +
            labs(
                x = "l",
                y = expression(F[L](l))
            ) +
            scale_x_continuous(
                breaks = function(x) pretty(x, n = 12),
                minor_breaks = function(x) pretty(x, n = 24),
                labels = scales::label_number(
                    big.mark = " ",
                    decimal.mark = ","
                ),
                expand = expansion(mult = c(0, 0))
            ) +
            scale_y_continuous(
                breaks = seq(0, 1, by = 0.1),
                minor_breaks = seq(0, 1, by = 0.05),
                labels = scales::label_number(
                    accuracy = 0.1,
                    decimal.mark = ","
                ),
                expand = expansion(mult = c(0, 0))
            ) +
            coord_cartesian(
                xlim = c(limite_gauche, limite_droite),
                ylim = c(-0.04, 1.02),
                clip = "on"
            ) +
            theme_minimal(base_size = 13) +
            theme(
                axis.line = element_blank()
            )
    })
}
shinyApp(ui,server)

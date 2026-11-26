annuity_cont <- function(t, delta) (1 - exp(-delta * pmax(t, 0))) / delta
survival_T <- function(t, law, mu, omega, x) {
  if (law == "Exponentielle") exp(-mu * pmax(t, 0)) else pmax(1 - pmax(t, 0)/(omega-x), 0)
}
density_T <- function(t, law, mu, omega, x) {
  if (law == "Exponentielle") mu*exp(-mu*pmax(t,0)) else ifelse(t>=0 & t<=omega-x, 1/(omega-x), 0)
}
premium_equivalence <- function(contract, b, delta, m, n, h, law, mu, omega, x) {
  S <- function(t) survival_T(t, law, mu, omega, x)
  max_age <- if (law == "Exponentielle") max(m + n, h) else omega - x
  prem_epv <- integrate(function(s) exp(-delta * s) * S(s), 0,
                        min(h, max_age), subdivisions = 1000)$value
  if (contract == "Assurance") {
    upper <- min(m + n, max_age)
    benefit_epv <- if (upper <= m) 0 else integrate(
      function(t) b * exp(-delta * t) * density_T(t, law, mu, omega, x),
      m, upper, subdivisions = 1000
    )$value
  } else {
    upper <- min(m + n, max_age)
    benefit_epv <- if (upper <= m) 0 else integrate(
      function(s) b * exp(-delta * s) * S(s), m, upper,
      subdivisions = 1000
    )$value
  }
  if (prem_epv <= 0) return(NA_real_)
  benefit_epv / prem_epv
}

loss_value <- function(T,contract,b,pi,delta,m,n,h) {
  premiums <- pi*annuity_cont(pmin(T,h),delta)
  benefits <- if(contract=="Assurance") ifelse(T>m & T<=m+n,b*exp(-delta*T),0) else
    b*exp(-delta*m)*annuity_cont(pmax(pmin(T,m+n)-m,0),delta)
  benefits-premiums
}
loss_distribution <- function(contract,b,pi,delta,m,n,h,law,mu,omega,x,N=15000) {
  H <- if(law=="Exponentielle") max(m+n,h) else omega-x
  cuts <- sort(unique(c(0,h,m,m+n,H))); cuts <- cuts[cuts>=0 & cuts<=H]
  t <- unique(sort(c(seq(0,H,length.out=N),cuts)))
  mid <- head(t,-1)+diff(t)/2
  w <- density_T(mid,law,mu,omega,x)*diff(t)
  L <- loss_value(mid,contract,b,pi,delta,m,n,h)
  tail <- survival_T(H,law,mu,omega,x)
  if(tail>1e-12){L<-c(L,loss_value(H+1,contract,b,pi,delta,m,n,h));w<-c(w,tail)}
  o<-order(L); data.frame(loss=L[o],weight=w[o],cdf=cumsum(w[o])/sum(w))
}
validate_parameters <- function(contract,law,delta,mu,omega,x,m,n,h,b,pi) {
  e<-character(); H<-if(law=="Exponentielle") 100 else omega-x
  if(delta<=0)e<-c(e,"delta doit être positif."); if(b<=0)e<-c(e,"b doit être positif.")
  if(!is.finite(pi)||pi<=0)e<-c(e,"La prime doit être positive.")
  if(law=="Exponentielle"&&mu<=0)e<-c(e,"mu doit être positif.")
  if(law=="De Moivre"&&omega<=x)e<-c(e,"Il faut omega > x.")
  if(m<0||m>=H)e<-c(e,"m est hors de l'horizon permis.")
  if(n<=0||m+n>H)e<-c(e,"n est invalide ou m+n dépasse l'horizon.")
  if(h<=0||h>m+n)e<-c(e,"h doit être positif et inférieur ou égal à m+n.")
  if(contract=="Rente"&&h>m)e<-c(e,"Pour une rente, h doit être inférieur ou égal à m.")
  e
}

valid_premium_interval <- function(
        contract,
        b,
        delta,
        m,
        n,
        h
) {
    a_h <- annuity_cont(h, delta)

    lower <- if (m == 0 && n == 100) {
        b * exp(-delta * n) / a_h
    } else {
        0
    }

    if (contract == "Assurance") {
        upper <- if (m == 0) {
            b
        } else {
            b * exp(-delta * m) / a_h
        }
    } else {
        upper <-
            b *
            exp(-delta * m) *
            annuity_cont(n, delta) /
            a_h
    }

    list(
        lower = lower,
        upper = upper
    )
}

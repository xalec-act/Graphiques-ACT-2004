# Calcule la valeur actuelle d'une rente continue certaine versée au taux de 1.
# t     : durée de versement de la rente.
# delta : force d'intérêt.
# pmax(t, 0) empêche l'utilisation d'une durée négative.
annuity_cont <- function(t, delta) (1 - exp(-delta * pmax(t, 0))) / delta


# Calcule la fonction de survie de la durée de vie future T.
# t     : durée écoulée.
# law   : loi de mortalité, soit "Exponentielle" ou "De Moivre".
# mu    : force de mortalité constante pour la loi exponentielle.
# omega : âge limite selon la loi de De Moivre.
# x     : âge actuel de l'assuré.
survival_T <- function(t, law, mu, omega, x) {
    # Sous la loi exponentielle, S(t) = exp(-mu * t).
    # Sous la loi de De Moivre, S(t) = 1 - t / (omega - x),
    # avec une valeur minimale de 0.
    if (law == "Exponentielle") exp(-mu * pmax(t, 0)) else pmax(1 - pmax(t, 0)/(omega-x), 0)
}


# Calcule la densité de probabilité de la durée de vie future T.
# t     : durée écoulée.
# law   : loi de mortalité utilisée.
# mu    : force de mortalité constante pour la loi exponentielle.
# omega : âge limite selon la loi de De Moivre.
# x     : âge actuel de l'assuré.
density_T <- function(t, law, mu, omega, x) {
    # Sous la loi exponentielle, f(t) = mu * exp(-mu * t).
    # Sous la loi de De Moivre, T est uniforme sur [0, omega - x].
    if (law == "Exponentielle") mu*exp(-mu*pmax(t,0)) else ifelse(t>=0 & t<=omega-x, 1/(omega-x), 0)
}


# Calcule la prime continue selon le principe d'équivalence.
# La valeur actuelle espérée des primes est rendue égale à
# la valeur actuelle espérée des prestations.
#
# contract : type de contrat, soit "Assurance" ou "Rente".
# b        : montant de la prestation ou taux de rente.
# delta    : force d'intérêt.
# m        : durée du différé.
# n        : durée de la période de couverture ou de versement.
# h        : durée de paiement des primes.
# law      : loi de mortalité.
# mu       : force de mortalité pour la loi exponentielle.
# omega    : âge limite pour la loi de De Moivre.
# x        : âge actuel de l'assuré.
premium_equivalence <- function(contract, b, delta, m, n, h, law, mu, omega, x) {

    # Fonction de survie définie avec les paramètres du contrat.
    S <- function(t) survival_T(t, law, mu, omega, x)

    # Détermine l'horizon maximal utilisé dans les intégrales.
    # Pour la loi de De Moivre, la durée de vie future ne peut pas
    # dépasser omega - x.
    max_age <- if (law == "Exponentielle") max(m + n, h) else omega - x

    # Valeur actuelle espérée d'une prime continue de taux 1,
    # payable tant que l'assuré survit, au plus jusqu'au temps h.
    prem_epv <- integrate(function(s) exp(-delta * s) * S(s), 0,
                          min(h, max_age), subdivisions = 1000)$value

    if (contract == "Assurance") {

        # Limite supérieure de la période de couverture.
        upper <- min(m + n, max_age)

        # Valeur actuelle espérée de la prestation d'assurance.
        # La prestation b est versée au décès si celui-ci survient
        # entre les temps m et m + n.
        benefit_epv <- if (upper <= m) 0 else integrate(
            function(t) b * exp(-delta * t) * density_T(t, law, mu, omega, x),
            m, upper, subdivisions = 1000
        )$value

    } else {

        # Limite supérieure de la période de versement de la rente.
        upper <- min(m + n, max_age)

        # Valeur actuelle espérée de la rente continue.
        # Le taux de rente b est versé entre m et m + n,
        # tant que l'assuré est vivant.
        benefit_epv <- if (upper <= m) 0 else integrate(
            function(s) b * exp(-delta * s) * S(s), m, upper,
            subdivisions = 1000
        )$value
    }

    # La prime ne peut pas être calculée si la valeur actuelle
    # espérée d'une prime unitaire est nulle ou négative.
    if (prem_epv <= 0) return(NA_real_)

    # Prime d'équivalence :
    # valeur actuelle espérée des prestations divisée par
    # la valeur actuelle espérée des primes unitaires.
    benefit_epv / prem_epv
}


# Calcule la perte de l'assureur pour une réalisation T
# de la durée de vie future.
#
# T        : durée de vie future observée.
# contract : type de contrat.
# b        : montant de la prestation ou taux de rente.
# pi       : taux continu de prime.
# delta    : force d'intérêt.
# m        : durée du différé.
# n        : durée de la période de couverture ou de versement.
# h        : durée maximale de paiement des primes.
loss_value <- function(T,contract,b,pi,delta,m,n,h) {

    # Valeur actuelle des primes payées jusqu'au décès T
    # ou jusqu'à la fin de la période de paiement h.
    premiums <- pi*annuity_cont(pmin(T,h),delta)

    # Pour une assurance, la prestation est versée au décès
    # si m < T <= m + n.
    #
    # Pour une rente, les prestations sont versées continuellement
    # à partir du temps m jusqu'au décès ou jusqu'au temps m + n.
    benefits <- if(contract=="Assurance") ifelse(T>m & T<=m+n,b*exp(-delta*T),0) else
        b*exp(-delta*m)*annuity_cont(pmax(pmin(T,m+n)-m,0),delta)

    # Perte de l'assureur :
    # valeur actuelle des prestations moins valeur actuelle des primes.
    benefits-premiums
}


# Approxime la distribution de la variable aléatoire de perte.
#
# contract : type de contrat.
# b        : montant de la prestation ou taux de rente.
# pi       : taux continu de prime.
# delta    : force d'intérêt.
# m        : durée du différé.
# n        : durée de la période de couverture ou de versement.
# h        : durée de paiement des primes.
# law      : loi de mortalité.
# mu       : force de mortalité pour la loi exponentielle.
# omega    : âge limite pour la loi de De Moivre.
# x        : âge actuel.
# N        : nombre de points utilisés pour discrétiser l'horizon.
loss_distribution <- function(contract,b,pi,delta,m,n,h,law,mu,omega,x,N=15000) {

    # Détermine l'horizon utilisé pour l'approximation.
    H <- if(law=="Exponentielle") max(m+n,h) else omega-x

    # Ajoute les points où la forme de la fonction de perte
    # peut changer : 0, h, m, m + n et H.
    cuts <- sort(unique(c(0,h,m,m+n,H))); cuts <- cuts[cuts>=0 & cuts<=H]

    # Construit une grille temporelle contenant les points de coupure.
    t <- unique(sort(c(seq(0,H,length.out=N),cuts)))

    # Calcule le milieu de chaque intervalle de la grille.
    mid <- head(t,-1)+diff(t)/2

    # Approxime la probabilité associée à chaque intervalle
    # par la densité au point milieu multipliée par sa largeur.
    w <- density_T(mid,law,mu,omega,x)*diff(t)

    # Calcule la perte associée à chaque point milieu.
    L <- loss_value(mid,contract,b,pi,delta,m,n,h)

    # Calcule la probabilité de survivre au-delà de l'horizon H.
    tail <- survival_T(H,law,mu,omega,x)

    # Si la probabilité résiduelle est non négligeable,
    # elle est ajoutée comme masse de probabilité supplémentaire.
    if(tail>1e-12){L<-c(L,loss_value(H+1,contract,b,pi,delta,m,n,h));w<-c(w,tail)}

    # Trie les pertes en ordre croissant.
    o<-order(L)

    # Retourne les pertes, leurs poids et leur fonction
    # de répartition cumulative normalisée.
    data.frame(loss=L[o],weight=w[o],cdf=cumsum(w[o])/sum(w))
}


# Vérifie que les paramètres du contrat et de la loi de mortalité
# satisfont les contraintes nécessaires aux calculs.
#
# La fonction retourne un vecteur de messages d'erreur.
# Un vecteur vide signifie que tous les paramètres sont valides.
validate_parameters <- function(contract,law,delta,mu,omega,x,m,n,h,b,pi) {

    # Initialise le vecteur de messages d'erreur et détermine
    # l'horizon maximal permis.
    e<-character(); H<-if(law=="Exponentielle") 100 else omega-x

    # Vérification de la force d'intérêt et de la prestation.
    if(delta<=0)e<-c(e,"delta doit être positif."); if(b<=0)e<-c(e,"b doit être positif.")

    # Vérification de la prime.
    if(!is.finite(pi)||pi<=0)e<-c(e,"La prime doit être positive.")

    # Vérification du paramètre de la loi exponentielle.
    if(law=="Exponentielle"&&mu<=0)e<-c(e,"mu doit être positif.")

    # Vérification des âges sous la loi de De Moivre.
    if(law=="De Moivre"&&omega<=x)e<-c(e,"Il faut omega > x.")

    # Vérification du différé.
    if(m<0||m>=H)e<-c(e,"m est hors de l'horizon permis.")

    # Vérification de la durée du contrat.
    if(n<=0||m+n>H)e<-c(e,"n est invalide ou m+n dépasse l'horizon.")

    # Vérification de la durée de paiement des primes.
    if(h<=0||h>m+n)e<-c(e,"h doit être positif et inférieur ou égal à m+n.")

    # Pour une rente, les primes doivent être entièrement payées
    # avant le début des versements de rente.
    if(contract=="Rente"&&h>m)e<-c(e,"Pour une rente, h doit être inférieur ou égal à m.")

    # Retourne l'ensemble des erreurs détectées.
    e
}


# Détermine l'intervalle dans lequel la prime doit se trouver
# pour que la fonction de perte respecte les bornes attendues.
#
# contract : type de contrat, soit "Assurance" ou "Rente".
# b        : montant de la prestation ou taux de rente.
# delta    : force d'intérêt.
# m        : durée du différé.
# n        : durée de la couverture ou du versement.
# h        : durée de paiement des primes.
valid_premium_interval <- function(
        contract,
        b,
        delta,
        m,
        n,
        h
) {
    # Valeur actuelle d'une rente continue certaine de durée h.
    a_h <- annuity_cont(h, delta)

    # La borne inférieure est généralement nulle.
    # Dans le cas particulier m = 0 et n = 100,
    # elle correspond à la prestation actualisée au temps n,
    # répartie sur la valeur actuelle des primes.
    lower <- if (m == 0 && n == 100) {
        b * exp(-delta * n) / a_h
    } else {
        0
    }

    if (contract == "Assurance") {

        # Pour une assurance immédiate, la borne supérieure
        # correspond directement au montant b.
        #
        # Pour une assurance différée, elle correspond à la
        # prestation actualisée au temps m, divisée par la
        # valeur actuelle des primes unitaires.
        upper <- if (m == 0) {
            b
        } else {
            b * exp(-delta * m) / a_h
        }

    } else {

        # Pour une rente, la borne supérieure est obtenue à partir
        # de la valeur actuelle de la rente versée pendant n années,
        # différée de m années, puis divisée par la valeur actuelle
        # des primes unitaires payables pendant h années.
        upper <-
            b *
            exp(-delta * m) *
            annuity_cont(n, delta) /
            a_h
    }

    # Retourne les deux bornes de l'intervalle valide.
    list(
        lower = lower,
        upper = upper
    )
}

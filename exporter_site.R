###
### Fichier pour exporter les sites sur un site.
###

if (!requireNamespace("shinylive", quietly = TRUE)) {
    install.packages("shinylive")
}

if (!requireNamespace("httpuv", quietly = TRUE)) {
    install.packages("httpuv")
}

if (dir.exists("docs")) {
    unlink(
        "docs",
        recursive = TRUE,
        force = TRUE
    )
}

dir.create("docs")

shinylive::export(
    appdir = "applications/application_L",
    destdir = "docs",
    subdir = "application_L"
)

shinylive::export(
    appdir = "applications/application_Z",
    destdir = "docs",
    subdir = "application_Z"
)

shinylive::export(
    appdir = "applications/application_Y",
    destdir = "docs",
    subdir = "application_Y"
)

page_accueil <- '
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">

  <title>Applications Shiny</title>

  <style>
    body {
      max-width: 900px;
      margin: 40px auto;
      padding: 20px;
      font-family: Arial, sans-serif;
      background: #f5f7fa;
      color: #222;
    }

    h1 {
      color: #136f8a;
    }

    .application {
      margin: 20px 0;
      padding: 24px;
      background: white;
      border-radius: 10px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }

    a {
      display: inline-block;
      padding: 10px 18px;
      color: white;
      background: #136f8a;
      border-radius: 5px;
      text-decoration: none;
    }

    a:hover {
      background: #0e5368;
    }
  </style>
</head>

<body>
  <h1>Applications Shiny</h1>

  <div class="application">
    <h2>Fonctions de perte</h2>
    <p>Fonctions L(T) et F<sub>L</sub>(l).</p>
    <a href="./application_L/">Ouvrir</a>
  </div>

  <div class="application">
    <h2>Fonctions pour Z</h2>
    <a href="./application_Z/">Ouvrir</a>
  </div>

  <div class="application">
    <h2>Fonctions pour Y</h2>
    <a href="./application_Y/">Ouvrir</a>
  </div>
</body>
</html>
'

writeLines(
    page_accueil,
    "docs/index.html",
    useBytes = TRUE
)

file.create("docs/.nojekyll")

message("Le dossier docs est prêt.")

# Staviz - Interactive Statistical Learning Platform

## How to install

### 1. Install R language version 4.5.1 on your local machine

https://cran.r-project.org/bin/windows/base/

### 2. Install R & Shiny extensions on your VS code editor

#### R extensions

https://marketplace.visualstudio.com/items?itemName=REditorSupport.r

https://marketplace.visualstudio.com/items?itemName=REditorSupport.r-syntax

#### Shiny extensions

https://marketplace.visualstudio.com/items?itemName=Posit.shiny

### 3. Install Docker & Docker Compose

#### Docker Desktop on Windows

https://docs.docker.com/desktop/setup/install/windows-install/

#### Docker Compose on Windows

https://docs.docker.com/compose/install/

## How to run project

Run command after install docker compose in step 3

```
docker compose up --build -d
```

Then open http://localhost:80

## How to run only module

You can only run the module you want in `modules` folder by click Play button in `app.R` if you already installed Shiny extension in Vscode:

```
.
└── module/
    ├── app.R
    ├── ui.R
    └── server.R
```

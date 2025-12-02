# Use the official R Shiny image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libsqlite3-dev \
  libsodium-dev \
  r-cran-sodium \
  && rm -rf /var/lib/apt/lists/*

# Copy your app to the image
COPY . /srv/shiny-server/

# Set ownership to the shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Install R packages (add your required packages here)
RUN R -e "install.packages(c('shiny', 'shinyjs', 'shinyBS', 'plotly', 'glue', 'sodium', 'shinydashboard', 'DT', 'googlesheets4', 'googledrive', 'RSQLite', 'pool', 'dplyr', 'shinyalert'), repos='https://cloud.r-project.org/')"

# Set the port environment variable
ENV PORT=3839

RUN sed -i -e 's/\blisten 3838\b/listen 3839/g' /etc/shiny-server/shiny-server.conf

# Expose the default Shiny port
EXPOSE 3839

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]

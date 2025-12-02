# Use the official R Shiny image
FROM rocker/shiny:latest

# Install system dependencies (if needed)
# RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev

# Copy your app to the image
COPY . /srv/shiny-server/

# Set ownership to the shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Install R packages (add your required packages here)
RUN R -e "install.packages(c('shiny'), repos='https://cloud.r-project.org/')"

# Expose the default Shiny port
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]

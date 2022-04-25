# SSRP_dashboard
This repo contains the scripts used to produce the visualizations used in the SSRP dashboard.  

<https://bitss.github.io/SSRP_dashboard/>.  

# Setup
The `03-data-filtering.Rmd` script adds tidy dataframes from the [SSRP_cleaning](https://github.com/BITSS/SSRP_cleaning) repository to the `/processed/` directory. To facilitate this, you need to make an `.Renviron` file with the line `clean_path = <LOCAL PATH TO SSRP_cleaning>`.

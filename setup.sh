#!/usr/bin/env bash

# Setup .Renviron file
read -p "SSRP_cleaning path: " SSRP_CLEAN_PATH
echo 'clean_path = "'$SSRP_CLEAN_PATH'"' > .Renviron

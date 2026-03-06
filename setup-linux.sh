#!/usr/bin/env bash
# setup-linux.sh — Install system dependencies for analyzeR on Ubuntu/Debian
# Run this BEFORE running setup.R
#
# Usage:
#   chmod +x setup-linux.sh
#   ./setup-linux.sh

set -e

echo "=== analyzeR: Installing system dependencies ==="

sudo apt-get update -qq

# Core build tools (needed to compile R packages like ranger, arrow)
sudo apt-get install -y \
  build-essential \
  g++ \
  gfortran \
  cmake \
  pkg-config

# Networking (needed by curl, httr, many R packages)
sudo apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev

# XML support (readxl, xml2)
sudo apt-get install -y \
  libxml2-dev

# Font rendering (ggplot2, systemfonts)
sudo apt-get install -y \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev

# Image support (ragg, png, jpeg)
sudo apt-get install -y \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev

# R development headers (required for compiling any R source package)
sudo apt-get install -y \
  r-base-dev

echo ""
echo "=== System dependencies installed ==="
echo "Next step: Rscript setup.R"

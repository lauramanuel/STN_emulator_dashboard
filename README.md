# PTM Emulator Dashboard

## Overview

The **PTM Emulator Dashboard** is an interactive Shiny-based web application designed to visualize and analyze outputs from a Particle Tracking Model (PTM) emulator. The tool integrates time series, validation metrics, routing statistics, and spatial data to support real-time and forecast-based decision-making workflows.

This dashboard is intended to replicate and extend the functionality of Reclamation-style decision-support tools by providing a clean, interactive interface for reviewing model outputs and comparing scenarios.

---

## Key Features

### 📊 Data Visualization
- Time series plots for:
  - Survival
  - Entrainment
- Routing probability distributions
- Emulator vs PTM validation plots
- 7-day entrainment summaries

### 🗺️ Spatial Analysis
- Interactive map using `leaflet`
- Displays:
  - Delta boundary
  - Channel network
  - Node locations
- Node-based entrainment visualized using:
  - Color (viridis scale)
  - Marker size

### 🔄 Interactivity
- Scenario selection
- Node selection
- Date range filtering
- Dynamic updates across all plots and maps

### 🎨 UI/UX Design
- Styled using `shinydashboard`
- Custom theme inspired by Reclamation dashboards
- Consistent color scheme using `viridis`
- Clean layout for stakeholder communication

---

## Project Structure
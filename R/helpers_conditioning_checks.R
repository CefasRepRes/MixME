# ---
# title: 'Functions to check structure of Operating Models'
# author: 'Matthew Pace'
# date: 'June 2022'
# ---
#
# Summary
# =======
#
# This script contains functions to check for correct and consistent structure
# of objects used in multi-stock operating models.
#
# Ideally to be applied in the first step of passing the OM to the simulation
# procedure.
#
# Should check that uncertainty or selectivity values in the projection
# period are available (even if there is no uncertainty - values are all 1s).
#
# This will be a wrapper around the existing check function ... what is it called?

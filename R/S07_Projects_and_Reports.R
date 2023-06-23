# Modeling
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-24

# Table of contents
# 1) create_analysis_project

#### 1) create_analysis_project ####
#' Create Files and Folders for Analysis Project
#'
#' Function to initialize an analysis project folder,
#' creating folders and files typical to most
#' analysis projects.
#'
#' @param initial Logical; if \code{TRUE} creates
#'   folders for source data and R scripts, creates
#'   the \code{'_targets.R'} file, and creates
#'   an example R script for cleaning source data.
#'   Otherwise generates a single template R script.
#' @param work Logical; if \code{TRUE} using current
#'   work email rather than personal email.
#' @param proj A character string, the project label
#'   to start each function with.
#'
#' @returns As a side effect, creates a set of folders
#' and files in the current working directory.
#'
#' @export

create_analysis_project <- function( initial = TRUE,
                                     work = FALSE,
                                     proj = 'XXX_AYY' ) {

  # Determine whether to report work or personal email
  if ( work ) {
    email_address <- "kpotter5@mgh.harvard.edu"
  } else {
    email_address <- "kevin.w.potter@gmail.com"
  }

  # Current date as YYYY-MM-DD
  cur_date <-
    as.character( format( Sys.Date(), "%Y-%m-%d" ) )

  # Current 'arfpam' version
  pkgs <- installed.packages()
  arfpam_ver <- pkgs[ pkgs[,1] == 'arfpam', 3]


  # Create template for header at start of each R script
  proj_header <- c(
    "# Written by Kevin Potter",
    paste0( "# email: ", email_address ),
    "# Please email me directly if you ",
    "# have any questions or comments",
    paste0( "# Last updated ", cur_date ),
    "",
    "# Table of contents"
  )

  # Initialize folders and files
  if ( initial ) {

    message( 'Initializing folders and files for analysis project' )

    ### Template for the '_targets.R' script

    template_for_targets <- c(
      "# Script to generate targets",
      proj_header,
      "# 1) Initial setup",
      "# 2) Generate targets",
      "",
      "### To generate targets ###",
      "# 1. Click on 'Session' and in pull-down menu for ",
      "#    'Set Working Directory' select 'To Project Directory'",
      "# 2. <Optional>",
      "#    - Clear workspace (click broom icon)",
      "#    - Restart R (Click on 'Session' and select 'Restart R')",
      "# 3. Type in console: targets::tar_make()",
      "",
      "#### 0) Template for function documentation ####",
      "# Title ",
      "# ",
      "# ... ",
      "# ",
      "# @param 'obj_x' An R object (see target output ",
      paste0(
        "#   from the '", proj, ".RXX.example' function)."
      ),
      "# ",
      "# @details ",
      "# Prerequisites:",
      "#   * The R package '?' (version ?)",
      paste0( "#   * The '", proj, ".RXX.example' function" ),
      "# ",
      "# @return ...",
      "",
      "#### 1) Initial setup ####",
      "",
      "# Load in package to manage generation of target outputs",
      "# install.packages( 'targets' )",
      "library(targets)",
      "",
      "library(dotenv)",
      "",
      "# Source in all R scripts with functions",
      "arfpam::source_R_scripts( path = 'R' )",
      "",
      "# Load in packages",
      "tar_option_set(",
      "  packages = c(",
      "    # Data frame manipulation and '%>%' operator",
      "    #   To install:",
      "    #   install.packages( 'dplyr' )",
      "    'dplyr',",
      "    # Assorted R functions for processing, analyses, and models",
      "    #   To install:",
      "    #   devtools::install_github( 'rettopwnivek/arfpam' )",
      "    'arfpam'",
      "  )",
      ")",
      "",
      "#### 2) Generate targets ####",
      "",
      "list(",
      "  tar_target(",
      "    chr_path_to_source_file,",
      paste0( "    ", proj, ".R01.path_to_source_file()," ),
      "    format = 'file'",
      "  )",
      ")",
      ""
    )

    # Create R script
    write(
      template_for_targets,
      file = '_targets.R',
      sep = "\n"
    )

    ### Template for .env file

    template_for_dotenv <- c(
      "# User-specific environment variables",
      paste0( "# User: Kevin Potter (", cur_date, ")" ),
      "",
      "# Local paths",
      "LOCAL_PROJECT=D:/CAM_postdoc/CAM_R_projects/CAM_analyses/<...>",
      "",
      "# DropBox folder paths",
      paste0(
        "DROPBOX_ANALYSES=C:/Users/tempp/Dropbox (Partners HealthCare)/",
        "CAM Data and Analyses/PCORI/Analyses/<...>"
      ),
      ""
    )

    # Create .env file
    write(
      template_for_dotenv,
      file = '.env',
      sep = "\n"
    )

    ### Create R folder

    dir.create( "R" )

    ### Template for script to clean data

    template_for_S01 <- c(
      "# Functions to clean data",
      proj_header,
      paste0( "# 1) ", proj, ".data.path_to_source_file" ),
      "",
      "#### 0) Code for debugging ####",
      "if ( FALSE ) {",
      "  ",
      "  # Load in packages",
      "  library(dplyr)",
      "  library(arfpam)",
      "  ",
      "  # Test current function(s)",
      "  source('R/R01-functions_to_clean_data.R')",
      "  ",
      "  ### 1) ",
      "  ",
      paste0(
        "  path_to_source_file <- ",
        proj, ".R01.path_to_source_file()"
      ),
      "",
      "}",
      "",
      paste0(
        "#### 1) ", proj, ".R01.path_to_source_file ####"
      ),
      "# Return Absolute Paths to Files in Source Folder",
      "# ",
      "# A convenience function that returns the ",
      "# absolute path to files in the 'Source' folder.",
      "# ",
      "# @param 'chr_partial_label' A character string, part of ",
      "#   the file name to locate in the 'Source' folder.",
      "# ",
      "# @details ",
      "# Prerequisites:",
      paste0(
        "#   * The package 'arfpam' (version ", arfpam_ver, ")"
      ),
      "# ",
      "# @return A character string, an absolute path to a ",
      "# given file.",
      "",
      paste0(
        proj, ".R01.path_to_source_file <- function( "
      ),
      "  chr_partial_label ) {",
      "  ",
      "  proj <- getwd()",
      "  chr_out <- find_file_name( ",
      "    chr_partial_label,",
      "    path = 'Source', output = 'name'",
      "  )",
      "  ",
      "  chr_out <- proj %p% '/Source/' %p% chr_out",
      "  ",
      "  return( chr_out )",
      "}",
      ""
    )

    write(
      template_for_S01,
      file = 'R/R01-functions_to_clean_data.R',
      sep = "\n"
    )

    ### Output and source folders

    dir.create( "Output" )
    dir.create( "Source" )

    message( 'Done!' )

    # Close 'Initialize folders and files'
  } else {

    message( 'Create template for analysis R script' )

    template_for_script <- c(
      "# Title",
      proj_header,
      paste0( "# 1) ", proj, ".function" ),
      "",
      "#### 0) Code for debugging ####",
      "if ( FALSE ) {",
      "  ",
      "  # Load in packages",
      "  library(dplyr)",
      "  library(arfpam)",
      "  ",
      "  # Test current function(s)",
      "  source('R/RXX-functions_to_do_something.R')",
      "  ",
      "  ### 1) ",
      "  ",
      paste0(
        "  obj_output <- ",
        proj, ".RXX.function()"
      ),
      "}",
      "",
      paste0(
        "#### 1) ", proj, ".RXX.function ####"
      ),
      "# Title ",
      "# ",
      "# ... ",
      "# ",
      "# @param 'obj_x' ... ",
      "# ",
      "# @details ",
      "# Prerequisites:",
      "#   * The package '?' (version ?)",
      "# ",
      "# @return ...",
      "",
      paste0(
        proj, ".RXX.function <- function( "
      ),
      "  obj_x ) {",
      "  ",
      "  # Do something",
      "  ",
      "}",
      ""
    )

    write(
      template_for_script,
      file = 'R/SXX-functions_to_do_something.R',
      sep = "\n"
    )

    message( 'Done!' )

    # Close else for 'Initialize folders and files'
  }

}



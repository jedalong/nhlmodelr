#' NHL shot location data for the 2011-2012 season
#'
#' Shot location records for the 2011-2012 season.
#' 
#' The NHL shot location data are stored as a dataframe with the following columns:
#' - PLAYID - Shot outcome (505 - Goal, 506 - Save, 507 - Block, 508 - Miss)
#' - PLR1.ID - Player ID from NHL.com.
#' - PlyrName - Player Name
#' - PlyrPos - Player Position (from NHL.com)
#' - TeamID - Team ID (see teamInfo)
#' - TeamStrength - Team Status (701 - Even Strength, 702 - PP, 703 - PK)
#' - ShotType - Type of shot ()
#' - ShotDistFt - ESPN shot distance measurement (in feet)
#' - GOALIE.ID - Goaltender ID (Not included for blocks or misses)
#' - GoalieName - Goaltender Name
#' - gTeamID - Goaltender Team ID (see \code{teamInfo})
#' - Goalie - Goaltender status (901 - Goaltender in, 902 - Goaltender out, 903 - Pen. Shot)
#' - x - Shot x location (x is the length of the rink)
#' - y - Shot y location (y is the width of the rink)
#' - Season - Year of the season
#' - PreRegPost - Game class (1 - pre season, 2 - reg. season, 3 - playoffs)
#' - GameID - ESPN.com Game ID
#'
#' @docType data
#' @keywords datasets
#' @format An \code{data.frame} object with 17 columns where each record is an NHL shot data point.
#' @name shots20112012
#-----------------------------------
NULL

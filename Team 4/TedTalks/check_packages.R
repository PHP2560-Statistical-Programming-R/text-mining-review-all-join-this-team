# taken from SullivanStatistics/Dunkin_Scrape/check_packages.R
#checks for r packages
#loads r packages
#installs packages if necessary

check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages())) {
      install.packages(name, repos="http://cran.us.r-project.org")
    }
    library(name, character.only=TRUE)
  }
}
#### Sourcing the UI elements

source("ui/uiHeader.R", local = TRUE)
source("ui/uiSideBar.R", local = TRUE)
source("ui/uiBody.R", local = TRUE)

dashboardPage(

#### DashBoard Header ####

    uiHeader,

#### DashBoard side bar ####

    uiSideBar,

#### DashBoard Body ####

    uiBody

)

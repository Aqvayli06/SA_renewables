# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("USERS MANAGEMENT", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: left;",
                     actionButton("adduser", "ADD NEW USER", style = "color: white; background-color:darkgreen;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     # actionButton("deleteuser", "DELETE USER", style = "color: white; background-color:goldenrod;
                     #              padding: 10px 15px; width: 150px; cursor: pointer;
                     #              font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Username Already Exist!!!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     shinyjs::hidden(
                       div(id = "yerna",
                           tags$p("User Added Successfully!",
                                  style = "color: green; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                   ))
)
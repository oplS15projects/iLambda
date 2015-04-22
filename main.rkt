#lang racket

(require web-server/servlet
         web-server/servlet-env)

(require net/url)

(define (app req)
  ;; LOGIN PAGE
  (send/suspend
   (lambda (k-url)
     (response/xexpr
      `(html (head
              ;; Title of the page
              (title "iLambda - Sign In")
              ;; Favicon
              (link ([rel "shortcut icon"] [href "/favicon.ico"]))
              ;; Link to boostrap styles
              (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
              ;; Link to our style sheet
              (link ([rel "stylesheet"] [href "/style.css"])))
             (body (div ([class "container"])
                        (div ([class "main"])
                             ;; Logo
                             (img ([src "/iLambda-logo.png"] [class "logo"]))
                             (p ([class "lead"])
                                "iLambda is designed to aid teachers in setting up projects on"
                                ;; Link to iSENSE homepage
                                (a ([href "http://isenseproject.org"]) " iSENSE")
                                mdash"a system for collecting, visualizing, and sharing data. To begin using iLambda, "
                                ;; Instructions bolded
                                (strong "please sign in with your iSENSE account."))
                             ;; Login form
                             (form ([action, k-url] [method "post"] [class "form-horizontal"])
                                   ;; Email
                                   (div ([class "form-group"])
                                        (label ([for "email"] [class "col-xs-2 col-sm-2 control-label"]) "Email")
                                        (div ([class "col-xs-10 col-sm-10"])
                                             (input ([type "email"] [class "form-control"] [name "email"]))))
                                   ;; Password
                                   (div ([class "form-group"])
                                        (label ([for "password"] [class "col-xs-2 col-sm-2 control-label"]) "Password")
                                        (div ([class "col-xs-10 col-sm-10"])
                                             (input ([type "password"] [class "form-control"]))))
                                   ;; Login button
                                   (button ([type "submit"] [class "btn btn-primary"]) "Login"))
                             (p "Don't have an iSENSE account?"
                                (a ([href "http://isenseproject.org/users/new"]) " Click here to register")
                                ".")
                             )))))))
  
  ;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ;; ISENSE LOGIN IMPLEMENTATION  
  
  (define test-url(string->url "http://isenseproject.org/api/v1/users/myInfo?email=your@email.com&password=yourpassword"))
  
  (display-pure-port(get-pure-port test-url))
  
  ;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ;; PROJECT SELECTION PAGE
  (send/suspend
   (lambda (k-url)
     (response/xexpr
      `(html (head (title "Project Selection Page")
                   (link ([rel "shortcut icon"][href "/favicon.ico"]))
                   (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
                   (link ([rel "stylesheet"] [href "/style.css"])))
             (body (h1 "Project Selection Page")
                   (p "Choose from the list of pre-existing projects:")
                   (form ([action, k-url] [class "form-horizontal"])
                         ;; Projects will go here
                         
                         ;; Project - Gummy Bear Lab
                         (strong "Gummy Bear Lab:  ")(button ([type "submit"]) "Create Project")
                         (p "Determine how a gummy bear changes when it is soaked in water overnight.")
                         (img ([src "http://www.acclaimmag.com/wp-content/uploads/2014/01/2074903820_1375612450.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Measuring Heart Rate
                         (strong "  Measuring Heart Rate:  ")(button ([type "submit"]) "Create Project")
                         (p "  Learn one method of testing aerobic activity for heart recovery.")
                         (img ([src "http://www.runnersgoal.com/wp-content/uploads/2013/07/heartratezone.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Heart Absorption
                         (strong "  Heat Absorption:  ")(button ([type "submit"]) "Create Project")
                         (p "  Determine why you stay cooler in lighter clothes than darker clothes in the hot sun.")
                         (img ([src "http://www.bakesforbreastcancer.org/wp-content/uploads/2012/03/sun.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Holding your breath
                         (strong "Holding Your Breath:  ")(button ([type "submit"]) "Create Project")
                         (p "Explore how your body handles holding your breath.")
                         (img ([src "http://www.itsallyogababy.com/wp-content/uploads/2013/01/breathing.jpeg"][class "projectIMG"]))
                         (p "")
                       
                         ;; Project - Freefall
                         (strong "Freefall:  ")(button ([type "submit"]) "Create Project")
                         (p "Determine the relationship between distance and time for free-falling objects.")
                         (img ([src "http://ecx.images-amazon.com/images/I/31E3Z3sDcrL._SY300_.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Bone Length and Height
                         (strong "Correlation of Bone Length and Height Activity:  ")(button ([type "submit"]) "Create Project")
                         (p "Determine is there is a relationship between a person's bone size and overall height.")
                         (img ([src "http://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Human_arm_bones_diagram.svg/1167px-Human_arm_bones_diagram.svg.png"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Objects Density
                         (strong "Density of Objects:")(button ([type "submit"]) "Create Project")
                         (p "Compare the density of various materials.")
                         (img ([src "http://isenseproject.org/media/99/99efbe54c90a72a1115faade22a5575f/plastic-counters.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Dice Rolls
                         (strong "Distribution of Dice Rolls:  ")(button ([type "submit"]) "Create Project")
                         (p "Understand the concepts of theoretical and experimental probability.")
                         (img ([src "http://isenseproject.org/media/35/35270c9752997a472374aec0ce72b6e6/cover%20picture.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; Project - Hooke's Law
                         (strong "Hooke's Law and Spring Constants:  ")(button ([type "submit"]) "Create Project")
                         (p "Understand the concepts of Hooke's Law and spring constants.")
                         (img ([src "http://isenseproject.org/media/37/3724d70818ac49716c81aa845919b764/cover%20picture.jpg"][class "projectIMG"]))
                         (p "")
                          
                         ;; Project - Replacement Probability
                         (strong "Single Draw with Replacement Probability with Two Distributions:  ")(button ([type "submit"]) "Create Project")
                         (p "Determine the quantity of each color of the object in a container.")
                         (img ([src "http://isenseproject.org/media/9f/9f4e88e4fb694cc4ea141fbadbaab685/skittles.jpg"][class "projectIMG"]))
                         (p "")
                         
                         ;; -------------------
                         ;; Prev and next buttons
                         (button ([type "submit"] [class "btn btn-danger prev"]) "Prev")
                         (button ([type "submit"] [class "btn btn-success"]) "Save & Continue")))))))
  ;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ;; PROJECT TITLE PAGE
  (send/suspend
   (lambda (k-url)
     (response/xexpr
      `(html (head
              ;; Title of the page
              (title "iLambda - Project Title")
              ;; Favicon
              (link ([rel "shortcut icon"] [href "/favicon.ico"]))
              ;; Link to boostrap styles
              (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
              ;; Link to our style sheet
              (link ([rel "stylesheet"] [href "/style.css"])))
             (body (div ([class "container"])
                        (div ([class "main"])
                             ;;  Bread crumbs
                             (p ([class "breadcrumbs"]) "Select Project > " (span ([class "current-tab"]) "Project Title") " > Add Media Objects > Add Contibutor Keys > Finish")
                             (h1 "Your Project's Title")
                             ;; Project title form
                             (form ([class "form-horizontal"])
                                   ;; Project title
                                   (div ([class "form-group"])
                                        (label ([for "title"]))
                                        (div (input ([type "text"] [class "form-control"] [value "The Bone Lab"]))))
                                   ;; Prev and next buttons
                                   (button ([type "submit"] [class "btn btn-danger prev"]) "Prev")
                                   (button ([type "submit"] [class "btn btn-success"]) "Save & Continue"))))))))))
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Start web application
;; Includes path to CSS file
(serve/servlet app
               #:extra-files-paths
               (list
                ;; IMPORTANT: Ravy, you will need to change this path to work w/ your computer
                ;; (build-path "/Users/kaitlyncarcia/Repos/iLambda") "htdocs"))
                (build-path "/home/ravythok/opl/iLambda") "style"))
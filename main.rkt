#lang racket

;; Kaitlyn Carcia (kcarcia) and Ravy Thok (rthok)
;; Organization of Programming Languages w/ Mark Sherman

;; We used a racket blog program as a guide to doing form submission.
;; Source: http://docs.racket-lang.org/continue/

;; ------------------------------------------------------------------------------------------------------------------------------------------------
;; REQUIRES

;; web server library
(require web-server/servlet
         web-server/servlet-env)

;; isense-racket api
(require "isense-racket-api/isense-3.rkt")

;; net library for get and post requests
(require net/url)

;; TO-DO: tried moving database stuff into another file, but then code wasn't working.
;; sigh... really would be nice to have that code into another file.
;; (require "db.rkt")

;; ------------------------------------------------------------------------------------------------------------------------------------------------
;; DATABASE PROCEDURES

;; define empty database
(define projectDB '())

;; makes a project entry
(define (make-project id instructions-url picture-url title fields)
  (list id instructions-url picture-url title fields))

;; inserts project into database
(define (insert-project project)
  (set! projectDB (append projectDB (list project))))

;; ********* project getters *********
;; returns id (number) of project
(define (id project)
  (car project))

;; Returns instructions url (string) of project
(define (instructions-url project)
  (cadr project))

;; Returns picture-url (string) of project
(define (picture-url project)
  (caddr project))

;; returns title (string) of project
(define (title project)
  (cadddr project))

;; returns list of fields of project
(define (fields project)
  (car (cddddr project)))

;; ********* fields getters *********
;; returns type (number) of field (1 = timestamp, 2 = number, 3 = text)
(define (type field)
  (car field))

;; returns name (string) of field
(define (name field)
  (cadr field))

;; returns units (string) of field
(define (units field)
  (caddr field))

;; returns restrictions (string) of field
(define (restrictions field)
  (cadddr field))

;; ********* misc. *********
;; returns project (as a list) with given id
(define (find-project id)
  (car (filter (lambda (x) (= (car x) id)) projectDB)))

;; makes a fields object with all a project's fields
(define (make-fields fields)
  (if (null? fields)
      '()
      (cons (isense-project-field (name (car fields)) (type (car fields)) (units (car fields)) (restrictions (car fields))) (make-fields (cdr fields)))))

;; ------------------------------------------------------------------------------------------------------------------------------------------------
;; DATABASE ENTRIES

;; Gummy Bear
(insert-project (make-project 1
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/gummy-bear.pdf"
                              "http://www.acclaimmag.com/wp-content/uploads/2014/01/2074903820_1375612450.jpg"
                              "Gummy Bear Lab"
                              (list (list 2 "Length" "m" "")
                                    (list 2 "Width" "m" "")
                                    (list 2 "Height" "m" "")
                                    (list 2 "Volume" "m^3" "")
                                    (list 2 "Mass" "oz" "")
                                    (list 3 "Day" "" "Day 1, Day 2")
                                    (list 3 "Color" "" "Red,Yellow,Orange,Green,Colorless,Blue,Purple"))))
;; Measuring Heart Rate
(insert-project (make-project 2
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/measuring-heart-rate.pdf"
                              "http://www.runnersgoal.com/wp-content/uploads/2013/07/heartratezone.jpg"
                              "Measuring Heart Rate"
                              (list (list 2 "Heart Rate" "" "")
                                    (list 2 "Time Elapsed" "seconds" ""))))
;; Heat Absorption
(insert-project (make-project 3
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/heat-absorption.pdf"
                              "http://www.bakesforbreastcancer.org/wp-content/uploads/2012/03/sun.jpg"
                              "Heat Absorption"
                              (list (list 1 "Timestamp" "" "")
                                    (list 3 "Glass" "" "Light,Dark")
                                    (list 2 "Temperature" "C" ""))))
;; Holding Your Breath
(insert-project (make-project 4
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/holding-your-breath.pdf"
                              "http://www.itsallyogababy.com/wp-content/uploads/2013/01/breathing.jpeg"
                              "Holding Your Breath"
                              (list (list 2 "Breathing Normally" "seconds" "")
                                    (list 2 "Breathing in Bag" "seconds" ""))))
;; Freefall
(insert-project (make-project 5
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/freefall.pdf"
                              "http://ecx.images-amazon.com/images/I/31E3Z3sDcrL._SY300_.jpg"
                              "Freefall"
                              (list (list 2 "Time" "seconds" "")
                                    (list 2 "Distance" "meters" ""))))
;; Correlation of Bone Length and Height Activity
(insert-project (make-project 6
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/Correlation-of%20Bone-Length-and%20Height-Activity.pdf"
                              "http://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Human_arm_bones_diagram.svg/1167px-Human_arm_bones_diagram.svg.png"
                              "Correlation of Bone Length and Height Activity"
                              (list (list 2 "Tibia" "cm" "")
                                    (list 2 "Fibula" "cm" "")
                                    (list 2 "Femur" "cm" "")
                                    (list 2 "Total Height" "cm" ""))))
;; Density of Objects
(insert-project (make-project 7
                              "http://isenseproject.org/media/06/06db85208e88aa120233d756b3550d5b/DensityofObjects.pdf"
                              "http://isenseproject.org/media/99/99efbe54c90a72a1115faade22a5575f/plastic-counters.jpg"
                              "Density of Objects"
                              (list (list 2 "Mass" "g" "")
                                    (list 2 "Volume" "mL" "")
                                    (list 2 "Density" "g/mL" "")
                                    (list 3 "Object Type" "" "Glass Marble,Rock,Plastic toy,Zinc washer"))))
;; Distribution of Dice Rolls
(insert-project (make-project 8
                              "http://isenseproject.org/media/f2/f208d643de0018c596313ad191165e21/Distribution%20of%20Dice%20Rolls.pdf"
                              "http://isenseproject.org/media/35/35270c9752997a472374aec0ce72b6e6/cover%20picture.jpg"
                              "Distribution of Dice Rolls"
                              (list (list 2 "Trial" ""  "")
                                    (list 2 "White Dice" "" "")
                                    (list 2 "Yellow Dice" "" "")
                                    (list 2 "Sum" "" ""))))
;; Hooke's Law and Spring Constants
(insert-project (make-project 9
                              "http://isenseproject.org/media/48/480dd73dac5829aad56f98256449bd8f/Hooke's%20Law%20and%20Spring%20Constants.pdf"
                              "http://isenseproject.org/media/37/3724d70818ac49716c81aa845919b764/cover%20picture.jpg"
                              "Hooke’s Law and Spring Constants"
                              (list (list 2 "Mass" "g" "")
                                    (list 2 "Displacement" "cm" "")
                                    (list 3 "Spring Label (a, b, c)"  "" "a,b,c"))))
;; Single Draw with Replacement Probability with Two Distributions
(insert-project (make-project 10
                              "http://isenseproject.org/media/3a/3a7e0f0d3f93543bb96d5200df7fdf93/Single_Draw_with_Replacement-Two_Distributions.pdf"
                              "http://isenseproject.org/media/9f/9f4e88e4fb694cc4ea141fbadbaab685/skittles.jpg"
                              "Single Draw with Replacement Probability with Two Distributions"
                              (list (list 2 "Red" "" "")
                                    (list 2 "Blue" "" "")
                                    (list 2 "Yellow" "" "")
                                    (list 3 "Bag Color" "" "Green,Pink"))))

;; ------------------------------------------------------------------------------------------------------------------------------------------------
;; BINDINGS/PARSING PROCEDURES FOR FORM SUBMISSION

;; login is a (login email password)
;; email is a string, and password is a string
(struct login (email password))

;; parse-login: bindings -> login
;; extracts a login out of the bindings (including email and password)
(define (parse-login bindings)
  (login (extract-binding/single 'email bindings)
         (extract-binding/single 'password bindings)))

;; parse-project: bindings -> string
;; extracts an id out of the bindings
(define (parse-project bindings)
  (extract-binding/single 'id bindings))

;; parse-title: bindings -> string
;; extracts a title out of the bindings
(define (parse-title bindings)
  (extract-binding/single 'title bindings))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; LOGIN PAGE

;; render-sign-in-page: request -> doesn't return
;; renders login page
(define (render-sign-in-page request)
  (local [(define (sign-in embed/url)
            (response/xexpr
             `(html
               ;; title of the page
               (title "iLambda - Sign In")
               ;; favicon;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
               (link ([rel "shortcut icon"] [href "/favicon.ico"]))
               ;; link to boostrap styles
               (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
               ;; link to our style sheet
               (link ([rel "stylesheet"] [href "/style.css"]))
               (body (div ([class "container"])
                          (div ([class "main"])
                               ;; logo
                               (img ([src "/iLambda-logo.png"] [class "logo"]))
                               (p ([class "lead"])
                                  "iLambda is designed to aid teachers in setting up projects on"
                                  ;; link to iSENSE homepage
                                  (a ([href "http://isenseproject.org"]) " iSENSE")
                                  mdash"a system for collecting, visualizing, and sharing data. To begin using iLambda, "
                                  ;; instructions bolded
                                  (strong "please sign in with your iSENSE account."))
                               ;; sign in form
                               (form ([action,(embed/url login-handler)]  [class "form-horizontal"])
                                     ;; email
                                     (div ([class "form-group"])
                                          (label ([for "email"] [class "col-xs-2 col-sm-2 control-label"]) "Email")
                                          (div ([class "col-xs-10 col-sm-10"])
                                               (input ([type "email"] [class "form-control"] [name "email"]))))
                                     ;; password
                                     (div ([class "form-group"])
                                          (label ([for "password"] [class "col-xs-2 col-sm-2 control-label"]) "Password")
                                          (div ([class "col-xs-10 col-sm-10s"])
                                               (input ([type "password"] [class "form-control"] [name "password"]))))
                                     ;; login button
                                     (button ([type "submit"] [class "btn btn-primary"]) "Login"))
                               (p "Don't have an iSENSE account?"
                                  (a ([href "http://isenseproject.org/users/new"]) " Click here to register")".")))))))
          ;; handles sign in form submission
          (define (login-handler request)
            ;; gets credentials
            (define cred  (parse-login (request-bindings request)))
            
            ;; creates url with credentials as params
            (define url (format "http://isenseproject.org/api/v1/users/myInfo?email=~s&password=~s" (string->symbol (login-email cred)) (string->symbol (login-password cred))))
            
            ;; converts url to string
            (define login-url(string->url url))
            
            ;; makes GET request to iSENSE API, and display results (for debugging)
            (display-pure-port (get-pure-port login-url))
            
            ;; TO-DO: validate user name and password are correct
            (if (eq? #t #t)
                ;; render project selection page if successful
                (render-select-project-page  (login-email cred) (login-password cred))
                ;; TO DO: user fails, display message?
                (display "you failed.")))]
    (send/suspend/dispatch sign-in)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; PROJECT SELECTION PAGE

;; TO-DO: could be nice to put this all in a for-each loop and use database functions
;; to insert the content into the html template... we'll see if we have time

;; render-select-project-page request -> doesn't return
;; renders project selection page
(define (render-select-project-page email pass)
  (local [(define (select-project embed/url)
            (response/xexpr
             `(html
               ;; title of the page
               (title "iLambda - Sign In")
               ;; favicon
               (link ([rel "shortcut icon"] [href "/favicon.ico"]))
               ;; link to boostrap styles
               (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
               ;; link to our style sheet
               (link ([rel "stylesheet"] [href "/style.css"]))
               (body (h1 "Project Selection")
                     (p ([class "lead"])"Choose from the list of pre-existing projects:")
                     ;; project - Gummy Bear Lab
                     (form ([action,(embed/url create-project)] [class "form-horizontal"])                         
                           (strong "Gummy Bear Lab:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "1"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Determine how a gummy bear changes when it is soaked in water overnight.")
                           (img ([src "http://www.acclaimmag.com/wp-content/uploads/2014/01/2074903820_1375612450.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Measuring Heart Rate
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "  Measuring Heart Rate:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "2"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "  Learn one method of testing aerobic activity for heart recovery.")
                           (img ([src "http://www.runnersgoal.com/wp-content/uploads/2013/07/heartratezone.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Heart Absorption
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "  Heat Absorption:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "3"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "  Determine why you stay cooler in lighter clothes than darker clothes in the hot sun.")
                           (img ([src "http://www.bakesforbreastcancer.org/wp-content/uploads/2012/03/sun.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Holding your breath
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Holding Your Breath:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "4"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Explore how your body handles holding your breath.")
                           (img ([src "http://www.itsallyogababy.com/wp-content/uploads/2013/01/breathing.jpeg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Freefall
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Freefall:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "5"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Determine the relationship between distance and time for free-falling objects.")
                           (img ([src "http://ecx.images-amazon.com/images/I/31E3Z3sDcrL._SY300_.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Bone Length and Height
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Correlation of Bone Length and Height Activity:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "6"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Determine is there is a relationship between a person's bone size and overall height.")
                           (img ([src "http://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Human_arm_bones_diagram.svg/1167px-Human_arm_bones_diagram.svg.png"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Objects Density
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Density of Objects:")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "7"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Compare the density of various materials.")
                           (img ([src "http://isenseproject.org/media/99/99efbe54c90a72a1115faade22a5575f/plastic-counters.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Dice Rolls
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Distribution of Dice Rolls:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "8"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Understand the concepts of theoretical and experimental probability.")
                           (img ([src "http://isenseproject.org/media/35/35270c9752997a472374aec0ce72b6e6/cover%20picture.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Hooke's Law
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Hooke's Law and Spring Constants:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "9"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Understand the concepts of Hooke's Law and spring constants.")
                           (img ([src "http://isenseproject.org/media/37/3724d70818ac49716c81aa845919b764/cover%20picture.jpg"] [class "projectIMG"]))
                           (p ""))
                     ;; project - Replacement Probability
                     (form ([action,(embed/url create-project)] [class "form-horizontal"]) 
                           (strong "Single Draw with Replacement Probability with Two Distributions:  ")
                           (input ([type "number"] [class "form-control"] [name "id"] [value "10"] [style "visibility: hidden;"]))
                           (button ([type "submit"]) "Create Project")
                           (p "Determine the quantity of each color of the object in a container.")
                           (img ([src "http://isenseproject.org/media/9f/9f4e88e4fb694cc4ea141fbadbaab685/skittles.jpg"] [class "projectIMG"]))
                           (p ""))))))
          (define (create-project request)
            ;; pass email, password, and id of selected project to project title page
            (render-project-title-page email pass (parse-project (request-bindings request))))]
    (send/suspend/dispatch select-project)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; PROJECT TITLE PAGE

;; render-project-title-page request -> doesn't return
;; renders project title page
(define (render-project-title-page email pass id)
  (local [(define (project-title embed/url)
            (response/xexpr
             `(html (head
                     ;; title of the page
                     (title "iLambda - Project Title")
                     ;; favicon
                     (link ([rel "shortcut icon"] [href "/favicon.ico"]))
                     ;; link to boostrap styles
                     (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
                     ;; link to our style sheet
                     (link ([rel "stylesheet"] [href "/style.css"])))
                    (body (div ([class "container"])
                               (div ([class "main"])
                                    ;;  bread crumbs
                                    (p ([class "breadcrumbs"]) "Select Project > " (span ([class "current-tab"]) "Project Title") " > Add Media Objects > Add Contibutor Keys > Finish")
                                    (h1 "Your Project's Title")
                                    ;; project title form
                                    (form ([class "form-horizontal"][action,(embed/url make-online-project)])
                                          ;; project title
                                          (div ([class "form-group"])
                                               (label ([for "title"]))
                                               (div (input ([type "text"] [class "form-control"] [name "title"] [value ,(title (find-project (string->number id)))]))))
                                          ;; next button
                                          (button ([type "submit"] [class "btn btn-success"]) "Save & Continue"))))))))
          (define (make-online-project request)
            ;; create the project with the given name using isense-racket API
            ;; credentials, title of project, fields
            (isense-create-project (isense-credentials-pass email pass)
                                   (parse-title (request-bindings request))
                                   (make-fields (fields (find-project (string->number id)))))
            (render-media-objects-page))]
    (send/suspend/dispatch project-title)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; MEDIA OBJECTS PAGE

;; render-media-objects-page request -> doesn't return
;; renders media objects page
(define (render-media-objects-page)
  (local [(define (media-objects embed/url)
            (response/xexpr
             `(html (head
                     ;; title of the page
                     (title "iLambda - Project Title")
                     ;; favicon
                     (link ([rel "shortcut icon"] [href "/favicon.ico"]))
                     ;; link to boostrap styles
                     (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
                     ;; link to boostrap file input
                     ;;(link ([rel "stylesheet"][href "//http://gregpike.net/demos/bootstrap-file-input/bootstrap.file-input.js"]))
                     ;; link to our style sheet
                     (link ([rel "stylesheet"] [href "/style.css"])))
                    (body (div ([class "container"])
                               (div ([class "main"])
                                    ;;  bread crumbs
                                    (p ([class "breadcrumbs"]) "Select Project > Project Title > "(span ([class "current-tab"]) "Add Media Objects")" > Add Contibutor Keys > Finish")
                                   
                                    ;; ---------------PAGE CONTENT GOES HERE---------------
                                    ;; project title form
                                    (form ([class "form-horizontal"][action,(embed/url add-media-objects)])
                                          (h1 "Add media objects to your project")
                                          (input ([type "file"][class "filestyle"]))
                                          ;; ---------------FORM CONTENT GOES HERE---------------
                                          ;; next button
                                          (p "")
                                          (button ([type "submit"] [class "btn btn-primary"])"Skip & Continue")
                                          (button ([type "submit"] [class "btn btn-success"]) "Upload & Continue"))))))))
          (define (add-media-objects request)
            ;; ---------------CODE TO ADD MEDIA OBJECTS GOES HERE---------------
            
            
            (render-finish-page))]
    (send/suspend/dispatch media-objects)))


;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; FINISH PAGE

;; render-finish-page request -> doesn't return
;; renders finish page
(define (render-finish-page)
  (local [(define (finish-page embed/url)
            (response/xexpr
             `(html (head
                     ;; title of the page
                     (title "iLambda - You're Finished!")
                     ;; favicon
                     (link ([rel "shortcut icon"] [href "/favicon.ico"]))
                     ;; link to boostrap styles
                     (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
                     ;; link to our style sheet
                     (link ([rel "stylesheet"] [href "/style.css"])))
                    (body (div ([class "container"])
                               (div ([class "main"])
                                    ;;  bread crumbs
                                    (p ([class "breadcrumbs"]) "Select Project > Project Title > Add Media Objects > Add Contibutor Keys > " (span ([class "current-tab"]) "Finish"))
                                    (h1 "Congrats, you're finished!")
                                    (p ([class "lead"]) "Here's the link to your project on iSENSE:")
                                    ;; project link
                                    ;; TO-DO: this needs so UI love
                                    (label ([for "project-link"]))
                                    (div (input ([type "text"] [class "form-control"] [value "http://isenseproject.org/projects/614"])))))))))]
    (send/suspend/dispatch finish-page)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; START SERVER

;; produces a page that displays all of the web content
(serve/servlet render-sign-in-page
               #:extra-files-paths
               (list
               ;; (build-path "/Users/kaitlyncarcia/Repos/iLambda") "htdocs"))
(build-path "/home/ravythok/opl/iLambda") "style"))
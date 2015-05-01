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

;; net library for get and post requests
(require net/url)

;; isense-racket api
(require "isense-racket-api/isense-3.rkt")

;; all database functionality
(require "db.rkt")

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
               ;; favicon
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
                               (form ([action ,(embed/url login-handler)]  [class "form-horizontal"])
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
            ;; gets credentials from form
            (define cred  (parse-login (request-bindings request)))
            
            ;; creates url with credentials as params
            (define url (format "http://isenseproject.org/api/v1/users/myInfo?email=~s&password=~s" (string->symbol (login-email cred)) (string->symbol (login-password cred))))
            
            ;; converts url to string
            (define login-url(string->url url))
            
            ;; makes GET request to iSENSE API, and display results (for debugging)
            (define response (read-line (get-pure-port login-url)))
            (define response-msg (substring response 2 5))
            
            ;; validate user name and password are correct
            (if (equal? response-msg "msg")
                ;; render login page with error if unsuccessfull
                (render-sign-in-page-error)
                ;; render project selection page if successful
                (render-select-project-page  (login-email cred) (login-password cred))))]
    (send/suspend/dispatch sign-in)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; LOGIN ERROR PAGE

;; render-sign-in-page-error: request -> doesn't return
;; renders login page w/ error message
(define (render-sign-in-page-error)
  (local [(define (sign-in embed/url)
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
               (body 
                (div ([class "container"])
                     (div ([class "main"])
                          ;; error message
                          (div ([class "alert alert-danger"] [role "alert"]) "The email and password you entered in does not match our records. Please try again.")
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
                          (form ([action ,(embed/url login-handler)]  [class "form-horizontal"])
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
            (define response (read-line (get-pure-port login-url)))
            (define response-msg (substring response 2 5))
            
            ;; validate user name and password are correct
            (if (equal? response-msg "msg")
                ;; render login page with error if unsuccessfull
                (render-sign-in-page-error)
                ;; render project selection page if successful
                (render-select-project-page  (login-email cred) (login-password cred))))]
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
               (body(div ([class "container"])
                         (div ([class "main"])
                              ;;  bread crumbs
                              (p ([class "breadcrumbs"]) (span ([class "current-tab"]) "Select Project") " > Project Title > Add Media Objects > Finish")
                              (h1 "Project Selection")
                              (p ([class "lead"] [style "margin-bottom: 50px;"])"Choose from the list of pre-existing projects:")
                              
                              ;; Row 1 of Panels-------------------------------------------
                              (div ([class "row"])
                                   ;; Gummy Bear Project----------------------  
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"]) "Gummy Bear Lab"))
                                             (div ([class "panel-body"])
                                                  (p "Determine how a gummy bear changes when it is soaked in water overnight.")
                                                  (img ([src "http://www.acclaimmag.com/wp-content/uploads/2014/01/2074903820_1375612450.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "1"] [style "visibility: hidden; height: 0px;"]))))))
                                   ;; Measuring Heart Rate----------------------  
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Measuring Heart Rate"))
                                             (div ([class "panel-body"])
                                                  (p "Learn one method of testing aerobic activity for heart recovery.")
                                                  (img ([src "http://www.runnersgoal.com/wp-content/uploads/2013/07/heartratezone.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "2"] [style "visibility: hidden; height: 0px;"])))))))
                              
                              ;; Row 2 of Panels-------------------------------------------
                              (div ([class "row"])
                                   ;; Heat Absorption Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Heat Absorption"))
                                             (div ([class "panel-body"])
                                                  (p "Determine why you stay cooler in lighter clothes than darker clothes in the hot sun.")
                                                  (img ([src "http://www.bakesforbreastcancer.org/wp-content/uploads/2012/03/sun.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "3"] [style "visibility: hidden; height: 0px;"]))))))
                                   ;; Holding Your Breath Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Holding Your Breath"))
                                             (div ([class "panel-body"])
                                                  (p "Explore how your body handles holding your breath.")
                                                  (img ([src "http://www.itsallyogababy.com/wp-content/uploads/2013/01/breathing.jpeg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "2"] [style "visibility: hidden; height: 0px;"])))))))
                              
                              ;; Row 3 of Panels-------------------------------------------
                              (div ([class "row"])
                                   ;; Freefall Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Freefall"))
                                             (div ([class "panel-body"])
                                                  (p "Determine the relationship between distance and time for free-falling objects.")
                                                  (img ([src "http://ecx.images-amazon.com/images/I/31E3Z3sDcrL._SY300_.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "5"] [style "visibility: hidden; height: 0px;"]))))))
                                   ;; Bone Length and Height Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Correlation of Bone Length and Height"))
                                             (div ([class "panel-body"])
                                                  (p "Determine is there is a relationship between a person's bone size and overall height.")
                                                  (img ([src "http://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Human_arm_bones_diagram.svg/1167px-Human_arm_bones_diagram.svg.png"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "6"] [style "visibility: hidden; height: 0px;"])))))))
                              
                              ;; Row 4 of Panels-------------------------------------------
                              (div ([class "row"])
                                   ;; Density of Objects Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Density of Objects"))
                                             (div ([class "panel-body"])
                                                  (p "Compare the density of various materials.")
                                                  (img ([src "http://isenseproject.org/media/99/99efbe54c90a72a1115faade22a5575f/plastic-counters.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "7"] [style "visibility: hidden; height: 0px;"]))))))
                                   ;; Dice Rolls Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Distribution of Dice Rolls"))
                                             (div ([class "panel-body"])
                                                  (p "Understand the concepts of theoretical and experimental probability.")
                                                  (img ([src "http://isenseproject.org/media/35/35270c9752997a472374aec0ce72b6e6/cover%20picture.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "8"] [style "visibility: hidden; height: 0px;"])))))))
                              ;; Row 5 of Panels-------------------------------------------
                              (div ([class "row"])
                                   ;; Hooke's Law and Spring Constants Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Hooke's Law and Spring Constants"))
                                             (div ([class "panel-body"])
                                                  (p "Understand the concepts of Hooke's Law and spring constants.")
                                                  (img ([src "http://isenseproject.org/media/37/3724d70818ac49716c81aa845919b764/cover%20picture.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (p " ")
                                                  (p " ")
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "9"] [style "visibility: hidden; height: 0px;"]))))))
                                   ;; Replacement Probability Project----------------------
                                   (div ([class "col-sm-6"])
                                        (div ([class "panel panel-primary"])
                                             (div ([class "panel-heading"])
                                                  (h4 ([class "panel-title"])"Single Draw with Replacement Probability with Two Distributions"))
                                             (div ([class "panel-body"])
                                                  (p "Determine the quantity of each color of the object in a container.")
                                                  (img ([src "http://isenseproject.org/media/9f/9f4e88e4fb694cc4ea141fbadbaab685/skittles.jpg"] 
                                                        [class "img-thumbnail"]
                                                        [alt "Thumbnail Image"]))
                                                  (form ([action,(embed/url create-project)] [class "form-horizontal"])
                                                        (button ([type "submit"][class "btn btn-success"][style "margin-top: 10px;"]) "Create Project")
                                                        (input ([type "number"] [class "form-control"] [name "id"] [value "10"] [style "visibility: hidden; height: 0px;"])))))))))))))
          
          
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
                                    (p ([class "breadcrumbs"]) "Select Project > " (span ([class "current-tab"]) "Project Title") " > Add Media Objects > Finish")
                                    (h1 "Your Project's Title")
                                    ;; project title form
                                    (form ([class "form-horizontal"] [action ,(embed/url make-online-project)])
                                          ;; project title, the default title is the value, but can be changed by the user
                                          (div ([class "form-group"])
                                               (label ([for "title"]))
                                               (center (input ([type "text"] [class "form-control"] [style "width: 60%;"] [name "title"] [value ,(title (find-project (string->number id)))]))))
                                          ;; next button
                                          (button ([type "submit"] [class "btn btn-success"]) "Continue"))))))))
          (define (make-online-project request)
            ;; search for project in database
            (define project (find-project (string->number id)))
            
            ;; create the project with the given name using isense-racket API
            ;; credentials, title of project, fields
            (let ((proj (isense-create-project (isense-credentials-pass email pass)
                                               (parse-title (request-bindings request))
                                               (make-fields (fields (find-project (string->number id)))))))
              ;; id of project on iSENE
              (define id (proj 'id))
              (render-media-objects-page id project)))]
    (send/suspend/dispatch project-title)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; MEDIA OBJECTS PAGE

;; render-media-objects-page request -> doesn't return
;; renders media objects page
(define (render-media-objects-page id project)
  (local [(define (media-objects embed/url)
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
                                    (p ([class "breadcrumbs"]) "Select Project > Project Title > "(span ([class "current-tab"]) "Add Media Objects")" > Finish")
                                    (h1 "Add Media Objects to Your Project")
                                    (p ([class "lead"]) "Here are the media objects to upload to your project:")
                                    ;; project image link 
                                    (div ([class "row"])
                                         (img ([src ,(picture-url project)] [class "img-media-object"])))
                                    (div ([class "row"] [style "margin-bottom: 25px;"])
                                         (a ([href ,(picture-url project)]) ,(picture-url project)))
                                    ;; project instructions link
                                    (div ([class "row"])
                                         (img ([src "http://www.conservationassured.org/cats_wp/wp-content/uploads/2013/10/PDF-icon.png"] [class "img-media-object"])))
                                    (div ([class "row"] [style "margin-bottom: 25px;"])
                                         (a ([href ,(instructions-url project)]) ,(instructions-url project)))
                                    ;; media objects form - continues to finish page
                                    (form ([class "form-horizontal"] [action ,(embed/url add-media-objects)])
                                          (button ([type "submit"] [class "btn btn-success"]) "Continue"))))))))
          (define (add-media-objects request)
            (render-finish-page id))]
    (send/suspend/dispatch media-objects)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; FINISH PAGE

;; render-finish-page request -> doesn't return
;; renders finish page
(define (render-finish-page id)
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
                                    (p ([class "breadcrumbs"]) "Select Project > Project Title > Add Media Objects > " (span ([class "current-tab"]) "Finish"))
                                    (h1 "Congrats, you're finished!")
                                    (p ([class "lead"]) "Here's the link to your project on iSENSE:")
                                    ;; project link
                                    (a ([href ,(format "http://isenseproject.org/projects/~s" id)]) ,(format "http://isenseproject.org/projects/~s" id))))))))]
    (send/suspend/dispatch finish-page)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; START SERVER

;; start web server
(serve/servlet render-sign-in-page
               #:extra-files-paths
               (list
                ;; IMPORTANT: MUST CHANGE PATH FOR CSS TO WORK
                (build-path "/Users/kaitlyncarcia/Repos/iLambda/") "htdocs"))
;;(build-path "/home/ravythok/opl/iLambda") "style"))
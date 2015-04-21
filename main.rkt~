#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (app req)
  ;; Sign in page
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
                          (form ([class "form-horizontal"])
                                ;; Email
                                (div ([class "form-group"])
                                     (label ([for "email"] [class "col-xs-2 col-sm-2 control-label"]) "Email")
                                     (div ([class "col-xs-10 col-sm-10"])
                                          (input ([type "email"] [class "form-control"]))))
                                ;; Password
                                (div ([class "form-group"])
                                     (label ([for "password"] [class "col-xs-2 col-sm-2 control-label"]) "Password")
                                     (div ([class "col-xs-10 col-sm-10"])
                                          (input ([type "password"] [class "form-control"]))))
                                ;; Login button
                                (button ([type "submit"] [class "btn btn-primary"]) "Login"))
                          (p "Don't have an iSENSE account?"
                             (a ([href "http://isenseproject.org/users/new"]) " Click here to register")
                             ".")))))))

;; Start web application
;; Includes path to CSS file
(serve/servlet app
               #:extra-files-paths
               (list
                ;; IMPORTANT: Ravy, you will need to change this path to work w/ your computer
                (build-path "/Users/kaitlyncarcia/Documents/UML2015/OPL/Project") "htdocs"))


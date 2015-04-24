#lang web-server/insta

(require "isense-racket/isense-3.rkt")

;; login is a (login email password)
;; email is a string, and password is a string
(struct login (email password))

;; start: request -> doesn't return
;; consumes a request and produces a page that displays all of the web content
(define (start request)
  (render-cred-page request))

;; parse-login: bindings -> login
;; extracts a login out of the bindings (including email and password)
(define (parse-login bindings)
  (login (extract-binding/single 'email bindings)
         (extract-binding/single 'password bindings)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; LOGIN PAGE
;; render-cred-page: request -> doesn't return
;; renders login page
(define (render-cred-page request)
  (local [(define (sign-in embed/url)
            (response/xexpr
             `(html
               ;; Title of the page
               (title "iLambda - Sign In")
               ;; Favicon
               (link ([rel "shortcut icon"] [href "/favicon.ico"]))
               ;; Link to boostrap styles
               (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
               ;; Link to our style sheet
               (link ([rel "stylesheet"] [href "/style.css"]))
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
                               ;; Sign in form
                               (form ([action,(embed/url insert-login-handler)]  [class "form-horizontal"])
                                     ;; Email
                                     (div ([class "form-group"])
                                          (label ([for "email"] [class "col-xs-2 col-sm-2 control-label"]) "Email")
                                          (div ([class "col-xs-10 col-sm-10"])
                                               (input ([type "email"] [class "form-control"] [name "email"]))))
                                     ;; Password
                                     (div ([class "form-group"])
                                          (label ([for "password"] [class "col-xs-2 col-sm-2 control-label"]) "Password")
                                          (div ([class "col-xs-10 col-sm-10s"])
                                               (input ([type "password"] [class "form-control"] [name "password"]))))
                                     ;; Login button
                                     (button ([type "submit"] [class "btn btn-primary"]) "Login"))
                               (p "Don't have an iSENSE account?"
                                  (a ([href "http://isenseproject.org/users/new"]) " Click here to register")
                                  ".")
                               ))))))
          (define (insert-login-handler request)
            ;; gets credentials
            (define cred  (parse-login (request-bindings request)))
            
            ;; creates url with credentials as params
            (define url (format "http://isenseproject.org/api/v1/users/myInfo?email=~s&password=~s" (string->symbol (login-email cred)) (string->symbol (login-password cred))))
            
            ;; converts url to string
            (define login-url(string->url url))
            
            ;; makes GET request to iSENSE API, and display results (for debugging)
            (display-pure-port (get-pure-port login-url))
            
            ;; TO-DO: valid user name and password are valid
            (if (eq? #t #t)
                ;; Render project selection page if successful
                (render-project-selection-page  (login-email cred) (login-password cred))
                ;; TO DO: user fails, display message?
                (display "you failed.")))]
    
    (send/suspend/dispatch sign-in)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; PROJECT SELECTION PAGE
;; render-project-selection-page request -> doesn't return
;; renders login page
(define (render-project-selection-page email pass)
  (local [(define (sign-in embed/url)
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
                                    (form ([class "form-horizontal"][action,(embed/url create-project)])
                                          ;; Project title
                                          (div ([class "form-group"])
                                               (label ([for "title"]))
                                               (div (input ([type "text"] [class "form-control"] [value "The Bone Lab"]))))
                                          ;; Prev and next buttons
                                          (button ([type "submit"] [class "btn btn-danger prev"]) "Prev")
                                          (button ([type "submit"] [class "btn btn-success"]) "Save & Continue"))))))))
          (define (create-project request)
            (define cred (isense-credentials-pass email pass))
            (define fields (isense-project-field "Number" 2 "deg" ""))
            (define a (isense-create-project cred "Test Project yay!" (list fields)))

            (render-project-selection-page3)
            (display "you failed."))]
    
    (send/suspend/dispatch sign-in)))

;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; PROJECT TITLE PAGE
(define (render-project-selection-page3)
  (local [(define (sign-in embed/url)
            (response/xexpr
             `(html
               ;; Title of the page
               (title "iLambda - Sign In")
               ;; Favicon
               (link ([rel "shortcut icon"] [href "/favicon.ico"]))
               ;; Link to boostrap styles
               (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
               ;; Link to our style sheet
               (link ([rel "stylesheet"] [href "/style.css"]))
               (body (div ([class "container"])
                          (div ([class "main"])
                               ))))))
          (define (insert-login-handler request)
            (define cred  (parse-login (request-bindings request)))
            (define url (format "http://isenseproject.org/api/v1/users/myInfo?email=~s&password=~s" (string->symbol (login-email cred)) (string->symbol (login-password cred))))
            (define test-url(string->url url))
            (display-pure-port (get-pure-port test-url))
            (if (eq? #t #t)
                (render-project-selection-page3)
                (display "you failed.")))]
    
    (send/suspend/dispatch sign-in)))
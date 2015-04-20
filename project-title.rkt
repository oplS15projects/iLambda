#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (app req)
  ;; Project title page
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
                                (button ([type "submit"] [class "btn btn-success"]) "Save & Continue"))))))))

;; Start web application
;; Includes path to CSS file
(serve/servlet app
               #:extra-files-paths
               (list
                ;; IMPORTANT: Ravy, you will need to change this path to work w/ your computer
                (build-path "/Users/kaitlyncarcia/Repos/iLambda") "htdocs"))

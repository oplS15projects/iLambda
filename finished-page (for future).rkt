#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (app req)
  ;; Project title page
  (response/xexpr
   `(html (head
           ;; Title of the page
           (title "iLambda - You're Finished!")
           ;; Favicon
           (link ([rel "shortcut icon"] [href "/favicon.ico"]))
           ;; Link to boostrap styles
           (link ([rel "stylesheet"] [href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"]))
           ;; Link to our style sheet
           (link ([rel "stylesheet"] [href "/style.css"])))
          (body (div ([class "container"])
                     (div ([class "main"])
                          ;;  Bread crumbs
                          (p ([class "breadcrumbs"]) "Select Project > Project Title > Add Media Objects > Add Contibutor Keys > " (span ([class "current-tab"]) "Finish"))
                          (h1 "Congrats, you're finished!")
                          (p ([class "lead"]) "Here's the link to your project on iSENSE:")
                          ;; Project link
                          (label ([for "project-link"]))
                          (div (input ([type "text"] [class "form-control"] [value "http://isenseproject.org/projects/614"])))))))))

;; Start web application
;; Includes path to CSS file
(serve/servlet app
               #:extra-files-paths
               (list
                ;; IMPORTANT: Ravy, you will need to change this path to work w/ your computer
                (build-path "/Users/kaitlyncarcia/Repos/iLambda") "htdocs"))

#lang racket

(require json)
(require net/url)
(require "quick-net.rkt")

; api definition
(provide (contract-out
          [isense-url string?]
          ; (isense-project-field name type unit restrictions)
          [isense-project-field (-> string? number? string? string? any)]
          ; (isense-credentials-ckey ckey name)
          [isense-credentials-ckey (-> string? string? any)]
          ; (isense-credentials-pass email pass)
          [isense-credentials-pass (-> string? string? any)]
          ; (isense-search str (sort "updated_at") (order "DESC") (count -1))
          [isense-search (->* (string?) (string? string? number?) any)]
          ; (isense-get-project-by-id id)
          [isense-get-project-by-id (-> number? any)]
          ; (isense-get-dataset-by-id id)
          [isense-get-dataset-by-id (-> number? any)]
          ; (isense-get-field-by-id id)
          [isense-get-field-by-id (-> number? any)]
          ; (isense-create-project cred name fields)
          [isense-create-project (-> (-> symbol? any) string? list? any)]
          ; (isense-add-field project cred type name units restrictions)
          [isense-add-field (-> (-> symbol? any) (-> symbol? any) number? string? string? string? any)]
          ; (isense-add-dataset project cred data name)
          [isense-add-dataset (-> (-> symbol? any) (-> symbol? any) list? string? any)]
          ; (isense-append-dataset dset cred data)
          [isense-append-dataset (-> (-> symbol? any) (-> symbol? any) list? any)]
          ))

; isense-url
; url to use for isense
(define isense-url
  "http://isenseproject.org")

; make-project
; given the data, make a function that returns each data element
; arguments:
;   id = project id
;   name = name of project
;   fields = ids of the fields in the project
;   datasets = ids of the datasets in the project, this can be void? to determine these
; arguments to returned function:
;   msg = 'id, 'name, 'fields, or 'datasets - returns corresponding value
(define (make-project id name fields datasets)
  
  (define (return-datasets)
    (if (void? datasets)
        (set! datasets #t)
        (void))
    datasets)
    
  (lambda (msg)
    (case msg
      ((id) id)
      ((name) name)
      ((fields) fields)
      ((datasets) (return-datasets))
      (else (error (string-append "make-project: Invalid option specified - " (symbol->string msg)))))))

; make-dataset
; given information about the dataset, create a function that returns that information
; arguments:
;   id = id of the dataset
;   name = name of the dataset
;   fields = ids of the fields in the dataset
;   data = a list of lists containing each datapoint in the dataset
; arguments of returned function:
;   msg = 'id, 'name, 'fields, 'data - returns corresponding value
(define (make-dataset id name fields data)
  (lambda (msg)
    (case msg
      ((id) id)
      ((name) name)
      ((fields) fields)
      ((data) data)
      (else (error (string-append "make-dataset: Invalid option specified - " (symbol->string msg)))))))

; make-field
; given information about a field, create a function that returns that information
; arguments:
;   id = id of the field
;   name = name of the field
;   type = type of the field
;   unit = unit of measurement for the field
;   restrictions = (works for strings only) valid values for the field
(define (make-field id name type unit restrictions)
  
  (lambda (msg)
    (case msg
      ((id) id)
      ((name) name)
      ((type) type)
      ((unit) unit)
      ((restrictions) restrictions)
      (else (error (string-append "make-field: Invalid option specified - " (symbol->string msg)))))))

; isense-project-field
; a version of make-field accessible to the user of the library, lacks a field ID
; used with isense-create-project
; arguments:
;   name = name of the field
;   type = type of the field
;   unit = unit of measurement for the field
;   restrictions = (works for strings only) valid values for the field
(define (isense-project-field name type unit restrictions)
  (lambda (msg)
    (case msg
      ((name) name)
      ((type) type)
      ((unit) unit)
      ((restrictions) restrictions)
      (else (error (string-append "make-field: Invalid option specified - " (symbol->string msg)))))))

; isense-credentials-ckey
; creates an object that holds a contribution key and contributor name
; arguments:
;   ckey = the contribution key
;   name = the contributor name (not really sure if this needs to be anything particular)
(define (isense-credentials-ckey ckey name)
  (lambda (msg)
    (case msg
      ((type) 'contribution-key)
      ((ckey) ckey)
      ((name) name)
      (else (error (string-append "isense-credentials-user: Invalid option specified - " (symbol->string msg)))))))

; isense-credentials-pass
; creates an object that holds an email and password
; arguments:
;   email = the contribution key
;   pass = the contributor name (not really sure if this needs to be anything particular)
(define (isense-credentials-pass email pass)
  (lambda (msg)
    (case msg
      ((type) 'login-password)
      ((email) email)
      ((password) pass)
      (else (error (string-append "isense-credentials-pass: Invalid option specified - " (symbol->string msg)))))))

; isense-search
; searches for projects that contain the string str somewhere in their name
; returns a list of project ids
; arguments:
;   str = string to search for
;   sort = criteria to sort the results by
;   order = either "ASC" or "DESC", short for ascending for descending
;   count = number of results to return, defaults to -1 (all results)
(define (isense-search str (sort "updated_at") (order "DESC") (count -1))
  (define (search page count)
    (let* ((get-num (if (and (< count 50) (>= count 0)) count 50))
           (request (string-append isense-url
                                   "/api/v1/projects?search=" str
                                   "&sort=" sort
                                   "&order=" order
                                   "&page=" (number->string page)
                                   "&per_page=" (number->string get-num)))
           (response (read-json (get-pure-port (string->url request))))
           (rest (if (= (length response) 0)
                     '()
                     (search (+ page 1) (- count get-num)))))
      (append (map (lambda (x) x)
                   response)
              rest)))
  
  (define (parse-project expr)
    (make-project (json-burrow expr 'id)
                  (json-burrow expr 'name)
                  (map (lambda (x)
                         (json-burrow expr 'id))
                       (json-burrow expr 'fields))
                  (void)))
  
  (define (parse expr)
    (map parse-project expr))
    
  (parse (search 1 count)))

; get-project-by-id
; given an id, return the project associated with that ID
; arguments:
;   id = the id of the project
(define (isense-get-project-by-id id)
  (let* ((request (string-append isense-url "/api/v1/projects/" (number->string id)
                                 "?recur=true"))
         (response (read-json (get-pure-port (string->url request)))))
    (if (false? (json-burrow response 'id))
        #f
        (make-project (json-burrow response 'id)
                      (json-burrow response 'name)
                      (map (lambda (x)
                             (json-burrow x 'id))
                           (json-burrow response 'fields))
                      (map (lambda (x)
                             (json-burrow x 'id))
                           (json-burrow response 'dataSets))))))

; get-dataset-by-id
; given a dataset id, return the dataset associated with that ID
; arguments:
;   id = the id of the dataset
(define (isense-get-dataset-by-id id)
  (let* ((request (string-append isense-url "/api/v1/data_sets/" (number->string id)
                                 "?recur=true"))
         (response (read-json (get-pure-port (string->url request)))))
    
    (define (get-column field-id)
      (cons field-id (map (lambda (x)
                            (json-burrow x (string->symbol (number->string field-id))))
                          (json-burrow response 'data))))
    
    (make-dataset (json-burrow response 'id)
                  (json-burrow response 'name)
                  (map (lambda (x)
                         (json-burrow x 'id))
                       (json-burrow response 'fields))
                  (map (lambda (x)
                         (get-column (json-burrow x 'id)))
                       (json-burrow response 'fields)))))
                  

; isense-get-field-by-id
; given a field id, return the field associated with that ID
; arguments:
;   id = the id of the field
; returns a field object representing the queried field, or false if it failed
(define (isense-get-field-by-id id)
  (let* ((request (string-append isense-url "/api/v1/fields/" (number->string id)))
         (response (read-json (get-pure-port (string->url request)))))
    (if (false? (json-burrow response 'id))
        #f
        (make-field (json-burrow response 'id)
                    (json-burrow response 'name)
                    (json-burrow response 'type)
                    (json-burrow response 'unit)
                    (json-burrow response 'restrictions)))))

; isense-create-project
; given a name, credentials, and a list of fields, make a project
; arguments:
;   cred = the credentials (either contrib key or email/password)
;   name = the name of the new project
;   fields = a list of field objects to add to the new project
; returns a project object representing the new project, or false if it failed
(define (isense-create-project cred name fields)
  
  (let* ((request (string-append isense-url "/api/v1/projects?recur=true"))
         (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                         'email (cred 'email)
                                                         'password (cred 'password)
                                                         'project_name name))))
         (response (read-json (post-pure-port (string->url request)
                                              postdata
                                              '("Content-Type: application/json"))))
         (temp-proj (make-project (json-burrow response 'id) "" '() '())))
    
    (if (false? (json-burrow response 'id))
        (void)
        (for-each (lambda (x)
                    (display (isense-add-field temp-proj
                                               cred
                                               (x 'type)
                                               (x 'name)
                                               (x 'unit)
                                               (x 'restrictions))))
                  fields))
    
    (if (false? (json-burrow response 'id))
        #f
        (begin
          (display response)
          (display (json-burrow response 'id))
          (display (json-burrow response 'name))
          (display (json-burrow response 'dataSets))
          (display (json-burrow response 'fields))
          (make-project (json-burrow response 'id)
                        (json-burrow response 'name)
                        (map (lambda (x)
                               (json-burrow response 'id))
                             (json-burrow response 'fields))
                        (map (lambda (x)
                               (json-burrow response 'id))
                             (if (false? (json-burrow response 'dataSets))
                                 '()
                                 (json-burrow response 'dataSets))))))))

; isense-add-field
; given a project and credentials, add a field to that project
; arguments:
;   project = the project you want to add the field to
;   cred = the credentials (either contrib key or email/password)
;   type = the type of field
;   name = the name of the field
;   units = units of measurement the field is in
;   restrictions = (for text fields only) valid values for the field
; returns an object representing the new field, or false if it failed
(define (isense-add-field project cred type name units (restrictions ""))
  
  (let* ((request (string-append isense-url "/api/v1/fields"))
         (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                         'email (cred 'email)
                                                         'password (cred 'password)
                                                         'field (hash
                                                                 'project_id (project 'id)
                                                                 'field_type type
                                                                 'name name
                                                                 'units units
                                                                 'restrictions restrictions)))))
         (response (read-json (post-pure-port (string->url request)
                                              postdata
                                              '("Content-Type: application/json")))))
    (if (false? (json-burrow response 'id))
        #f
        (make-field (json-burrow response 'id)
                    (json-burrow response 'name)
                    (json-burrow response 'type)
                    (json-burrow response 'unit)
                    (json-burrow response 'restrictions)))))

; isense-add-dataset
; given a project, add a dataset to that project
;   project = a project function to add the data set to
;   data = a list of lists, the data to add to the dataset
;   name = name of the dataset
;   cred = the credentials (either contrib key or email/password)
; returns a dataset object representing the dataset created, or false if it failed
(define (isense-add-dataset project cred data name)
  
  (define (build-data-table data)
    (apply hash (foldl (lambda (first rest)
                         (append (list (string->symbol (number->string (car first))) (cdr first)) rest))
                       '()
                       data)))
  
  (define (upload-ckey)
    (let* ((request (string-append isense-url "/api/v1/projects/" (number->string (project 'id))
                                   "/jsonDataUpload"))
           (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                           'title name
                                                           'contribution_key (cred 'ckey)
                                                           'contributor_name (cred 'name)
                                                           'data (build-data-table data)))))
           (response (read-json (post-pure-port (string->url request)
                                                postdata
                                                '("Content-Type: application/json")))))
    
      (display response)
      
      (define (get-column field-id)
        (cons field-id (map (lambda (x)
                              (json-burrow x (string->symbol (number->string field-id))))
                            (json-burrow response 'data))))
      
      (if (false? (json-burrow response 'id))
          #f
          (make-dataset (json-burrow response 'id)
                        (json-burrow response 'name)
                        (map (lambda (x)
                               (json-burrow x 'id))
                             (json-burrow response 'fields))
                        (map (lambda (x)
                               (get-column (json-burrow x 'id)))
                             (json-burrow response 'fields))))))
    
  (define (upload-pass)
    (let* ((request (string-append isense-url "/api/v1/projects/" (number->string (project 'id))
                                   "/jsonDataUpload"))
           (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                           'title name
                                                           'email (cred 'email)
                                                           'password (cred 'password)
                                                           'data (build-data-table data)))))
           (response (read-json (post-pure-port (string->url request)
                                                postdata
                                                '("Content-Type: application/json")))))
    
      (define (get-column field-id)
        (cons field-id (map (lambda (x)
                              (json-burrow x (string->symbol (number->string field-id))))
                            (json-burrow response 'data))))
      
      (if (false? (json-burrow response 'id))
          #f
          (make-dataset (json-burrow response 'id)
                        (json-burrow response 'name)
                        (map (lambda (x)
                               (json-burrow x 'id))
                             (json-burrow response 'fields))
                        (map (lambda (x)
                               (get-column (json-burrow x 'id)))
                             (json-burrow response 'fields))))))
  
  (cond
    ((eq? (cred 'type) 'contribution-key) (upload-ckey))
    ((eq? (cred 'type) 'login-password) (upload-pass))))

; isense-append-dataset
; given a dataset and some data, append the data to the end of the dataset
; arguments:
;   dset = the dataset you wish to append to
;   cred = the credentials (either contrib key or email/password)
;   data = the data to append
; returns a dataset object representing the dataset appended to, or false if it failed
(define (isense-append-dataset dset cred data)
  
  (define (build-data-table data)
    (apply hash (foldl (lambda (first rest)
                         (append (list (string->symbol (number->string (car first))) (cdr first)) rest))
                       '()
                       data)))
  
  (define (upload-ckey)
    (let* ((request (string-append isense-url "/api/v1/data_sets/append"))
           (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                           'id (dset 'id)
                                                           'title (dset 'name)
                                                           'contributor_name (cred 'name)
                                                           'contribution_key (cred 'ckey)
                                                           'data (build-data-table data)))))
           (response (read-json (post-pure-port (string->url request)
                                                postdata
                                                '("Content-Type: application/json")))))
    
      (define (get-column field-id)
        (cons field-id (map (lambda (x)
                              (json-burrow x (string->symbol (number->string field-id))))
                            (json-burrow response 'data))))
      
      (if (false? (json-burrow response 'id))
          #f
          (make-dataset (json-burrow response 'id)
                        (json-burrow response 'name)
                        (map (lambda (x)
                               (json-burrow x 'id))
                             (json-burrow response 'fields))
                        (map (lambda (x)
                               (get-column (json-burrow x 'id)))
                             (json-burrow response 'fields))))))
  
  (define (upload-pass)
    (let* ((request (string-append isense-url "/api/v1/data_sets/append"))
           (postdata (string->bytes/utf-8 (jsexpr->string (hash
                                                           'id (dset 'id)
                                                           'email (cred 'email)
                                                           'password (cred 'password)
                                                           'data (build-data-table data)))))
           (response (read-json (post-pure-port (string->url request)
                                                postdata
                                                '("Content-Type: application/json")))))
      
      (define (get-column field-id)
        (cons field-id (map (lambda (x)
                              (json-burrow x (string->symbol (number->string field-id))))
                            (json-burrow response 'data))))
      
      (if (false? (json-burrow response 'id))
          #f
          (make-dataset (json-burrow response 'id)
                        (json-burrow response 'name)
                        (map (lambda (x)
                               (json-burrow x 'id))
                             (json-burrow response 'fields))
                        (map (lambda (x)
                               (get-column (json-burrow x 'id)))
                             (json-burrow response 'fields))))))
  
  (cond
    ((eq? (cred 'type) 'contribution-key) (upload-ckey))
    ((eq? (cred 'type) 'login-password) (upload-pass))))

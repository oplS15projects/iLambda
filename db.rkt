#lang racket

(require "isense-racket-api/isense-3.rkt")

;; functions provided to main.rkt
(provide title)
(provide find-project)
(provide make-fields)
(provide fields)
(provide picture-url)
(provide instructions-url)

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


#lang racket

(require "isense-racket/isense-3.rkt")

;; Define empty database
(define projectDB '())

;; Returns a project entry
(define (make-project id instructions-url picture-url title fields)
  (list id instructions-url picture-url title fields))

;; Inserts project into database
(define (insert-project project)
  (set! projectDB (append projectDB (list project))))

;; Returns instruction url of project
(define (id project)
  (car project))

;; Returns picture url of project
(define (instructions-url project)
  (cadr project))

;; Returns title of project
(define (picture-url project)
  (caddr project))

;; Returns list of fields of project
(define (title project)
  (cadddr project))

(define (fields project)
  (car (cddddr project)))

(define (type field)
  (car field))

(define (name field)
  (cadr field))

(define (units field)
  (caddr field))

(define (restrictions field)
  (cadddr field))
  
;; TO-DO:
;; Add procedures to parse through fields

;; Returns project with given id
(define (find-project id)
  (filter (lambda (x) (= (car x) id)) projectDB))

(define (make-fields fields)
  (if (null? fields)
      '()
      (cons (isense-project-field (name fields) (type fields) (units fields) (restrictions fields)) (make-fields (cdr fields)))))
      

;; ------------------------------------------------------------------------------------------------------------------------------------------------

;; TO-DO:
;; Potentially may need to rework URLs and how we are passing objects to website

;; Gummy Bear
(insert-project (make-project 1
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/gummy-bear.pdf"
                              "http://www.acclaimmag.com/wp-content/uploads/2014/01/2074903820_1375612450.jpg"
                              "Gummy Bear Lab"
                              (list (list "number" "Length" "m")
                                    (list "number" "Width" "m")
                                    (list "number" "Height" "m")
                                    (list "number" "Volume" "m^3")
                                    (list "number" "Mass" "oz")
                                    (list "text" "Day" (list "Day 1" "Day 2"))
                                    (list "text" "Color" (list "Red" "Yellow" "Orange" "Green" "Colorless" "Blue" "Purple")))))

;; Measuring Heart Rate
(insert-project (make-project 2
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/measuring-heart-rate.pdf"
                              "http://www.runnersgoal.com/wp-content/uploads/2013/07/heartratezone.jpg"
                              "Measuring Heart Rate"
                              (list (list "number" "Heart Rate" "")
                                    (list "number" "Time Elapsed" "seconds"))))


;; Heat Absorption
(insert-project (make-project 3
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/heat-absorption.pdf"
                              "http://www.bakesforbreastcancer.org/wp-content/uploads/2012/03/sun.jpg"
                              "Heat Absorption"
                              (list "timestamp"
                                    (list "text" "Glass" (list "Light" "Dark"))
                                    (list "number" "Temperature" "C"))))

;; Holding Your Breath
(insert-project (make-project 4
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/holding-your-breath.pdf"
                              "http://www.itsallyogababy.com/wp-content/uploads/2013/01/breathing.jpeg"
                              "Holding Your Breath"
                              (list (list "number" "Breathing Normally" "seconds")
                                    (list "number" "Breathing in Bag" "seconds"))))

;; Freefall
(insert-project (make-project 5
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/freefall.pdf"
                              "http://ecx.images-amazon.com/images/I/31E3Z3sDcrL._SY300_.jpg"
                              "Freefall"
                              (list (list "number" "Time" "seconds")
                                    (list "number" "Distance" "meters"))))

;; Correlation of Bone Length and Height Activity
(insert-project (make-project 6
                              "http://weblab.cs.uml.edu/~kcarcia/OPLProject/Correlation-of%20Bone-Length-and%20Height-Activity.pdf"
                              "http://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Human_arm_bones_diagram.svg/1167px-Human_arm_bones_diagram.svg.png"
                              "Correlation of Bone Length and Height Activity"
                              (list (list "number" "Tibia" "cm")
                                    (list "number" "Fibula" "cm")
                                    (list "number" "Femur" "cm")
                                    (list "number" "Total Height" "cm"))))

;; Density of Objects
(insert-project (make-project 7
                              "http://isenseproject.org/media/06/06db85208e88aa120233d756b3550d5b/DensityofObjects.pdf"
                              "http://isenseproject.org/media/99/99efbe54c90a72a1115faade22a5575f/plastic-counters.jpg"
                              "Density of Objects"
                              (list (list "number" "Mass" "g")
                                    (list "number" "Volume" "mL")
                                    (list "number" "Density" "g/mL")
                                    (list "text" "Object Type" (list "Glass Marble" "Rock" "Plastic toy" "Zinc washer")))))

;; Distribution of Dice Rolls
(insert-project (make-project 8
                              "http://isenseproject.org/media/f2/f208d643de0018c596313ad191165e21/Distribution%20of%20Dice%20Rolls.pdf"
                              "http://isenseproject.org/media/35/35270c9752997a472374aec0ce72b6e6/cover%20picture.jpg"
                              "Distribution of Dice Rolls"
                              (list (list "number" "Trial" '())
                                    (list "number" "White Dice" '())
                                    (list "number" "Yellow Dice" '())
                                    (list "number" "Sum" '()))))

;; Hooke's Law and Spring Constants
(insert-project (make-project 9
                              "http://isenseproject.org/media/48/480dd73dac5829aad56f98256449bd8f/Hooke's%20Law%20and%20Spring%20Constants.pdf"
                              "http://isenseproject.org/media/37/3724d70818ac49716c81aa845919b764/cover%20picture.jpg"
                              "Hooke’s Law and Spring Constants"
                              (list (list "number" "Mass" "g")
                                    (list "number" "Displacement" "cm")
                                    (list "text" "Spring Label (a, b, c)" (list "a" "b" "c")))))

;; Single Draw with Replacement Probability with Two Distributions
(insert-project (make-project 10
                              "http://isenseproject.org/media/3a/3a7e0f0d3f93543bb96d5200df7fdf93/Single_Draw_with_Replacement-Two_Distributions.pdf"
                              "http://isenseproject.org/media/9f/9f4e88e4fb694cc4ea141fbadbaab685/skittles.jpg"
                              "Single Draw with Replacement Probability with Two Distributions"
                              (list (list "number" "Red" "")
                                    (list "number" "Blue" "")
                                    (list "number" "Yellow" "")
                                    (list "text" "Bag Color" (list "Green" "Pink")))))



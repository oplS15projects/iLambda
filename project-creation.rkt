#lang racket

(require "isense-racket/isense-3.rkt")

(define cred (isense-credentials-pass "kate.carcia@gmail.com" "Weather1"))

(define fields (isense-project-field "Number" 2 "deg"))

(isense-create-project cred "Test Project" fields)
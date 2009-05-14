#lang scheme/base

(require (planet schematics/sake:1))

(define-task compile
  ()
  (action:compile "levmar-test.ss"))

(define-task test
  (compile)
  (action:test "levmar-test.ss" 'levmar-tests))

(define-task all
  (test compile))

(define-task default
  (all))



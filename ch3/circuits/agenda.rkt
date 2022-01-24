#lang sicp

(#%require "../queue.rkt")

(define (make-time-segment t queue)
  (cons t queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda t) (set-car! agenda t))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let [(q (make-queue))]
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      ; If we're at the current time, append to the segment queue
      (insert-queue! (segment-queue (car segments))
                     action)
      (let [(rest (cdr segments))]
        ; How we maintain order -> insert before rest if time is before the first segment in the
        ; rest of the agenda
        ; < case. Note that belongs-before? returns #t if rest is null
        (if (belongs-before? rest)
          (set-cdr! segments
                    (cons (make-new-time-segment time action)
                          (cdr segments)))
          ; for >= case, recursively call add-to-segments!
          (add-to-segments! rest)))))


  (let [(segments (segments agenda))]
    (if (belongs-before? segments)
      ; Special case where new segment comes before first in agenda
      (set-segments! agenda (cons (make-new-time-segment time action)
                                  segments))
      (add-to-segments! segments))))
  
(define (remove-first-agenda-item! agenda)
  (let [(q (segment-queue (first-segment agenda)))]
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let [(first-seg (first-segment agenda))]
      ; update current-time here
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let [(first-item (first-agenda-item the-agenda))]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(#%provide after-delay propagate current-time the-agenda)
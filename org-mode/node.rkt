#lang racket

(provide
 (struct-out position)
 (struct-out comment)
 (struct-out header)
 (struct-out special)
 (struct-out directive)
 (struct-out plain-list)
 (struct-out property)
 (struct-out properties)
 (struct-out table)
 (struct-out paragraph)
 (struct-out block)
 (struct-out emphasis)
 emphasis-list)

(struct position [start-line start-column start-offset end-line end-column end-offset] #:transparent)


;; cookie is [50%] or [1/3]
(struct header [position level comment priority tags text status cookie (children #:mutable)] #:transparent)

(struct comment [position indentation text] #:transparent)

(struct block [position indentation type lang text] #:transparent)

(struct special [position indentation name text] #:transparent)

(struct directive [position indentation name text] #:transparent)

(struct plain-list [position indentation type text checkbox cookie (children #:mutable)] #:transparent)

(struct property [position name text] #:transparent)

(struct properties [position indentation properties] #:transparent)

(struct table [position indentation rows] #:transparent)

(struct paragraph [position texts] #:transparent)

(define emphasis-list
  '(line-throught
    verbatim
    underline
    bold
    italic
    code))

(struct emphasis [position type text] #:transparent)

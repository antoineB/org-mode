#lang racket

(provide
 (struct-out position)
 (struct-out comment)
 (struct-out header)
 (struct-out special)
 (struct-out keyword)
 (struct-out plain-list)
 (struct-out property)
 (struct-out properties)
 (struct-out table)
 (struct-out paragraph)
 (struct-out block)
 (struct-out markup)
 (struct-out latex)
 (struct-out macro)
 (struct-out date)
 (struct-out snippet)
 (struct-out inline-babel)
 (struct-out inline-block)
 (struct-out link)
 (struct-out line-break)
 (struct-out target)
 (struct-out radio-target)
 (struct-out entity)
 (struct-out super-script)
 (struct-out sub-script)
 (struct-out footnote)
 (struct-out latex-fragment)
 (struct-out dynamic-block)
 (struct-out horizontal-rule)
 (struct-out fixed-width))

(struct position [start-line start-column start-offset end-line end-column end-offset] #:transparent)


;; cookie is [50%] or [1/3]
(struct header [position level comment priority tags text status cookie (children #:mutable)] #:transparent)

(struct comment [position indentation text] #:transparent)

(struct block [position type lang text] #:transparent)

(struct dynamic-block [position name params text] #:transparent)

(struct horizontal-rule [position text] #:transparent)

(struct latex [position name text] #:transparent)

(struct fixed-width [position text] #:transparent)

(struct special [position indentation name text] #:transparent)

(struct keyword [position indentation name text] #:transparent)

(struct plain-list [position indentation type checkbox cookie tag (children #:mutable)] #:transparent)

(struct property [position name text] #:transparent)

(struct properties [position indentation properties] #:transparent)

(struct table [position indentation rows] #:transparent)

(struct paragraph [position texts] #:transparent)

(struct object [])

(struct markup object [border text] #:transparent)

(struct macro object [text] #:transparent)

(struct date object [text] #:transparent)

(struct snippet object [text] #:transparent)

(struct inline-babel object [text] #:transparent)

(struct inline-block object [text] #:transparent)

(struct link object [link description] #:transparent)

(struct line-break object [text] #:transparent)

(struct target object [text] #:transparent)

(struct radio-target object [text] #:transparent)

(struct entity object [text] #:transparent)

(struct sub-script object [name text] #:transparent)

(struct super-script object [name text] #:transparent)

(struct footnote object [label description] #:transparent)

(struct latex-fragment object [text] #:transparent)

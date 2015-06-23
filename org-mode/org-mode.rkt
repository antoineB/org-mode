#lang racket

(require (prefix-in node: "node.rkt"))

(provide read-org write-org)

(module+ test
  (require rackunit))

;; TODO: understand the meaning of \\ at end of line

;; TODO: be case insensitive for #+BEGIN_

;; TODO: treat the following forms:
;; macro {{{stuff}}} http://orgmode.org/org.html#Macro-replacement
;; http://orgmode.org/org.html#Subscripts-and-superscripts
;; footnote http://orgmode.org/org.html#Footnotes
;; links http://orgmode.org/org.html#Hyperlinks
;; date http://orgmode.org/org.html#Dates-and-Times
;; latex snippet

;; TODO: apply emphasis on text
;;
;; They must be preceed and followed by space or newline. But super/sub script
;; should not.  par contre les supper script ne doivent pas être précéder d'un
;; blank
(define line-throught-re #rx"[+](?=[^ ]).*[^ ][+]")
(define verbatim-re #rx"[=](?=[^ ]).*[^ ][=]")
(define underline-re #rx"[_](?=[^ ]).*[^ ][_]")
(define bold-re #rx"[*](?=[^ ]).*[^ ][*]")
(define italic-re #rx"[/](?=[^ ]).*[^ ][/]")
(define code-re #rx"[~](?=[^ ]).*[^ ][~]")


(define (decode-block indentation type lang in position)
  (define (read-until-line re in [result ""])
    (define line (read-line in))
    (cond [(eq? eof line) eof]
          [(regexp-match? re line) result]
          [else (read-until-line re in (string-append result line))]))
  (define text (read-until-line (string-append "^ *#[+]END_" type " *$") in))
  (when (eq? eof text)
    (raise-user-error "Impossible to find end for ~a started at ~a:~a"
                      (string-append "#+BEGIN_" type)
                      (node:position-start-line position)
                      (node:position-start-column position)))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (node:block
   (struct-copy node:position position
                [end-line end-line]
                [end-column end-column]
                [end-offset end-offset])
   indentation
   type
   lang
   text))

(define current-header-status (make-parameter '("TODO" "DONE")))

(define current-date-format
  (make-parameter
   #px"^[0-9]{4}(-[0-9]{2}){2}( ((Sat|Sun|Mon|Tue|Thu|Fri)? )?([0-9]{2})?(:[0-9]{2}){,2})?"))

(module+ test
  (check-true (regexp-match? (current-date-format) "2015-06-06 Sat 00:00:00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 Sat 00:00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 Sat 00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 Sat"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 00:00:00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 00:00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06 00"))
  (check-true (regexp-match? (current-date-format) "2015-06-06")))


(define (decode-header line in position)
  (define line-in (open-input-string line))
  (define level (string-length (bytes->string/utf-8 (second (regexp-match #rx"^([*]+) " line-in)))))
  (define comment (and (regexp-match-peek #rx"^COMMENT " line-in)
                       (regexp-match #rx"^COMMENT " line-in)))
  (define status
    (let ([re (regexp (string-join (current-header-status) "|"
                                   #:before-first "^("
                                   #:after-last ") "))])
      (and (regexp-match-peek re line-in) (second (regexp-match re line-in)))))
  (define-values (text cookie priority tags)
    (let* ([text (port->string line-in)]
           [cookie (extract-cookie text)]
           [text (if (string? cookie) cookie (car cookie))]
           [cookie (and (pair? cookie) (cdr cookie))]
           [priority (extract-priority text)]
           [text (if (string? priority) priority (car priority))]
           [priority (and (pair? priority) (cdr priority))]
           [tags (extract-tags text)]
           [text (if (string? tags) tags (car tags))]
           [tags (and (pair? tags) (cdr tags))])
      (close-input-port line-in)
      (values
       text
       cookie
       priority
       tags)))
  (node:header position level comment priority tags text status cookie '()))

(define (extract-priority text)
  (define pos (regexp-match-positions #rx"\\[#[A-Z]\\]" text))
  (if pos
      (cons
       (string-append
        (substring text 0 (caar pos))
        (substring text (cdar pos)))
       (substring text (+ 2 (caar pos)) (sub1 (cdar pos))))
      text))

(define (extract-cookie text)
  (define pos (regexp-match-positions #px"\\[([0-9]{,3}%|[0-9]+/[0-9]+)\\]" text))
  (if pos
      (cons
       (string-append
        (substring text 0 (caar pos))
        (substring text (cdar pos)))
       (substring text
                  ((if (cadr pos) caadr caaddr) pos)
                  ((if (cadr pos) cdadr cdaddr) pos)))
      text))

(define (extract-tags text)
  (define pos (regexp-match-positions #rx":([^ ]+):$" text))
  (if pos
      (cons
       (string-append
        (substring text 0 (caar pos))
        (substring text (cdar pos)))
       (string-split (substring text (caadr pos) (cdadr pos)) ":"))
      text))

(module+ test
  (let ([data0 (open-input-string "* COMMENT TODO [#A] some text :house:garden:")]
        [data1 (open-input-string "* DONE some text [50%] :house:garden:")]
        [data2 (open-input-string "** [1/2] some text [50%]")]
        [data3 (open-input-string "* DONE text :text:  :tag:")])
    ;; TODO: use the decode header method
    (check-true (node:header? (decode-next-section data0)))
    (check-true (node:header? (decode-next-section data1)))
    (check-true (node:header? (decode-next-section data2)))
    (check-true (node:header? (decode-next-section data3)))))

(define (decode-properties indentation in position)
  (define (loop in result start-line start-colum start-offset)
    (define line (read-line in))
    (define-values (end-line end-column end-offset) (port-next-location in))
    (match line
      [(regexp "^[ ]*$") (loop in result end-line end-column end-offset)]
      [(regexp "^[ ]*:([^:]+):(.*)$" (list _ name text))
       (define new-position (node:position start-line start-colum start-offset end-line end-column end-offset))
       (loop
        in
        (cons
         (node:property
          new-position
          name
          (string-trim text))
         result)
        end-line end-column end-offset)]
      [(regexp "^[ ]*:END:[ ]*$")
       (node:properties
        (struct-copy node:position position
                     [end-line end-line]
                     [end-column end-column]
                     [end-offset end-offset])
        indentation
        (reverse result))]
      [else
       (raise-user-error "Impossible to find end for :PROPERTIES: started at ~a:~a"
                         (node:position-start-line position)
                         (node:position-start-column position))]))
  (loop in
        empty
        (node:position-end-line position)
        (node:position-end-column position)
        (node:position-end-offset position)))

(define (drop-last lst)
  (reverse (rest (reverse lst))))

(define (decode-table line in position)
 (define indentation (first (regexp-match #rx"^[ ]*" line)))
 (define (read-line-starting-with in re) (and (regexp-match-peek re in) (read-line in)))
 (let loop ([line line]
            [result empty])
   (if line
       (loop
        (read-line-starting-with in (regexp (string-append indentation "\\|")))
        (cons
         (cond [(regexp-match? #rx"^ *\\|(-+[+]?)+\\|?$" line)
                (regexp-match* #rx"(-+)" line)]
               [(regexp-match* #rx"\\|([^|]*)" line) =>
                (lambda (columns)
                  (map
                   (lambda (str) (substring str 1))
                   (if (equal? "|" (last columns)) (drop-last columns) columns)))])
         result))
       (node:table
        (let-values ([(end-line end-column end-offset) (port-next-location in)])
          (struct-copy node:position position
                       [end-line end-line]
                       [end-column end-column]
                       [end-offset end-offset]))
        (string-length indentation)
        (reverse result)))))

(define (decode-next-section in)
  (define specials-re
    (let ([specials '("ID"
                      "TODO"
                      "TAGS"
                      "ALLTAGS"
                      "CATEGORY"
                      "PRIORITY"
                      "DEADLINE"
                      "SCHEDULED"
                      "CLOSED"
                      "TIMESTAMP"
                      "TIMESTAMP_IA"
                      "CLOCKSUM"
                      "CLOCKSUM_T"
                      "BLOCKED"
                      "ITEM"
                      "FILE")])
      (string-append "^([ ]*)(" (string-join specials "|") "):([.]*)")))
  (define-values (start-line start-column start-offset) (port-next-location in))
  (define line (read-line in))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (define position (node:position start-line start-column start-offset
                                  end-line end-column end-offset))
  ;; contain all the begining type of line of org-mode
  (match line
    [(regexp "^[*]+ ") (decode-header line in position)]
    [(regexp "^[ ]*[|]") (decode-table line in position)]
    [(regexp "^( *)#+BEGIN_([A-Z]+)[ ]?(.*)" (list _ indentation type lang))
     (decode-block (string->number indentation) type lang in position)]
    [(regexp "^( *):PROPERTIES:" (list _ indentation))
     (decode-properties (string-length indentation) in position)]
    [(regexp "^( *)#[+]([A-Z]+):(.*)" (list _ indentation name text))
     (node:directive position (string-length indentation) name (string-trim text))]
    ;; [(regexp "^[ ]*#+END_[A-Z]+") 'block-end]
    ;; [(regexp "^[ ]*:END:") 'properties-end]
    ;; [(regexp "^[ ]*:[a-zA-Z_0-9-]:") 'property]
    [(regexp "^( *)(-|[+]|[0-9]+[.]) (\\[[ X-]\\])?(.*)" (list _ indentation type checkbox text))
     (node:plain-list position
                    (string-length indentation)
                    (case type
                      [("+") 'plus]
                      [("-") 'minus]
                      [else (string->number (first (regexp-match "[0-9]+" type)))])
                    (string-trim text)
                    checkbox
                    #f
                    empty)]
    [(regexp "^( +)[*] (\\[[ X-]\\])?(.*)" (list _ indentation checkbox text))
     (node:plain-list position
                    (string-length indentation)
                    'bullet
                    (string-trim text)
                    checkbox
                    #f
                    empty)]
    [(regexp specials-re (list _ indentation name text))
     (node:special position (string-length indentation) (string-length indentation) name (string-trim text))]
    [(regexp "^( *)#(.*)" (list _ indentation text))
     (node:comment position (string-length indentation) (string-trim text))]
    [(? (curry equal? eof)) eof]
    [else (cons line position)]))

(define (end-paragraph stack)
  (match stack
    [(list-rest texts 'paragraph rst)
     (stack-add
      rst
      (node:paragraph
       (let ([start (cdr (last texts))]
             [end (cdr (first texts))])
         (node:position (node:position-start-line start)
                        (node:position-start-column start)
                        (node:position-start-offset start)
                        (node:position-end-line end)
                        (node:position-end-column end)
                        (node:position-end-offset end)))
       (reverse (map car texts))))]
    [else stack]))

(define (end-indentation stack indentation)
  (match stack
    [(list-rest children (? node:plain-list? plain) rst)
     (if (>= (node:plain-list-indentation plain) indentation)
         (begin
           (node:set-plain-list-children! plain (reverse children))
           (end-indentation (stack-add rst plain) indentation))
         stack)]
    [else stack]))

(define (end-header stack level)
  (match stack
    [(list-rest texts 'paragraph rst)
     (end-header (end-paragraph stack) level)]
    [(list-rest children (? node:plain-list? plain) rst)
     (node:set-plain-list-children! plain (reverse children))
     (end-header (stack-add rst plain) level)]
    [(list-rest children (? node:header? header) rst)
     (if (>= (node:header-level header) level)
         (begin
           (node:set-header-children! header (reverse children))
           (end-header (stack-add rst header) level))
         stack)]
    [else stack]))

(define (end-all stack)
  (match stack
    [(list elems) elems]
    [(list-rest children (? node:plain-list? plain) rst)
     (node:set-plain-list-children! plain (reverse children))
     (end-all (stack-add rst plain))]
    [(list-rest children (? node:header? header) rst)
     (node:set-header-children! header (reverse children))
     (end-all (stack-add rst header))]
    [(list-rest texts 'paragraph rst)
     (end-all (end-paragraph stack))]))


(define (stack-add stack data)
  (cons
   (cons data (first stack))
   (rest stack)))

(define (paragraph-add stack text)
  (match stack
    [(list-rest texts 'paragraph rst)
     (cons
      (cons text texts)
      (cons 'paragraph rst))]
    [else
     (cons
      (list text)
      (cons 'paragraph stack))]))

;; How its work:
;;
;; Go line after line and for each find it's type. Do more find grainned parsing
;; on the line, and add it to a stack. If the line mark the end of previously
;; stacked data for example by have a lower indentation level or being from
;; another type, unwind the stack to a data we are in.
(define (read-org in)
  (let loop ([stack '(())])
    (match (decode-next-section in)
      [(? node:comment? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:comment-indentation block)) block))]
      [(? node:table? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:table-indentation block)) block))]
      [(? node:block? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:block-indentation block)) block))]
      [(? node:properties? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:properties-indentation block)) block))]
      [(? node:directive? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:directive-indentation block)) block))]
      [(? node:special? block)
       (loop (stack-add (end-indentation (end-paragraph stack) (node:special-indentation block)) block))]
      [(? node:header? header)
       (loop
        (let* ([stack (end-paragraph stack)]
               [stack (end-indentation stack 0)]
               [stack (end-header stack (node:header-level header))])
          (cons empty (cons header stack))))]
      [(? node:plain-list? plain)
       (let* ([stack (end-paragraph stack)]
              [stack (end-indentation stack (node:plain-list-indentation plain))])
         (loop
          (cons empty (cons plain stack))))]
      [(? (curry equal? eof))
       (values
        (reverse
         (end-all stack)))]
      [(? pair? text)
       (loop
        (let ([indentation (string-length (first (regexp-match #rx"^ *" (car text))))])
          (match stack
            [(list-rest _ 'paragraph _ (? node:plain-list? plain) rst)
             (if (> (node:plain-list-indentation plain) indentation)
                 (paragraph-add (end-indentation (end-paragraph stack)) text)
                 (paragraph-add stack text))]
            [(list-rest children (? node:plain-list? plain) rst)
             (paragraph-add (end-indentation stack indentation) text)]
            [else
             (paragraph-add stack text)])))])))


(define (write-org org-struct out extra-indentation)
  (define (unless-empty test data)
    (write
     (if test (string-append data " ") "")
     out))
  (match org-struct
    [(struct node:header [position level comment priority tags text status cookie children])
     (display (string-append (make-string level #\*) " ") out)
     (unless comment "COMMENT ")
     (unless status (string-append status " "))
     ;;(if priority
     (display text out)
     ;; cookie
     ;; tags
     (display #\newline out)
     (for ([child children])
       (write-org child out 0))]
    [(struct node:comment [position indentation text])
     (display (make-string indentation #\space) out)
     (display "#" out)
     (display text out)
     (display #\newline out)]
    [(struct node:block [position indentation type lang text])
     (let ([space (make-string (+ extra-indentation indentation) #\space)])
       (display space out)
       (display (format "#+BEGIN_~a ~a" type lang) out)
       (display #\newline out)
       (display text out)
       (display #\newline out)
       (display space out)
       (display (format "#+END_~a" type) out)
       (display #\newline out))]
    [(struct node:special [position indentation name text])
     (display (make-string (+ extra-indentation indentation) #\space) out)
     (display name out)
     (display ": " out)
     (display text out)
     (display #\newline out)]
    [(struct node:directive [position indentation name text])
     (display (make-string indentation #\space) out)
     (display "#+" out)
     (display name out)
     (display ": " out)
     (display text out)
     (display #\newline out)]
    [(struct node:properties [position indentation properties])
     (let ([space (make-string (+ extra-indentation indentation) #\space)])
       (display space out)
       (display ":PROPERTIES:" out)
       (display #\newline out)
       (for ([prop properties])
         (display (format ":~a: ~a"
                        (node:property-name prop)
                        (node:property-text prop))
                out)
         (display #\newline out))
       (display space out)
       (display ":END:" out)
       (display #\newline out))]
    [(struct node:paragraph [position texts])
     (for ([text texts])
       (display text out)
       (display #\newline out))]
    [(struct node:plain-list [position indentation type text checkbox cookie children])
     (let ([space (make-string indentation #\space)])
       (display space out)
       (display
        (case type
          [(bullet) "* "]
          [(plus) "+ "]
          [(minus) "- "]
          [else (string-append (number->string type) ". ")])
        out)
       ;; checkbox
       ;; cookie
       (display text out)
       (display #\newline out)
       (for ([child children])
         (write-org child out indentation)
         #;(display #\newline out)))]
    [(struct node:table [position indentation rows])
     (let ([space (make-string (+ extra-indentation indentation) #\space)])
       (for ([row rows])
         (display space out)
         (display "|" out)
         (for ([column row])
           (display column out)
           (display "|" out))
         (display #\newline out)))]))

#lang racket

(require org-mode
         (only-in xml xexpr?))

(define (xexpr-list? elem)
  (and (list? elem)
       (not (xexpr? elem))))

(define (to-xexpr-elem elem)
  (match elem
    [(struct org-comment [_ _ text])
     `(!HTML-COMMENT () ,text)]
    [(struct org-header [_ level #f _ _ text _ _ children])
     `((,(string->symbol (string-append "h" (number->string level))) () ,text)
       ,@(to-xexpr-list children))]
    [(struct org-paragraph [_ texts])
     `(p ()
         ,@(for*/list ([text texts]
                        [part text])
              (if (string? part)
                  part
                  `(,(case (org-emphasis-type part)
                       [(line-throught) 'emp]
                       [(verbatim) 'pre]
                       [(underline) 'u]
                       [(bold) 'b]
                       [(italic) 'i]
                       [(code) 'code])
                    ()
                    ,(org-emphasis-text part)))))]
    [(struct org-block [_ _ (regexp "(?i:HTML)") _ text])
     (raise "Need to be done, block HTML")]
    [(struct org-block [_ _ (regexp "(?i:QUOTE)") _ text])
     `(blockquote () ,text)]
    [(struct org-block [_ _ (regexp "(?i:SRC)") lang text])
     `(pre ((class ,(string-append "brush: " lang))) (code () ,text))]
    [(struct org-block [_ _ (regexp "(?i:CENTER)") _ text])
     `(p ((style "text-align:center")) ,text)]
    [(struct org-directive [_ _ (regexp "(?i:HTML)") text])
     (raise "Need to be done, block HTML")]
    [(struct org-directive [_ _ (regexp "(?i:QUOTE)") text])
     `(q () ,text)]
    [(struct org-plain-list-group [children])
     `(ol ()
          ,@(map to-xexpr-elem children))]
    [(struct org-plain-list [_ _ _ text _ _ '()])
     `(li () ,text)]
    [(struct org-plain-list [_ _ _ text _ _ children])
     (if (org-paragraph? (first children))
         `(li ()
              ,(to-xexpr-elem
                (struct-copy org-paragraph
                             (first children)
                             [texts (cons text (org-paragraph-texts (first children)))]))
              ,@(to-xexpr-list (rest children)))
         `(li () ,text ,@(to-xexpr-list children)))]
    ;; other form should not be converted to xexpr
    [(or (? org-block?)
         (? org-header?)
         (? org-directive?)
         (? org-special?)
         (? org-properties?))
     #f]
    ;; (struct table [position indentation rows] #:transparent)
    [else
     (printf "~a\n" elem)
     (raise-user-error "Not a valid element")]))

(struct org-plain-list-group [children] #:transparent)

;; All consecutive list are grouped into org-plain-list-group, this behavior
;; should be put into the parser.
(define (to-xexpr-list lst)
  (define grouped-list
    (let loop ([lst lst]
               [plain-list empty]
               [result empty])
      (cond [(empty? lst)
             (reverse
              (if (empty? plain-list)
                  result
                  (cons (org-plain-list-group (reverse plain-list)) result)))]
            [(org-plain-list? (first lst))
             (loop (rest lst) (cons (first lst) plain-list) result)]
            [(not (empty? plain-list))
             (loop (rest lst)
                   empty
                   (cons (org-plain-list-group (reverse plain-list)) result))]
            [else
             (loop (rest lst) empty (cons (first lst) result))])))
  (let loop ([lst grouped-list]
             [result empty])
    (if (empty? lst)
        (reverse result)
        (let ([data (to-xexpr-elem (first lst))])
          (loop (rest lst)
                (cond [(and (list? data)
                            (not (empty? data))
                            (list? (first data)))
                       (append (reverse data) result)]
                      [(false? data)
                       result]
                      [else
                       (cons data result)]))))))

(provide to-xexpr-list)

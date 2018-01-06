#lang racket/base

(require (prefix-in node: "node.rkt"))

(provide read-org write-org)

(require racket/match
         racket/string
         racket/list
         racket/set
         racket/port)

(module+ test
  (require rackunit))

;; draft format: http://orgmode.org/worg/dev/org-syntax.html

;; TODO: Objects can only be found in the following locations:
;;
;;    affiliated keywords defined in org-element-parsed-keywords,
;;    document properties,
;;    verse blocks.

;; TODO: support for directive (affiliated keyword) seems errorneous

;; TODO: http://orgmode.org/worg/dev/org-syntax.html#Clock,_Diary_Sexp_and_Planning

(define current-extractors (make-parameter (set)))

(define (extract-object src)
  (parameterize ([current-extractors
                  (set
                   extract-super/sub-script*
                   extract-target*
                   extract-radio-target*
                   extract-footnote*
                   extract-entity*
                   extract-latex-fragment*
                   extract-markup*
                   extract-angular-link*
                   extract-link*
                   extract-space-break*
                   extract-inline-babel*
                   extract-inline-block*
                   extract-snippet*
                   extract-macro*
                   extract-date*)])
    (extract-object-sub* src)))

(define (extract-object-sub* src)
  ;; The order in witch the extractor are apply is important
  (define extractors (current-extractors))
  (define (apply-extractor texts extractor)
    (if (set-member? extractors extractor)
        (flatten (map (lambda (str) (if (string? str) (extractor str) str)) texts))
        texts))
  (let* ([texts (list src)]
         [texts (apply-extractor texts extract-macro*)]
         [texts (apply-extractor texts extract-snippet*)]
         [texts (apply-extractor texts extract-radio-target*)]
         [texts (apply-extractor texts extract-target*)]
         [texts (apply-extractor texts extract-date*)]
         [texts (apply-extractor texts extract-angular-link*)]
         [texts (apply-extractor texts extract-link*)]
         [texts (apply-extractor texts extract-footnote*)]
         [texts (apply-extractor texts extract-latex-fragment*)]
         [texts (apply-extractor texts extract-entity*)]
         [texts (apply-extractor texts extract-inline-babel*)]
         [texts (apply-extractor texts extract-inline-block*)]
         [texts (apply-extractor texts extract-markup*)]
         [texts (apply-extractor texts extract-super/sub-script*)]
         [texts (apply-extractor texts extract-space-break*)])
    texts))


;; http://orgmode.org/org.html#Subscripts-and-superscripts
(define (extract-super/sub-script* str)
  (define (inner str [before ""])
    (match (regexp-match-positions #rx"[^ \f\r\n\t^]+[\\^_]" str)
      [(list (cons start end))
       (define new-str (substring str end))
       (define new-positions (regexp-match-positions #rx"^([*]|([+-]?[a-zA-Z0-9,\\.]*[a-zA-Z0-9]))" new-str))
       (cond
         [new-positions (values (string-append before (substring str 0 start))
                                (substring str start (+ end (cdar new-positions)))
                                (substring str (+ end (cdar new-positions))))]
         [(equal? new-str "") (values #f #f (string-append before str))]
         [(or (char=? (string-ref new-str 0) #\()
              (char=? (string-ref new-str 0) #\{))
          (define new-end (consume-balanced
                           new-str
                           (if (char=? (string-ref new-str 0) #\{)
                               (hash #\{ #\} #\} 'closer)
                               (hash #\( #\) #\) 'closer))
                           (lambda (char opened)
                             (match opened
                               [(list (cons #\( _)) (char=? char #\))]
                               [(list (cons #\{ _)) (char=? char #\})]
                               [else #f]))))
          (if (= 0 new-end)
              (inner new-str (string-append before (substring str 0 end)))
              (values (string-append before (substring str 0 start))
                      (substring str start (+ end new-end 1))
                      (substring str (+ end new-end 1))))]
         [else (inner new-str (string-append before (substring str 0 end)))])]
      [else (values #f #f (string-append before str))]))

  (define (extract-data* str)
    (parameterize ([current-extractors
                    (set-intersect
                     (current-extractors)
                     (set
                      extract-footnote*
                      extract-super/sub-script*
                      extract-entity*
                      extract-latex-fragment*
                      extract-markup*))])
      (extract-object-sub* str)))

  (let loop ([str str]
             [result '()])
    (define-values (before script after) (inner str))
    (if (not before)
        (reverse (cons str result))
        (let ([script (match (regexp-match #rx"^([^^_]+)([_^])([{(]?(?:.+?)[})])?$" script)
                        [(list _ name "^" data)
                         (node:super-script name (extract-data* data))]
                        [(list _ name "_" data)
                         (node:sub-script name (extract-data* data))])])
          (match* (before after)
            [("" "") (reverse (cons script result))]
            [(before "") (reverse (list* script before result))]
            [("" after) (loop after (cons script result))]
            [(before after) (loop after (list* script before result))])))))



(define (consume-balanced str opener-closer end?)
  (define in (open-input-string str))
  ;; opened is a Listof (Pairof Char Integer;#|position|#)
  (begin0
      (let loop ([opened (list)]
                 [position 0])
        (define c (read-char in))
        (cond
          [(eof-object? c) 0]
          [(end? c opened) position]
          [(equal? 'closer (hash-ref opener-closer c #f))
           (if (and (not (empty? opened))
                    (equal? c (hash-ref opener-closer (caar opened) #f)))
               (loop (rest opened) (add1 position))
               (cdr (last opened)))]
          [(hash-ref opener-closer c #f) (loop (cons (cons c position) opened) (add1 position))]
          [else
           (loop opened (add1 position))]))
    (close-input-port in)))

(module+ test
  (let ([consume-fn (lambda (str)
                      (consume-balanced str
                                        (hash #\( #\) #\) 'closer)
                                        (lambda (char opened)
                                          (match opened
                                            [(list (cons #\( _)) (and (char=? char #\)))]
                                            [(list (cons #\{ _)) (and (char=? char #\}))]
                                            [else #f]))))])
    (check-equal? (consume-fn "(t(ot)o)") 7)
    (check-equal? (consume-fn "(t(ot)o") 0)))

;; http://orgmode.org/worg/dev/org-syntax.html#Targets_and_Radio_Targets
(define (extract-target* str)
  (define positions (regexp-match-positions* #rx"(?<!\\s)(<<[^\r\n<>]+>>)(?!\\s)" str #:match-select cadr))
  (extract-simple* node:target positions str))

(define current-entities (make-parameter '("Agrave"
                                           "agrave"
                                           "Aacute"
                                           "aacute"
                                           "Acirc"
                                           "acirc"
                                           "Atilde"
                                           "atilde"
                                           "Auml"
                                           "auml"
                                           "Aring"
                                           "AA"
                                           "aring"
                                           "AElig"
                                           "aelig"
                                           "Ccedil"
                                           "ccedil"
                                           "Egrave"
                                           "egrave"
                                           "Eacute"
                                           "eacute"
                                           "Ecirc"
                                           "ecirc"
                                           "Euml"
                                           "euml"
                                           "Igrave"
                                           "igrave"
                                           "Iacute"
                                           "iacute"
                                           "Icirc"
                                           "icirc"
                                           "Iuml"
                                           "iuml"
                                           "Ntilde"
                                           "ntilde"
                                           "Ograve"
                                           "ograve"
                                           "Oacute"
                                           "oacute"
                                           "Ocirc"
                                           "ocirc"
                                           "Otilde"
                                           "otilde"
                                           "Ouml"
                                           "ouml"
                                           "Oslash"
                                           "oslash"
                                           "OElig"
                                           "oelig"
                                           "Scaron"
                                           "scaron"
                                           "szlig"
                                           "Ugrave"
                                           "ugrave"
                                           "Uacute"
                                           "uacute"
                                           "Ucirc"
                                           "ucirc"
                                           "Uuml"
                                           "uuml"
                                           "Yacute"
                                           "yacute"
                                           "Yuml"
                                           "yuml"
                                           "fnof"
                                           "real"
                                           "image"
                                           "weierp"
                                           "ell"
                                           "imath"
                                           "jmath"
                                           "Alpha"
                                           "alpha"
                                           "Beta"
                                           "beta"
                                           "Gamma"
                                           "gamma"
                                           "Delta"
                                           "delta"
                                           "Epsilon"
                                           "epsilon"
                                           "varepsilon"
                                           "Zeta"
                                           "zeta"
                                           "Eta"
                                           "eta"
                                           "Theta"
                                           "theta"
                                           "thetasym"
                                           "vartheta"
                                           "Iota"
                                           "iota"
                                           "Kappa"
                                           "kappa"
                                           "Lambda"
                                           "lambda"
                                           "Mu"
                                           "mu"
                                           "nu"
                                           "Nu"
                                           "Xi"
                                           "xi"
                                           "Omicron"
                                           "omicron"
                                           "Pi"
                                           "pi"
                                           "Rho"
                                           "rho"
                                           "Sigma"
                                           "sigma"
                                           "sigmaf"
                                           "varsigma"
                                           "Tau"
                                           "Upsilon"
                                           "upsih"
                                           "upsilon"
                                           "Phi"
                                           "phi"
                                           "varphi"
                                           "Chi"
                                           "chi"
                                           "acutex"
                                           "Psi"
                                           "psi"
                                           "tau"
                                           "Omega"
                                           "omega"
                                           "piv"
                                           "varpi"
                                           "partial"
                                           "alefsym"
                                           "aleph"
                                           "gimel"
                                           "beth"
                                           "dalet"
                                           "ETH"
                                           "eth"
                                           "THORN"
                                           "thorn"
                                           "dots"
                                           "cdots"
                                           "hellip"
                                           "middot"
                                           "iexcl"
                                           "iquest"
                                           "shy"
                                           "ndash"
                                           "mdash"
                                           "quot"
                                           "acute"
                                           "ldquo"
                                           "rdquo"
                                           "bdquo"
                                           "lsquo"
                                           "rsquo"
                                           "sbquo"
                                           "laquo"
                                           "raquo"
                                           "lsaquo"
                                           "rsaquo"
                                           "circ"
                                           "vert"
                                           "brvbar"
                                           "S"
                                           "sect"
                                           "amp"
                                           "lt"
                                           "gt"
                                           "tilde"
                                           "slash"
                                           "plus"
                                           "under"
                                           "equal"
                                           "asciicirc"
                                           "dagger"
                                           "dag"
                                           "Dagger"
                                           "ddag"
                                           "nbsp"
                                           "ensp"
                                           "emsp"
                                           "thinsp"
                                           "curren"
                                           "cent"
                                           "pound"
                                           "yen"
                                           "euro"
                                           "EUR"
                                           "EURdig"
                                           "EURhv"
                                           "EURcr"
                                           "EURtm"
                                           "copy"
                                           "reg"
                                           "trade"
                                           "minus"
                                           "pm"
                                           "plusmn"
                                           "times"
                                           "frasl"
                                           "colon"
                                           "div"
                                           "frac12"
                                           "frac14"
                                           "frac34"
                                           "permil"
                                           "sup1"
                                           "sup2"
                                           "sup3"
                                           "radic"
                                           "sum"
                                           "prod"
                                           "micro"
                                           "macr"
                                           "deg"
                                           "prime"
                                           "Prime"
                                           "infin"
                                           "infty"
                                           "prop"
                                           "propto"
                                           "not"
                                           "neg"
                                           "land"
                                           "wedge"
                                           "lor"
                                           "vee"
                                           "cap"
                                           "cup"
                                           "int"
                                           "therefore"
                                           "there4"
                                           "because"
                                           "sim"
                                           "cong"
                                           "simeq"
                                           "asymp"
                                           "approx"
                                           "ne"
                                           "neq"
                                           "equiv"
                                           "triangleq"
                                           "le"
                                           "leq"
                                           "ge"
                                           "geq"
                                           "lessgtr"
                                           "lesseqgtr"
                                           "ll"
                                           "Ll"
                                           "lll"
                                           "gg"
                                           "Gg"
                                           "ggg"
                                           "prec"
                                           "preceq"
                                           "preccurlyeq"
                                           "succ"
                                           "succeq"
                                           "succcurlyeq"
                                           "sub"
                                           "subset"
                                           "sup"
                                           "supset"
                                           "nsub"
                                           "sube"
                                           "nsup"
                                           "supe"
                                           "setminus"
                                           "forall"
                                           "exist"
                                           "exists"
                                           "nexist"
                                           "nexists"
                                           "empty"
                                           "emptyset"
                                           "isin"
                                           "in"
                                           "notin"
                                           "ni"
                                           "nabla"
                                           "ang"
                                           "angle"
                                           "perp"
                                           "sdot"
                                           "cdot"
                                           "lceil"
                                           "rceil"
                                           "lfloor"
                                           "rfloor"
                                           "lang"
                                           "rang"
                                           "hbar"
                                           "mho"
                                           "larr"
                                           "leftarrow"
                                           "gets"
                                           "lArr"
                                           "Leftarrow"
                                           "uarr"
                                           "uparrow"
                                           "uArr"
                                           "Uparrow"
                                           "rarr"
                                           "to"
                                           "rightarrow"
                                           "rArr"
                                           "Rightarrow"
                                           "darr"
                                           "downarrow"
                                           "dArr"
                                           "Downarrow"
                                           "harr"
                                           "leftrightarrow"
                                           "hArr"
                                           "Leftrightarrow"
                                           "crarr"
                                           "hookleftarrow"
                                           "arccos"
                                           "arcsin"
                                           "arctan"
                                           "arg"
                                           "cos"
                                           "cosh"
                                           "cot"
                                           "coth"
                                           "csc"
                                           "deg"
                                           "det"
                                           "dim"
                                           "exp"
                                           "gcd"
                                           "hom"
                                           "inf"
                                           "ker"
                                           "lg"
                                           "lim"
                                           "liminf"
                                           "limsup"
                                           "ln"
                                           "log"
                                           "max"
                                           "min"
                                           "Pr"
                                           "sec"
                                           "sin"
                                           "sinh"
                                           "sup"
                                           "tan"
                                           "tanh"
                                           "bull"
                                           "bullet"
                                           "star"
                                           "lowast"
                                           "ast"
                                           "odot"
                                           "oplus"
                                           "otimes"
                                           "check"
                                           "checkmark"
                                           "para"
                                           "ordf"
                                           "ordm"
                                           "cedil"
                                           "oline"
                                           "uml"
                                           "zwnj"
                                           "zwj"
                                           "lrm"
                                           "rlm"
                                           "smile"
                                           "frown"
                                           "smiley"
                                           "blacksmile"
                                           "sad"
                                           "clubs"
                                           "clubsuit"
                                           "spades"
                                           "spadesuit"
                                           "hearts"
                                           "heartsuit"
                                           "diams"
                                           "diamondsuit"
                                           "diamond"
                                           "Diamond"
                                           "loz")))

;; http://orgmode.org/worg/dev/org-syntax.html#Entities_and_LaTeX_Fragments
(define (extract-entity* str)
  (define positions (regexp-match-positions*
                     (pregexp (string-append "(\\\\(" (string-join (current-entities) "|") ")(?:\\{\\})?)(?:\\p{^L}|\\p{^N})"))
                     str
                     #:match-select cadr))
  (extract-simple* node:entity positions str))

;; http://orgmode.org/worg/dev/org-syntax.html#Targets_and_Radio_Targets
(define (extract-radio-target* str)
  (define positions (regexp-match-positions* #rx"(?<!\\s)(<<<[^\r\n<>]+>>>)(?!\\s)" str #:match-select cadr))
  (define (extract str)
    (define data (regexp-match #rx"^<<<(.+?)>>>$" str))
    (parameterize ([current-extractors
                    (set-intersect
                     (current-extractors)
                     (set
                      extract-super/sub-script*
                      extract-entity*
                      extract-latex-fragment*
                      extract-markup*))])
      (node:radio-target
       (extract-object-sub* (second data)))))
  (extract-simple* extract positions str))

;; http://orgmode.org/org.html#Footnotes
(define (extract-footnote* str)
  (define (inner str [before ""])
    (match (regexp-match-positions #px"\\[fn:(\\p{N}|\\p{L})*:" str)
      [(list (cons start end) _)
       (define new-str (substring str end))
       (define new-end (consume-balanced
                        new-str
                        (hash #\[ #\] #\] 'closer)
                        (lambda (char opened)
                          (and (empty? opened) (char=? char #\])))))
       (if (= 0 new-end)
           (inner new-str (string-append before (substring str 0 end)))
           (values (string-append before (substring str 0 start))
                   (substring str start (+ end new-end 1))
                   (substring str (+ end new-end 1))))]
      [else (values #f #f (string-append before str))]))

  (define (extract-description* str)
    (parameterize ([current-extractors
                    (set-intersect
                     (current-extractors)
                     (set
                      extract-footnote*
                      extract-super/sub-script*
                      extract-entity*
                      extract-latex-fragment*
                      extract-markup*))])
      (extract-object-sub* str)))

  (let loop ([str str]
             [result '()])
    (define-values (before footnote after) (inner str))
    (if (not before)
        (reverse (cons str result))
        (let* ([data (regexp-match #px"^\\[fn:((\\p{N}|\\p{L})*):(.+)\\]$" footnote)]
               [foot (node:footnote (second data) (extract-description* (fourth data)))])
          (match* (before after)
            [("" "") (reverse (cons foot result))]
            [(before "") (reverse (list* foot before result))]
            [("" after) (loop after (cons foot result))]
            [(before after) (loop after (list* foot before result))])))))


(module+ test
  (check-match (extract-footnote* "some [fn::note] working")
               (list "some " (node:footnote "" '("note")) " working"))
  (check-match (extract-footnote* "some [fn::  bad [fn::note] working")
               (list "some [fn::  bad " (node:footnote "" '("note")) " working")))

;; http://orgmode.org/worg/dev/org-syntax.html#Entities_and_LaTeX_Fragments
(define (extract-latex-fragment* str)
  (define re
    (string-join
     (list
      (string-append "(\\\\(?:" (string-join (current-entities) "|") ")(?:(?:\\\\[([^][{}\n]+\\\\])|(?:\\{[^\n{}]+\\})))")
                     "([\\][(][^)]+[\\][)])"
                     "([\\]\\[[^]]+[\\]\\])"
                     "(\\$\\$(?:(?:\\$[^$])|[^$])+?\\$\\$)"
                     "(?:^|[^$])([$][^.,?;'\"][$])(?:$|[^$])"
                     "(?:^|[^$])([$][^ \n\t\f\r.,;$][^$\n]*?(?:\n[^$\n]*)?(?:\n[^$\n]*)?(?:\n[^$\n]*)?[^ \n\t\f\r.,$][$])(?:$|[^$])")
      "|"))
    (define positions (regexp-match-positions* re str #:match-select (lambda (x) (first (filter values (rest x))))))
    (extract-simple* node:latex-fragment positions str))

;; TODO: Allow to choose which char is markup border.
;; https://orgmode.org/worg/dev/org-syntax.html#Emphasis_Markers
(define (extract-markup* str)
  (define re #px"([ \t('\"{]|^)(([+_*/=~])([^ \t\r\n,\"']|[^ \t\r\n,\"'].*?(?:\n.*?){0,3}[^ \t\r\n,\"'])\\3)([- \t.,:!?;'\")}]|$)")
  (define positions (regexp-match-positions* re str #:match-select caddr))
  (extract-simple*
   (lambda (str)
     (define parts (regexp-match re str))
     (define sub (substring (third parts) 1 (sub1 (string-length (third parts)))))
     (node:markup (fourth parts)
                  (case (fourth parts)
                    [("~" "=") sub]
                    [else
                     (parameterize ([current-extractors
                                     (set-intersect
                                      (current-extractors)
                                      (set
                                       extract-footnote*
                                       extract-super/sub-script*
                                       extract-entity*
                                       extract-latex-fragment*
                                       extract-markup*))])
                       (extract-object-sub* sub))])))
   positions
   str))

(module+ test
  (check-match (extract-markup* " *test*  ")
               (list " " (node:markup "*" '("test")) "  ")))

(define current-org-link-types (make-parameter '("http" "https" "ftp" "mailto" "file" "news" "shell" "elisp" "doi" "message" "file+sys" "file+emacs" "bbdb" "bibtex" "docview" "gnus" "info" "irc" "mhe" "rmail")))

(define (escape-regexp str)
  ;; TODO: do more cases
  (string-replace
   (string-replace
    (string-replace
     (string-replace str "+" "\\+")
     "*" "\\*")
    "(" "\\(")
   ")" "\\)"))

(define (extract-angular-link* str)
  (define positions (regexp-match-positions* (string-append "<" (string-join (map escape-regexp (current-org-link-types)) "|") ":[^]<>\r\n]+>") str))
  (extract-simple* node:angular-link positions str))

(define WORD-CONSTITUANT-RE "\\w")

(define (extract-link* str)
  ;; TODO: can't be checked need to know what the radios are first PRE1 RADIO
  ;; POST1 ("radio" link)
  ;;
  ;; One easy use is to check the file for radio first in extract-object ? (but it will not work for radio declared after)

  (define protocol-re (string-join (map escape-regexp (current-org-link-types)) "|"))
  ;; TODO: WORD-CONSTITUANT-RE should be UTF-8 "\\p{N}|\\p{L}"
  (define link1 (regexp (string-append "(?<!" WORD-CONSTITUANT-RE ")(" protocol-re ":[^ \r\n\f\t()<>]+)(?!" WORD-CONSTITUANT-RE ")")))
  (define link2 (let ([path3 (string-append
                              "("
                              (string-join
                               (list (string-append "(" protocol-re "):[^][]+")
                                     (string-append "(" protocol-re ")://[^][]+")
                                     "id:[a-fA-f0-9]+(-[a-fA-f0-9]+)*"
                                     "#[^][]+"
                                     "\\([^][]+\\)"
                                     "[^][]+")
                               ")|(")
                              ")")])
                  (regexp (string-append "\\[\\[(" path3 ")\\](\\[[^][]+\\])?\\]"))))

  (define (extract-object-link* str)
    (match (regexp-match #rx"^\\[\\[([^]]+)\\](\\[(.+?)\\])?\\]$" str)
      [(list _ link #f #f)
       (node:link link '())]
      [(list _ link _ description)
       (node:link
        link
        (parameterize ([current-extractors
                        (set-intersect
                         (current-extractors)
                         (set extract-date* extract-inline-babel* extract-inline-block* extract-snippet*
                              extract-macro* extract-angular-link* extract-markup* extract-target* extract-latex-fragment*
                              extract-super/sub-script*))])
          (extract-object-sub* description)))]))

  (define (make-plain-link str)
    (define data (regexp-match #rx"^(.+?):(.+)$" str))
    (node:link (second data) (third data)))

  (define positions (regexp-match-positions* link2 str))
  (define elems (extract-simple* extract-object-link* positions str))
  (let loop ([elems elems]
             [result '()])
    (match elems
      ['() (reverse result)]
      [(list (? node:link? elem)) (reverse (cons elem result))]
      [(list-rest (? node:link? elem) rst) (loop rst (cons elem result))]
      [(list-rest (? string? str) rst)
       (define positions (regexp-match-positions* link1 str #:match-select cadr))
       (loop rst (let loop ([elems (extract-simple* make-plain-link positions str)] [result result])
                   (if (empty? elems) result (loop (rest elems) (cons (first elems) result)))))])))


(define (extract-simple* constructor positions str)
  (let loop ([positions positions]
             [result (if (empty? positions)
                         (list str)
                         (let ([sub (substring str 0 (caar positions))])
                           (if (string=? "" sub) (list) (list sub))))])
    (if (empty? positions)
        (reverse result)
        (loop (rest positions)
              (let* ([start (caar positions)]
                     [end (cdar positions)]
                     [res (cons (constructor (substring str start end)) result)])
                (if (empty? (cdr positions))
                    (let ([sub (substring str end)])
                      (if (string=? "" sub) res (cons sub res)))
                    (cons (substring str end (caadr positions)) res)))))))

;; http://orgmode.org/worg/dev/org-syntax.html#Line_Breaks
(define (extract-space-break* str)
  (define positions (regexp-match-positions* #rx"\\\\( *[\r\n]| +)" str))
  (extract-simple* node:line-break positions str))

;; http://orgmode.org/worg/dev/org-syntax.html#Inline_Babel_Calls_and_Source_Blocks
(define (extract-inline-babel* str)
  (define positions (regexp-match-positions* #rx"call_[^\r\n()]+(\\[[^]\n]+\\])?\\([^)\n]*\\)" str))
  (extract-simple* node:inline-babel positions str))

(module+ test
  (check-match (extract-inline-babel* "start call_name(content) end")
               (list "start " (node:inline-babel "call_name(content)") " end")))

;; http://orgmode.org/worg/dev/org-syntax.html#Inline_Babel_Calls_and_Source_Blocks
(define (extract-inline-block* str)
  (define positions (regexp-match-positions* #rx"src_[^\r\n \f\t]+(\\[[^]\n]+\\])?{[^}\n]*}" str))
  (extract-simple* node:inline-block positions str))

(module+ test
  (check-match (extract-inline-block* "start src_racket{(+ 1 1)} end")
               (list "start " (node:inline-block "src_racket{(+ 1 1)}") " end")))

;; http://orgmode.org/worg/dev/org-syntax.html#Export_Snippets
(define (extract-snippet* str)
  (define positions (regexp-match-positions* #px"@@(\\p{L}|\\p{N}|-)+:([^@]|@[^@])*@@" str))
  (extract-simple* node:snippet positions str))

(module+ test
  (check-match (extract-snippet* "start@@name:content@@end")
               (list "start" (node:snippet "@@name:content@@") "end")))

;; http://orgmode.org/org.html#Macro-replacement
(define (extract-macro* str)
  ;; TODO: accept more than defined in the doc.
  (define positions (regexp-match-positions* #rx"{{{[^}]+}}}" str))
  (extract-simple* node:macro positions str))

(module+ test
  (check-match (extract-macro* "start{{{macro}}}end")
               (list "start" (node:macro "{{{macro}}}") "end")))

;; http://orgmode.org/org.html#Dates-and-Times
(define (extract-date* str)
  (define date "[0-9]{4}-[0-9]{2}-[0-9]{2} [^]+->0-9\r\n ]+")
  (define time "[0-9]{2}:[0-9]{2}")
  (define repeater-or-delay "([.+]?\\+|-?-)[0-9]+[hdwmy]")
  (define supra
    (pregexp
     (string-append
      "("
      (string-join
       (list
        "<%%\\([^>\r\n]+\\)>"
        (string-append "<" date " " time "-" time "( " repeater-or-delay "){0,2}>")
        (string-append "\\[" date " " time "-" time "( " repeater-or-delay "){0,2}\\]")
        (string-append "<" date "( " time ")?" "( " repeater-or-delay "){0,2}>--<" date "( " time ")?" "( " repeater-or-delay "){0,2}>")
        (string-append "\\[" date "( " time ")?" "( " repeater-or-delay "){0,2}\\]--\\[" date "( " time ")?" "( " repeater-or-delay "){0,2}\\]")
        (string-append "<" date "( " time ")?" "( " repeater-or-delay "){0,2}>")
        (string-append "\\[" date "( " time ")?" "( " repeater-or-delay "){0,2}\\]"))
       ")|(")
      ")")))
  (define positions (regexp-match-positions* supra str))
  (extract-simple* node:date positions str))

(module+ test
  (check-match (extract-date* "<2015-06-06 Sat 00:00>")
               (list (node:date "<2015-06-06 Sat 00:00>")))
  (check-match  (extract-date* "[2015-06-06 Sat 00:00]")
               (list (node:date "[2015-06-06 Sat 00:00]")))
  (check-match (extract-date* "<2015-06-06 Sat>")
              (list (node:date "<2015-06-06 Sat>"))))

(define (pack-paragraph elem elems)
  (match* (elem elems)
    [((node:paragraph _ texts0)
      (list-rest (node:paragraph position texts1) elems))
     (cons
      (node:paragraph position
                      (string-append texts1 "\n" texts0))
      elems)]
    [(elem (list-rest (node:paragraph position para) elems))
     (list* elem (node:paragraph position (extract-object para)) elems)]
    [(_ _) (cons elem elems)]))

(define (read-header-children children indentation in)
  (define (peek-indentation)
    (define header (regexp-match-peek #rx"^[*]+" in))
    (and header (bytes-length (first header))))
  (if (or (eof-object? (peek-char in))
          (and (peek-indentation)
               (<= (peek-indentation) indentation)))
      (reverse+apply-emphase children)
      (read-header-children (pack-paragraph (read-next-section in) children) indentation in)))

(define current-indentation (make-parameter 0))
(define current-header-status (make-parameter '("TODO" "DONE")))

(define (read-plain-list-children children in)
  (define (peek-indentation)
    (bytes-length (first (regexp-match-peek #rx"^ *" in))))
  (if (or (eof-object? (peek-char in))
          (< (peek-indentation) (current-indentation)))
      (reverse+apply-emphase children)
      (read-plain-list-children (pack-paragraph (read-next-section in) children) in)))

(define (decode-header line in indentation position)
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
       (extract-object text)
       cookie
       priority
       tags)))
  (node:header position level comment priority tags text status cookie (read-header-children '() indentation in)))

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

(module+ test
  (let ([data0 (open-input-string "* COMMENT TODO [#A] some text :house:garden:")]
        [data1 (open-input-string "* DONE some text [50%] :house:garden:")]
        [data2 (open-input-string "** [1/2] some text [50%]")]
        [data3 (open-input-string "* DONE text :text:  :tag:")])
    ;; TODO: use the decode header method
    (check-true (node:header? (read-next-section data0)))
    (check-true (node:header? (read-next-section data1)))
    (check-true (node:header? (read-next-section data2)))
    (check-true (node:header? (read-next-section data3)))))

(define (extract-tags text)
  (define pos (regexp-match-positions #rx":([^ ]+):$" text))
  (if pos
      (cons
       (string-append
        (substring text 0 (caar pos))
        (substring text (cdar pos)))
       (string-split (substring text (caadr pos) (cdadr pos)) ":"))
      text))

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
       (raise-user-error (format "Impossible to find end for :PROPERTIES: started at ~a:~a"
                                 (node:position-start-line position)
                                 (node:position-start-column position)))]))
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
        (for/list ([line (reverse result)])
          (for/list ([column line])
            (if (regexp-match #rx"^-+$" column)
                column
                (extract-object column))))))))

(define (read-until-line re in [result ""])
  (define line (read-line in))
  (cond [(eq? eof line) eof]
        [(regexp-match? re line) result]
        [else (read-until-line re in (string-append result line "\n"))]))

(define (decode-block type lang in position)
  (define text (read-until-line (string-append "^ *#[+](?i:END)_" type " *$") in))
  (when (eq? eof text)
    (raise-user-error (format "Impossible to find end for ~a started at ~a:~a"
                              (string-append "#+BEGIN_" type)
                              (node:position-start-line position)
                              (node:position-start-column position))))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (node:block
   (struct-copy node:position position
                [end-line end-line]
                [end-column end-column]
                [end-offset end-offset])
   type
   lang
   text))

(define (decode-dynamic-block name params in position)
  (define text (read-until-line (string-append "^ *#[+](?i:END):[ \t\f]*$") in))
  (when (eq? eof text)
    (raise-user-error (format "Impossible to find end for dynamic block started at ~a:~a"
                              (node:position-start-line position)
                              (node:position-start-column position))))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (node:dynamic-block
   (struct-copy node:position position
                [end-line end-line]
                [end-column end-column]
                [end-offset end-offset])
   name
   params
   text))

(define (decode-latex name in position)
  (define text (read-until-line (string-append "^\\\\end\\{" name "\\}") in))
  (when (eq? eof text)
    (raise-user-error (format "Impossible to find end for ~a started at ~a:~a"
                              (string-append "\\begin{" name "}")
                              (node:position-start-line position)
                              (node:position-start-column position))))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (node:latex
   (struct-copy node:position position
                [end-line end-line]
                [end-column end-column]
                [end-offset end-offset])
   name
   text))

(define (reverse+apply-emphase elems)
  (reverse
   (match elems
     [(list-rest (node:paragraph position texts) elems)
      (cons (node:paragraph position (extract-object texts)) elems)]
     [else elems])))

(define (read-org in)
  (let loop ([elems '()])
    (define elem (read-next-section in))
    (if (eof-object? elem)
        (reverse+apply-emphase elems)
        (loop
         (pack-paragraph elem elems)))))

(define (read-next-section in)
  (define specials-re #rx"^([ ]*)(ID|TODO|TAGS|ALLTAGS|CATEGORY|PRIORITY|DEADLINE|SCHEDULED|CLOSED|TIMESTAMP|TIMESTAMP_IA|CLOCKSUM|CLOCKSUM_T|BLOCKED|ITEM|FILE):([.]*)")
  (define-values (start-line start-column start-offset) (port-next-location in))
  (define line (read-line in))
  (define-values (end-line end-column end-offset) (port-next-location in))
  (define position (node:position start-line start-column start-offset
                                  end-line end-column end-offset))
  ;; contain all the begining type of line of org-mode
  (match line
    [(regexp "^([*]+) " (list _ indentation)) (decode-header line in (string-length indentation) position)]
    [(regexp "^[ ]*[|]") (decode-table line in position)]
    [(regexp "^ *#[+](?i:BEGIN)_([A-Za-z]+)[ ]?(.*)" (list _ type lang))
     (decode-block type lang in position)]
    [(regexp "^ *#[+](?i:BEGIN): +([^ \t\f]+)[ ]?(.*)" (list _ name params))
     (decode-dynamic-block name params in position)]
    [(regexp "^ *:(?:$|(?:[ \t\f]+(.*)))" (list _ data))
     (node:fixed-width position (or data ""))]
    [(regexp "^ *(-----[-]*)[ \t\f]*$" (list _ text))
     (node:horizontal-rule position text)]
    ;; TODO: didn't work, not utf8 not multiline
    [(pregexp "^ *\\\\begin\\{([A-Za-z0-9*]+)\\}(.+?)\\\\end\\{[A-Za-z0-9*]+\\}" (list _ name text))
      (node:latex position name text)]
    [(pregexp "^ *\\\\begin\\{((?:\\p{N}|\\p{L}|[*])+)\\}" (list _ name))
     (decode-latex name in position)]
    [(regexp "^( *):PROPERTIES:" (list _ indentation))
     (decode-properties (string-length indentation) in position)]
    [(regexp "^( *)#[+]([A-Z]+):(.*)" (list _ indentation name text))
     (node:keyword position (string-length indentation) name (string-trim text))]
    [(regexp "^( *)(-|[+]|[0-9]+[.)]) ?(\\[[ X-]\\])?(.*)" (list _ indentation type checkbox text))
     ;; Skip [@NUMBER] specificity

     ;; tag :: [cookie%] text
     ;; [cookie%] tag :: text
     ;; tag :: text [cookie%]
     ;; text [cookie%]
     ;; text
     (let* ([cookie (regexp-match #rx"\\[([0-9]+%|[0-9]+/[0-9]+)\\]" text)]
            [cookie (and cookie (second cookie))]
            [text (if cookie (regexp-replace #rx"\\[([0-9]+%|[0-9]+/[0-9]+)\\]" text "") text)]
            [tag (regexp-match #rx"^ *([^:]+?) *::" text)]
            [tag (and tag (second tag))]
            [text (if tag (regexp-replace #rx"^ *([^:]+?) *::" text "") text)])
       (parameterize ([current-indentation (+ 1 (string-length indentation))])
         (node:plain-list position
                          (string-length indentation)
                          (case type
                            [("+") 'plus]
                            [("-") 'minus]
                            [else (string->number (first (regexp-match "[0-9]+" type)))])
                          checkbox
                          cookie
                          tag
                          ;; TODO: position is not correct
                          (read-plain-list-children (list (node:paragraph position text)) in))))]
    [(regexp "^( +)[*] (\\[[ X-]\\])?(.*)" (list _ indentation checkbox text))
     (let* ([cookie (regexp-match #rx"\\[([0-9]+%|[0-9]+/[0-9]+)\\]" text)]
            [cookie (and cookie (second cookie))]
            [text (if cookie (regexp-replace #rx"\\[([0-9]+%|[0-9]+/[0-9]+)\\]" text "") text)]
            [tag (regexp-match #rx"^ *([^:]+?) *::" text)]
            [tag (and tag (second tag))]
            [text (if tag (regexp-replace #rx"^ *([^:]+?) *::" text "") text)])
       (parameterize ([current-indentation (+ 1 (string-length indentation))])
         (node:plain-list position
                          (string-length indentation)
                          'bullet
                          checkbox
                          cookie
                          tag
                          ;; TODO: position is not correct
                          (read-plain-list-children (list (node:paragraph position text)) in))))]
    [(regexp specials-re (list _ indentation name text))
     (node:special position (string-length indentation) (string-length indentation) name (string-trim text))]
    [(regexp "^( *)#(.*)" (list _ indentation text))
     (node:comment position (string-length indentation) (string-trim text))]
    [(? eof-object?) eof]
    [else (node:paragraph position (remove-indentation line))]))

(define (remove-indentation line)
  (cond
    [(= 0 (current-indentation)) line]
    [(let ([r (regexp-match #rx"^ +" line)])
       (and r
            (> (string-length (first r)) (current-indentation))))
       ;; emacs indent to list level +2 but list level +1 is accepted as
       ;; children
       (substring line (add1 (current-indentation)))]
    [else (substring line (current-indentation))]))

(define (display* out . elems)
  (for ([elem elems])
    (display elem out)))

(define (display-objects objects out)
  (for ([object objects])
    (match object
      [(struct node:markup [border text]) (display border out)
                                          (display-objects text out)
                                          (display border out)]
      [(struct node:macro [text]) (display* out "{{{" text "}}}")]
      [(struct node:date [text]) (display text out)]
      [(struct node:snippet [text]) (display text out)]
      [(struct node:line-break [text]) (display text out)]
      [(struct node:target [text]) (display text out)]
      [(struct node:inline-babel [text]) (display text out)]
      [(struct node:inline-block [text]) (display text out)]
      [(struct node:entity [text]) (display text out)]
      [(struct node:latex-fragment [text]) (display text out)]
      [(struct node:footnote [label description]) (display* out "[fn:" label ":")
                                                  (display-objects description out)
                                                  (display "]" out)]
      [(struct node:angular-link [text]) (display text out)]
      [(struct node:radio-target [text]) (display text out)]
      [(struct node:link [link description]) (display* out "[[" link "]")
                                             (when description
                                               (display "[" out)
                                               (display-objects description out)
                                               (display "]" out))
                                             (display "]" out)]
      [(struct node:radio-target [text]) (display "<<<" out)
                                         (display-objects text)
                                         (display ">>>")]
      [(struct node:sub-script [name text]) (display* out name "_")
                                            (if (string? text)
                                                (display text out)
                                                (display-objects text out))]
      [(struct node:super-script [name text]) (display* out name "^")
                                              (if (string? text)
                                                  (display text out)
                                                  (display-objects text out))]
      [(? string?) (display object out)])))


(define (display-whitespace nb out)
  (display (make-string nb  #\space) out))

(define (write-org org-struct out [extra-indentation 0])
  (for ([elem org-struct])
    (match elem
      [(struct node:header [position level comment priority tags text status cookie children])
       (display* out (make-string level #\*) " ")
       (and status (display*  out status " "))
       (and priority (display* out "[#" priority "] "))
       (and comment (display "COMMENT " out))
       (display-objects text out)
       (and cookie (display* out "[" cookie "]"))
       (and tags (display (string-join tags ":" #:before-first ":" #:after-last ":") out))
       (display #\newline out)
       (write-org children out 0)]
      [(struct node:comment [position indentation text])
       (display-whitespace indentation out)
       (display* out "#" text "\n")]
      [(struct node:block [position type lang text])
       (display-whitespace (+ 2 extra-indentation) out)
       (display* out (format "#+BEGIN_~a ~a" type lang)
                 "\n"
                 text
                 "\n")
       (display-whitespace (+ 2 extra-indentation) out)
       (display* out (format "#+END_~a" type) "\n")]
      [(struct node:special [position indentation name text])
       (display-whitespace (+ extra-indentation indentation) out)
       (display* out name ": " text "\n")]
      [(struct node:keyword [position indentation name text])
       (display-whitespace indentation out)
       (display* out "#+" name ": " text "\n")]
      [(struct node:properties [position indentation properties])
       (display-whitespace (+ extra-indentation indentation) out)
       (display ":PROPERTIES:\n" out)
       (for ([prop properties])
         (display (format ":~a: ~a"
                          (node:property-name prop)
                          (node:property-text prop))
                  out)
         (display #\newline out))
       (display-whitespace (+ extra-indentation indentation) out)
       (display ":END:\n" out)]
      [(struct node:horizontal-rule [position text])
       (display-whitespace extra-indentation out)
       (display text out)]
      [(struct node:dynamic-block [position name params text])
       (display-whitespace extra-indentation out)
       (display* out "#+BEGIN: " name " " params "\n")
       (display text out)
       (display-whitespace extra-indentation out)
       (display "#+END:\n")]
      [(struct node:fixed-width [position text])
       (display-whitespace extra-indentation out)
       (display* out ":" text)]
      [(struct node:latex [position name text])
       (display-whitespace extra-indentation out)
       (display* out "\\begin{" name "}\n")
       (display text out)
       (display* out "\\end{" name "}\n")]
      [(struct node:table [position indentation rows])
       (let ([space (make-string (+ extra-indentation indentation) #\space)])
         (for ([row rows])
           (display space out)
           (display "|" out)
           (for ([column row])
             (display-objects column out)
             (display "|" out))
           (display #\newline out)))]
      [(struct node:plain-list [position indentation type checkbox cookie tag children])
       (let ([space (make-string indentation #\space)])
         (display space out)
         (display
          (case type
            [(bullet) "* "]
            [(plus) "+ "]
            [(minus) "- "]
            [else (string-append (number->string type) ". ")])
          out)
         (and checkbox (display* out checkbox))
         (and cookie (display* out "[" cookie "] "))
         (write-org children out indentation))]
      [(struct node:paragraph [position texts])
       (display-objects
        (map (lambda (str)
               (if (string? str)
                   (string-replace str "\n" (string-append "\n" (make-string (+ 2 extra-indentation) #\space)))
                   str))
             texts)
        out)
       (display "\n" out)])))


(module+ test
  (let ([block #<<ORGCODE
#+BEGIN_SRC racket
(+ 1 1)
#+END_SRC

#+begin_src racket
(+ 1 1)
#+end_src
ORGCODE
               ])
    (check-match (read-org (open-input-string block))
                 (list (? node:block?) _ (? node:block?))))
(let ([plain-list #<<ORGCODE
  1) a
  2) b
ORGCODE
                  ])
  (check-match
   (read-org (open-input-string plain-list))
   (list
    (node:plain-list _ 2 1 #f #f #f (list (node:paragraph _ '("a"))))
    (node:plain-list _ 2 2 #f #f #f (list (node:paragraph _ '("b")))))))
(let ([plain-list #<<ORGCODE
1.
 toto
2.
 tutu
ORGCODE
                  ])
  (check-match
   (read-org (open-input-string plain-list))
   (list (? node:plain-list?) (? node:plain-list?))))
(let ([plain-list #<<ORGCODE
- toto
  maison
  - tutu
    maison
  maison
ORGCODE
                  ])
  (check-match
   (read-org (open-input-string plain-list))
   (list (node:plain-list _ 0 'minus _ _ _
                          (list (node:paragraph _ '("toto\nmaison"))
                                (? node:plain-list?)
                                (node:paragraph _ '("maison")))))))
(let ([plain-list #<<ORGCODE
\begin{text}aaaa\end{text}
\begin{text}
aaa
bbb
\end{text}
ORGCODE
                  ])
  (check-match
   (read-org (open-input-string plain-list))
   (list (node:latex _ "text" "aaaa")
         (node:latex _ "text" "aaa\nbbb\n")))))

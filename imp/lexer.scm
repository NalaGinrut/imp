;;  Copyright (C) 2012
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Ragnarok is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Ragnarok is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (language imp lexer)
  #:use-module (system base lalr)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (make-imp-tokenizer))

(define *operations* "~+-*=<^|:")

(define *delimiters* (string-append " ,.\n;\t" *operations*))

(define *invalid-char* "`!@#$%&\\'\"?></")
  
(define *keywords*
  '(("if" . if)
    ("then" . then)
    ("else" . else)
    ("while" . while)
    ("do" . do)
    ("skip" . skip)
    ("true" . true)
    ("false" . false)))

(define *op-tokens*
  '(("+" . +)
    ("-" . -)
    ("*" . *)
    ("=" . eq)
    ("<=" . less-eq)
    (":=" . assign)
    ("|" . or)
    ("^" . and)
    ("~" . not)))

(define *puctuations*
  '((";" . semi-colon)
    ("," . comma)
    ("." . dot)
    ("{" . lbrace)
    ("}" . rbrace)
    ("(" . lparen)
    (")" . rparen)
    ("\n" . newline)))

(define *charset-not-in-var*
  (string->char-set (string-append *invalid-char* *operations* *delimiters*)))

(define is-whitespace?
  (lambda (c)
    (char-set-contains? char-set:whitespace c)))

;; in Simple IMP, we only have bin/oct/hex number
(define is-number?
  (lambda (c)
    (char-set-contains? char-set:hex-digit c)))

(define-syntax-rule (checker what c)
  (string-contains what (string c)))

(define is-delimiter?
  (lambda (c)
    (or (eof-object? c)
	(checker *delimiters* c))))

(define is-op?
  (lambda (c)
    (and (not (eof-object? c))
	 (checker *operations* c))))

(define (unget-char1 c port)
  (and (char? c) (unread-char c port)))
 
(define port-skip
  (lambda (port n)
    (let lp((n n))
      (cond
       ((not (zero? n))
	(read-char port)
	(lp (1- n)))))))
	 
(define read-word
  (lambda (port)
    (read-delimited *delimiters* port 'peek)))
    
(define next-is-keyword?
  (lambda (port)
    (let* ((word (read-word port))
	   (keyword (assoc-ref *keywords* word)))
      (if keyword
	  keyword
	  (begin
	    (unread-string word port)
	    #f)))))

(define* (get-number lst #:optional (base 10))
  (string->number (apply string (reverse lst)) base))

(define come-back-baby
  (lambda (port . babies)
    (for-each (lambda (c)
		(unget-char1 c port))
	      babies)))

(define is-immediate-number?
  (lambda (c)
    (let ((i (char->integer c)))
      (and (>= i #x30) (<= i #x39)))))

(define next-is-number?
  (lambda (port)
    (if (is-immediate-number? (peek-char port))
	10 ;; decimal situation
	(let* ((c0 (read-char port))
	       (c1 (read-char port))
	       (c2 (peek-char port)))
	  (if (char=? c0 #\#)
              (if (is-number? c2)
                  (case c1
                    ((#\x) 16)
                    ((#\o) 8)
                    ((#\b) 2)
                    (else (error "invalid number base!" c1)))
                  (error "invalid number form!" c2))
	      (begin
		(come-back-baby port c1 c0)
		#f))))))
	    
(define read-number
  (lambda (port base)
    (let lp((c (read-char port)) (num '()))
      (cond
       ((or (is-delimiter? c) (is-op? c)) 
	(let ((ret (get-number num base)))
	  (or (eof-object? c) (unget-char1 c port))
	  (if ret 
	      (values 'number ret)
	      (error "invalid number!" (apply string (reverse num))))))
       ((is-number? c)
	(lp (read-char port) (cons c num)))
       (else
	(error "invalid number!" (apply string (reverse (cons c num)))))))))

(define next-is-operation?
  (lambda (port)
    (let lp((c (read-char port)) (op '()))
      (cond
       ((checker *operations* c)
	(lp (read-char port) (cons c op)))
       (else
	(unget-char1 c port)
	(assoc-ref *op-tokens* (apply string (reverse op))))))))

(define check-var
  (lambda (var)
    (cond
     ((and (not (string-null? var))
           (is-immediate-number? (string-ref var 0)))
      #f)
     (else
      (string-any (lambda (c)
                   (and (char-set-contains? *charset-not-in-var* c) c))
                  var)))))
	 
(define next-is-var?
  (lambda (port)
    (let ((word (read-word port)))
      (if (check-var word)
	  (string->symbol word)
	  (begin
	    (unread-string word port)
	    #f)))))
    
(define next-is-comment?
  (lambda (port)
    (let* ((c0 (read-char port))
	   (c1 (read-char port)))
      (cond
       ((and (char=? #\/ c0) (char=? #\/ c1))
	#t)
       (else
	(come-back-baby port c1 c0)
	#f)))))

(define skip-comment
  (lambda (port)
    (read-delimited "\n" port)))

(define next-is-puctuation?
  (lambda (port)
    (let* ((c (read-char port))
	   (punc (assoc-ref *puctuations* (string c))))
      (cond
       (punc punc)
       (else
	(unget-char1 c port)
	#f)))))
	  
(define (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

(define next-token
  (lambda (port)
    (let* ((loc (port-source-location port))
	   (return (lambda (category value)
		     (make-lexical-token category loc value)))
	   (c (peek-char port)))
      (cond
       ((eof-object? c) '*eoi*)
       ((next-is-comment? port) 
	(skip-comment port) ;; only line comment
	(next-token port))
       ((is-whitespace? c) 
	(read-char port) 
	(next-token port))
       ((next-is-number? port)
	=> (lambda (base)
	     (receive (type ret) (read-number port base) (return type ret))))
       ((next-is-keyword? port) 
	=> (lambda (word)
	     (return word #f)))
       ((next-is-var? port)
	=> (lambda (var)
	     (return 'variable var)))
       ((next-is-operation? port) 
	=> (lambda (op)
	     (return op #f)))
       ((next-is-puctuation? port)
	=> (lambda (punc)
 	     (return punc #f)))
       (else
        (error "invalid token!"))))))

(define imp-tokenizer
  (lambda (port)
    (let lp ((out '()))
      (let ((tok (next-token port)))
 	(if (eq? tok '*eoi*)
 	    (reverse! out)
 	    (lp (cons tok out)))))))

(define (make-imp-tokenizer port)
  (lambda ()
    (next-token port))) ;;(imp-tokenizer port)))

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
		(and (char? c) (unread-char c port)))
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
	  (if (and (char=? c0 #\#) (is-number? c2))
	      (case c1
		((#\x) 16)
		((#\o) 8)
		((#\b) 2)
		(else (error "invalid number base!" c1)))
	      (begin
		(come-back-baby port c1 c0)
		#f))))))
	    
(define read-number
  (lambda (port base)
    (let lp((c (read-char port)) (num '()))
      (cond
       ((or (is-delimiter? c) (is-op? c)) 
	(let ((ret (get-number num base)))
	  (or (eof-object? c) (unread-char c port))
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
	(unread-char c port)
	(assoc-ref *op-tokens* (apply string (reverse op))))))))

(define check-var
  (lambda (var)
    (call/cc (lambda (ret) 
	       (and (not (string-null? var))
		    (is-immediate-number? (string-ref var 0))
		    (ret #f))
	       (string-for-each 
		(lambda (c) 
		  (let ((s (string c)))
		    (unless (or (string-contains *invalid-char* s)
				(string-contains *operations* s)
				(string-contains *delimiters* s))
		      (ret #t))))
		var)
	       #f))))
	 
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
	(unread-char c port)
	#f)))))
	  
(define (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

(define imp-tokenizer
  (lambda (port)
    (let* ((loc (port-source-location port))
	   (return (lambda (category value)
		     (make-lexical-token category loc value)))
	   (c (peek-char port)))
      (cond
       ((eof-object? c) '*eoi*)
       ((next-is-comment? port) 
	(skip-comment port) ;; only line comment
	(imp-tokenizer port))
       ((is-whitespace? c) 
	(read-char port) 
	(imp-tokenizer port))
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
	(error "wrong syntax!" c))))))

(define (make-imp-tokenizer port)
  (lambda ()
    (imp-tokenizer port)))

;; (define imp-tokenize
;;   (lambda (port)
;;     (let lp ((out '()))
;;       (let ((tok (imp-tokenizer port)))
;; 	(if (eq? tok '*eoi*)
;; 	    (reverse! out)
;; 	    (lp (cons tok out)))))))
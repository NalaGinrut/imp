;;  Copyright (C) 2012 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (language imp spec)
  #:use-module (system base language)
  #:use-module (language imp compile-tree-il)
  #:use-module (language imp parser)
  #:export (imp))

;;;
;;; Language definition
;;;

(define-language imp
  #:title	"IMP language"
  #:reader      (lambda (port env) 
		  (imp-read port))
  #:compilers   `((tree-il . ,compile-tree-il))
  #:printer	write)

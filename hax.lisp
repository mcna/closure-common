;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: An event API for the HTML parser, inspired by SAX
;;;   Created: 2007-10-14
;;;    Author: David Lichteblau
;;;   License: BSD
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005,2007 David Lichteblau

;;; Redistribution and use  in source and binary   forms, with or  without
;;; modification, are permitted provided that the following conditions are
;;; met:                                                                  
;;;                                                                   
;;; 1. Redistributions  of  source  code  must retain  the above copyright
;;;    notice, this list of conditions and the following disclaimer.      
;;;                                                                   
;;; 2. Redistributions in  binary form must reproduce  the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution
;;;                                                                   
;;; THIS  SOFTWARE   IS PROVIDED ``AS  IS''   AND ANY  EXPRESS  OR IMPLIED
;;; WARRANTIES, INCLUDING, BUT NOT LIMITED  TO, THE IMPLIED WARRANTIES  OF
;;; MERCHANTABILITY  AND FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;; INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO,   PROCUREMENT OF SUBSTITUTE GOODS   OR
;;; SERVICES;  LOSS OF  USE,  DATA, OR  PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER  CAUSED AND ON ANY THEORY  OF LIABILITY,  WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;; IN ANY WAY  OUT OF THE  USE OF THIS SOFTWARE,  EVEN IF ADVISED OF  THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :hax
  (:use :common-lisp)
  (:export #:abstract-handler
	   #:default-handler
           
	   #:make-attribute
	   #:standard-attribute
           #:find-attribute
           #:attribute-name
           #:attribute-value
           #:attribute-specified-p
	   
           #:start-document
           #:start-element
           #:characters
           #:end-element
           #:end-document
           #:comment))

(in-package :hax)


;;;; ATTRIBUTE

(defgeneric attribute-name (attribute))
(defgeneric attribute-value (attribute))
(defgeneric attribute-specified-p (attribute))

(defclass standard-attribute ()
  ((name :initarg :name :accessor attribute-name)
   (value :initarg :value :accessor attribute-value)
   (specified-p :initarg :specified-p :accessor attribute-specified-p)))

(defun make-attribute (name value &optional (specified-p t))
  (make-instance 'standard-attribute
                 :name name
                 :value value
                 :specified-p specified-p))

(defun %rod= (x y)
  ;; allow rods *and* strings *and* null
  (cond
    ((zerop (length x)) (zerop (length y)))
    ((zerop (length y)) nil)
    ((stringp x) (string= x y))
    (t (runes:rod= x y))))

(defun find-attribute (name attrs)
  (find name attrs :key #'attribute-name :test #'%rod=))


;;;; ABSTRACT-HANDLER and DEFAULT-HANDLER

(defclass abstract-handler () ())
(defclass default-handler (abstract-handler) ())

(defgeneric start-document (handler name public-id system-id)
  (:method ((handler null) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil)
  (:method ((handler default-handler) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil))

(defgeneric start-element (handler name attributes)
  (:method ((handler null) name attributes)
    (declare (ignore name attributes))
    nil)
  (:method ((handler default-handler) name attributes)
    (declare (ignore name attributes))
    nil))

(defgeneric characters (handler data)
  (:method ((handler null) data)
    (declare (ignore data))
    nil)
  (:method ((handler default-handler) data)
    (declare (ignore data))
    nil))

(defgeneric end-element (handler name)
  (:method ((handler null) name)
    (declare (ignore name))
    nil)
  (:method ((handler default-handler) name)
    (declare (ignore name))
    nil))

(defgeneric end-document (handler)
  (:method ((handler null)) nil)
  (:method ((handler default-handler)) nil))

(defgeneric comment (handler data)
  (:method ((handler null) data)
    (declare (ignore data))
    nil)
  (:method ((handler default-handler) data)
    (declare (ignore data))
    nil))

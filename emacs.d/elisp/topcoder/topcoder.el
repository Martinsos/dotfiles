;;; topcoder.el --- use Emacs in the TopCoder Arena

;; Copyright (C) 2005 Tomasz Malesinski

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Author: Tomasz Malesinski <tmal@mimuw.edu.pl>
;; Keyword: topcoder

;; TODO replace superscripts and subscripts with something renderable
;; TODO keymap not working when autoloaded (Emacs bug?)
;; TODO save code given by setSource somewhere (as a backup file (*~)?)

(provide 'topcoder)

(require 'w3m)

(defgroup topcoder nil
  "Interfacing Emacs with the TopCoder Arena"
  :group 'external)

(defcustom topcoder-directory
  (concat (getenv "HOME") "/topcoder")
  "TopCoder working directory"
  :type '(directory)
  :group 'topcoder)

(defcustom topcoder-template-cpp
  "#include <iostream>
#include <sstream>
#include <algorithm>
#include <utility>
#include <vector>
#include <string>
#include <cstdio>
#include <cmath>

using namespace std;

$library
class $class-name {
public:
  $signature {
    $point
  }
};

$tests
// Born in Emacs
"
"TopCoder C++ code template"
:type '(string)
:group 'topcoder)

(defcustom topcoder-template-java
  "import java.util.*;
public class $class-name {
$library
  public $signature {
    $point
  }
$tests
}
// Born in Emacs
"
"TopCoder Java code template"
:type '(string)
:group 'topcoder)

(defcustom topcoder-template-cs
  ""
"TopCoder C# code template"
:type '(string)
:group 'topcoder)

(defcustom topcoder-template-vb
  ""
"TopCoder Visual Basic code template"
:type '(string)
:group 'topcoder)

(defcustom topcoder-code-library-cpp
  '(("FOR" . "#define FOR(i,a,b) for(int i=(a);i<=(b);++i)")
    ("FORD" . "#define FORD(i,a,b) for(int i=(a);i>=(b);--i)")
    ("REP" . "#define REP(i,n) for(int i=0;i<(n);++i)")
    ("VAR" . "#define VAR(v,init) __typeof(init) v=init")
    ("FOREACH" .
     "#define FOREACH(i,c) for(VAR(i,(c).begin());i!=(c).end();++i)"))
"TopCoder C++ code library"
:type '(alist :key-type (string) :value-type (string))
:group 'topcoder)

(defcustom topcoder-code-library-java nil
"TopCoder Java code library"
:type '(alist :key-type (string) :value-type (string))
:group 'topcoder)

(defcustom topcoder-code-library-cs nil
"TopCoder C# code library"
:type '(alist :key-type (string) :value-type (string))
:group 'topcoder)

(defcustom topcoder-code-library-vb nil
"TopCoder Visual Basic code library"
:type '(alist :key-type (string) :value-type (string))
:group 'topcoder)

(defcustom topcoder-cpp-compile-flags
  "-Wall -W -O2 -s"
  "TopCoder C++ compilation flags"
  :type '(string)
  :group 'topcoder)

(defvar topcoder-set-problem-hook)

(defun topcoder-parse-template (template dict)
  (let ((i 0)
	(point (point)))
    (while (< i (length template))
      (if (string-match "\\$\\(\\$\\|[a-zA-Z0-9_-]+\\)" template i)
	  (let ((beg (match-beginning 0))
		(var (match-string 1 template))
		(end (match-end 0)))
	    (insert (substring template i beg))
	    (cond ((equal var "$")
		   (insert "$"))
		  ((equal var "point")
		   (setq point (point)))
		  (t 
		   (insert (cdr (assoc
				 (intern-soft var)
				 dict)))))
	    (setq i end))
	(insert (substring template i))
	(setq i (length template))))
    (goto-char point)))

(defun topcoder-get-lexem (s i lexems)
  (if (atom lexems)
      nil
    (let ((r (funcall (car lexems) s i)))
      (if (atom r)
	  (topcoder-get-lexem s i (cdr lexems))
	r))))

(defun topcoder-tokenize-value (value)
  (let ((lexems
	 (list
	  (function (lambda (s i)
		      (if (memq (elt s i) '(?  ?\n ?\t))
			  (cons (1+ i) nil))))
	  (function (lambda (s i)
		      (let ((a (assoc (elt s i)
				      '((?{ . left-bracket)
					(?} . right-bracket)
					(?, . comma)))))
			(if (consp a)
			    (cons (1+ i) (cdr a))))))
	  (function (lambda (s i)
		      (if (eq (elt s i) ?\")
			  (if (string-match "\"\\(\\(\\\\\"\\|[^\"]\\)*\\)\""
					    s i)
			      ;; TODO unquote string!
			      (cons (match-end 0)
				    (cons 'string (match-string 1 s)))))))
	  (function (lambda (s i)
		      (if (eq (string-match
			       "-?\\([0-9]*\\.[0-9]+\\|[0-9]+\\)\\([eE]-?[0-9]+\\)?" s i) i)
			  (cons (match-end 0)
				(cons 'number (match-string 0 s))))))))
	(i 0)
	(res ()))
    (while (< i (length value))
      (let ((r (topcoder-get-lexem value i lexems)))
	(cond
	 ((atom r)
	  (setq res nil)
	  (setq i (length value)))
	 (t
	  (setq i (car r))
	  (if (not (eq nil (cdr r)))
	      (setq res (cons (cdr r) res)))))))
    (reverse res)))

(defun topcoder-parse-list (tokens)
  (cond
   ((atom tokens) nil)
   ((eq (car tokens) 'right-bracket)
    (cons nil (cdr tokens)))
   (t
    (let ((first (topcoder-parse-value1 tokens)))
      (when first
	(let ((res (cons (car first) nil))
	      (tok (cdr first))
	      (err t))
	  (while
	      (cond ((atom tok)
		     nil)
		    ((eq (car tok) 'right-bracket)
		     (setq tok (cdr tok))
		     (setq err nil))
		    ((eq (car tok) 'comma)
		     (let ((r (topcoder-parse-value1 (cdr tok))))
		       (if (atom r)
			   nil
			 (setq res (cons (car r) res))
			 (setq tok (cdr r))
			 t)))))
	  (if err
	      nil
	    (cons (reverse res) tok))))))))

      
(defun topcoder-parse-value1 (tokens)
  (if (atom tokens)
      nil
    (cond
     ((eq (car tokens) 'left-bracket)
      (topcoder-parse-list (cdr tokens)))
     ((consp (car tokens))
      (cond ((eq (caar tokens) 'number)
	     tokens)
	    ((eq (caar tokens) 'string)
	     tokens))))))

(defun topcoder-parse-value (value)
  (let ((r (topcoder-parse-value1 (topcoder-tokenize-value value))))
    (when (and r (atom (cdr r)))
      (car r))))

(defun topcoder-source-file-name (class-name language)
  (concat topcoder-directory "/" class-name "." language))

(defun topcoder-backup-file-name (class-name language)
  (concat topcoder-directory "/" class-name ".bak"))

(defun topcoder-simple-value-cpp-p (value)
  (and (consp value) (or (eq (car value) 'number)
			 (eq (car value) 'string))))

(defun topcoder-element-type (type)
  (cons (car type) (1- (cdr type))))

(defun topcoder-type-cpp (type)
  (if (= (cdr type) 0)
      (cond ((string-equal (car type) "String") "string")
	    ((string-equal (car type) "long") "long long")
	    (t (car type)))
    (concat "vector< " (topcoder-type-cpp (topcoder-element-type type))
	    " >")))

(defun topcoder-type-java (type)
  (let ((s (car type))
	(i (cdr type)))
    (while (> i 0)
      (setq s (concat s "[]"))
      (setq i (1- i)))
    s))

(defun topcoder-simple-value-cpp (value type)
  (cond ((eq (car value) 'number)
	 (if (string-equal (car type) "long")
	     (concat (cdr value) "LL")
	   (cdr value)))
	((eq (car value) 'string)
	 ;; TODO quote the string when it will be unquoted in tokenize
	 (concat "\"" (cdr value) "\""))))

(defun topcoder-composite-assignment-cpp (var value type
					      &optional dont-declare)
  (let ((type1 (topcoder-element-type type)))
    (if (and (consp value) (topcoder-simple-value-cpp-p (car value)))
	(concat
	 (if dont-declare
	     ""
	   (concat (topcoder-type-cpp type) " " var ";\n"))
	 "{" (topcoder-type-cpp type1) " tmpa[] = {"
	 (let ((topcoder-cac-type type1))
	   (mapconcat
	    (function (lambda (v)
			(topcoder-simple-value-cpp v topcoder-cac-type)))
	    value
	    ", "))
	 "};\n"
	 var ".assign(tmpa, tmpa + " (number-to-string (length value)) ");}\n")
      (let ((res (if dont-declare
		     (concat var ".clear();\n")
		   (concat (topcoder-type-cpp type) " " var ";\n"))))
	(while value
	  (setq res
		(concat res "{"
			(topcoder-composite-assignment-cpp "tmp"
							   (car value)
							   type1)
			var ".push_back(tmp);}n"))
	  (setq value (cdr value)))
	res))))

(defun topcoder-assignment-cpp (var value type &optional dont-declare)
  (if (topcoder-simple-value-cpp-p value)
      (concat (if dont-declare
		  ""
		(concat (topcoder-type-cpp type) " "))
	      var " = "
	      (topcoder-simple-value-cpp value type) ";\n")
    (topcoder-composite-assignment-cpp var value type dont-declare)))

(defun topcoder-print-res-call-cpp (type stream arg)
  (cond
   ((> (cdr type) 0)
    (concat "_print_res(" stream ", " arg ");"))
   ((string-equal (car type) "String")
    (concat stream " << '\"' << " arg " << '\"';"))
   (t (concat stream " << " arg ";"))))
      
(defun topcoder-print-res-cpp (type)
  (cond
   ((> (cdr type) 0)
    (concat
     (topcoder-print-res-cpp (topcoder-element-type type))
     "void _print_res(ostream& str, " (topcoder-type-cpp type) " v) {
  bool first = 1;
  str << \"{\";
  for (" (topcoder-type-cpp type)
    "::iterator i = v.begin(); i != v.end(); i++) {
    if (first) {
      str << \" \";
      first = 0;
    } else
      str << \", \";
    " (topcoder-print-res-call-cpp (topcoder-element-type type) "str" "*i") "
  }
  str << \" }\";
}
"))
   (t "")))

(defun topcoder-return-handlers-cpp (type)
  (concat
   (topcoder-print-res-cpp type)
   (if (string-equal "double" (car type))
       (concat
	"bool _cmp_res(double expected, double got) {
  if (fabs(expected - got) < 1e-9) return 1;
  else {
    double a = (1.0 - 1e-9) * expected;
    double b = (1.0 + 1e-9) * expected;
    return got < max(a, b) && got > min(a, b);
  }
}
"
	(if (> (cdr type) 0)
	    "bool _cmp_res(vector<double> expected, vector<double> got) {
  return expected.size() == got.size()
    && equal(expected.begin(), expected.end(), got.begin(), (bool (*)(double, double))_cmp_res);
}
"
	  ""))
     "")))

(defun topcoder-print-res-call-java (type arg)
  (cond
   ((> (cdr type) 0)
    (concat "_print_res(" arg ");"))
   ((string-equal (car type) "String")
    (concat "System.out.print('\"' + " arg " + '\"');"))
   (t (concat "System.out.print(" arg ");"))))
      
(defun topcoder-print-res-java (type)
  (cond
   ((> (cdr type) 0)
    (concat
     (topcoder-print-res-java (topcoder-element-type type))
     "public static void _print_res(" (topcoder-type-java type) " a) {
  boolean first = true;
  System.out.print('{');
  for (int i = 0; i < a.length; i++) {
    if (first) {
      System.out.print(' ');
      first = false;
    } else
      System.out.print(\", \");
    "
     (topcoder-print-res-call-java (topcoder-element-type type) "a[i]")
"
  }
  System.out.print(\" }\");
}
"))
   (t "")))

(defun topcoder-return-handlers-java (type)
  (concat
   (topcoder-print-res-java type)
   (if (string-equal "double" (car type))
       (concat
	"static boolean _cmp_res(double expected, double got) {
  if (Math.abs(expected - got) < 1e-9) return true;
  else {
    double a = (1.0 - 1e-9) * expected;
    double b = (1.0 + 1e-9) * expected;
    return got < Math.max(a, b) && got > Math.min(a, b);
  }
}
"
	(if (> (cdr type) 0)
	    "static boolean _cmp_res(double[] expected, double[] got) {
  if (expected.length != got.length) return false;
  for (int i = 0; i < expected.length; i++)
    if (!_cmp_res(expected[i], got[i]))
      return false;
  return true;
}
"
	  ""))
     "")))

(defun topcoder-test-function-cpp (desc)
  (let ((ret (cdr (assoc 'return desc))))
    (concat
     (topcoder-return-handlers-cpp ret)
     "void _run_test("
     (mapconcat
      (function (lambda (p) (concat (topcoder-type-cpp (car p)) " " (cdr p))))
      (cdr (assoc 'params desc))
      ", ")
     ", " (topcoder-type-cpp ret) " res_expected, int test_no) {\n"
     (topcoder-type-cpp ret) " tc_res = (new " (cdr (assoc 'class-name desc))
     ")->" (cdr (assoc 'method-name desc)) "("
     (mapconcat 'cdr
		(cdr (assoc 'params desc))
		", ")
     ");\n"
     (let ((cmp "tc_res == res_expected")
	   (ok-show nil)
	   (print-results
	    (concat "cout << \"expected: \";\n"
		    (topcoder-print-res-call-cpp ret "cout" "res_expected")
		    "\ncout << endl << \"got: \";\n"
		    (topcoder-print-res-call-cpp ret "cout" "tc_res")
		    "\ncout << endl;\n")))
       (if (equal "double" (car ret))
	   (progn
	     (setq cmp "_cmp_res(res_expected, tc_res)")
	     (setq ok-show t)))
       (concat 
	"if (" cmp ")\n"
	"cout << \"--- test \" << test_no << \": ok ---\" << endl;\n"
	"else {\n"
	"cout << \"--- test \" << test_no << \": failed ---\" << endl;\n"
	(if ok-show "" print-results)
	"}\n"
	(if ok-show print-results "")
	"}\n")))))

(defun topcoder-test-cpp (desc test n)
  (let ((p (cdr (assoc 'params desc)))
	(ret (cdr (assoc 'return desc)))
	(a (car test))
	(res ""))
    (while p
      (setq res (concat res
                        (topcoder-assignment-cpp (cdr (car p))
                                                 (topcoder-parse-value(car a))
                                                 (car (car p))
                                                 t)))
      (setq p (cdr p))
      (setq a (cdr a)))
    (concat res 
	    (topcoder-assignment-cpp "res_expected"
				     (topcoder-parse-value (cdr test))
				     (cdr (assoc 'return desc))
				     t)
	    "_run_test("
	    (mapconcat 'cdr
		       (cdr (assoc 'params desc))
		       ", ")
	    ", res_expected, " (number-to-string n) ");\n")))

(defun topcoder-test-function-java (desc)
  (let ((ret (cdr (assoc 'return desc))))
    (concat
     (topcoder-return-handlers-java ret)
     "static void _run_test("
     (mapconcat
      (function (lambda (p) (concat (topcoder-type-java (car p)) " " (cdr p))))
      (cdr (assoc 'params desc))
      ", ")
     ", " (topcoder-type-java ret) " res_expected, int test_no) {\n"
     (topcoder-type-java ret) " tc_res = new " (cdr (assoc 'class-name desc))
     "()." (cdr (assoc 'method-name desc)) "("
     (mapconcat 'cdr
		(cdr (assoc 'params desc))
		", ")
     ");\n"
     (let ((cmp "tc_res == res_expected")
	   (ok-show nil)
	   (print-results
	      (concat
	       "System.out.print(\"expected: \");\n"
	       (topcoder-print-res-call-java ret "res_expected") "\n"
	       "System.out.print(\"\\ngot: \");\n"
	       (topcoder-print-res-call-java ret "tc_res")
	       "\nSystem.out.println();\n")))
       (cond ((equal "double" (car ret))
	      (setq cmp "_cmp_res(res_expected, tc_res)")
	      (setq ok-show t))
	     ((> (cdr ret) 0)
	      (setq cmp "Arrays.equals(res_expected, tc_res)"))
	     ((equal "String" (car ret))
	      (setq cmp "tc_res.equals(res_expected)")))
       (concat
	"if (" cmp ")\n"
	"System.out.println(\"--- test \" + test_no + \""
	": ok ---\");\n"
	"else {\n"
	"System.out.println(\"--- test \" + test_no + \""
	": failed ---\");\n"
	(if ok-show "" print-results)
	"}\n"
	(if ok-show print-results "")
	))
     "}\n")))

(defun topcoder-simple-value-java (value type)
  (if (> (cdr type) 0)
      (let ((topcoder-svj-type (topcoder-element-type type)))
	(concat "new " (topcoder-type-java type) " {"
		(mapconcat
		 (function (lambda (v)
			     (topcoder-simple-value-java
			      v topcoder-svj-type)))
		 value
		 ", ")
		"}"))
    (cond ((eq (car value) 'number)
	   (if (string-equal (car type) "long")
	       (concat (cdr value) "L")
	     (cdr value)))
	  ((eq (car value) 'string)
	   ;; TODO quote the string when it will be unquoted in tokenize
	   (concat "\"" (cdr value) "\"")))))

;; (defun topcoder-test-java (desc test n)
;;   (let ((p (cdr (assoc 'params desc)))
;; 	(a (car test))
;; 	(retval (cdr (assoc 'return desc)))
;; 	(res ""))
;;     (while p
;;       (setq res
;; 	    (concat res
;; 		    (cdr (car p)) " = " 
;; 		    (topcoder-simple-value-java (topcoder-parse-value (car a))
;; 						(caar p))
;; 		    ";\n"))
;;       (setq p (cdr p))
;;       (setq a (cdr a)))
;;     (concat res
;; 	    "res_expected = "
;; 	    (topcoder-simple-value-java (topcoder-parse-value (cdr test))
;; 					retval)
;; 	    ";\n"
;; 	    "_run_test("
;; 	    (mapconcat 'cdr
;; 		       (cdr (assoc 'params desc))
;; 		       ", ")
;; 	    ", res_expected, " (number-to-string n) ");\n")))

(defun topcoder-test-java (desc test n)
  (let ((p (cdr (assoc 'params desc)))
	(a (car test))
	(retval (cdr (assoc 'return desc)))
	(args ""))
    (while p
      (setq args
	    (concat args
		    (topcoder-simple-value-java (topcoder-parse-value (car a))
						(caar p))
		    ", "))
      (setq p (cdr p))
      (setq a (cdr a)))
    (concat "_run_test("
	    args
	    (topcoder-simple-value-java (topcoder-parse-value (cdr test))
					retval)
	    ", " (number-to-string n) ");\n")))

(defun topcoder-tests-cpp (desc)
  (let ((params (cdr (assoc 'params desc)))
	(retval (cdr (assoc 'return desc)))
	(test-cases (cdr (assoc 'test-cases desc))))
    (concat 
     (topcoder-test-function-cpp desc)
     "int main() {\n"
     (mapconcat (function (lambda (p)
			    (concat (topcoder-type-cpp (car p))
				    " " (cdr p) ";")))
		params "\n")
     "\n"
     (topcoder-type-cpp retval) " res_expected;\n"
     (let ((n 0)
	   (s "")
	   (l test-cases))
       (while l
	 (setq s (concat s (topcoder-test-cpp desc (car l) n)))
	 (setq n (1+ n))
	 (setq l (cdr l)))
       s)
     "}\n")))

(defun topcoder-tests-java (desc)
  (let ((params (cdr (assoc 'params desc)))
	(retval (cdr (assoc 'return desc)))
	(test-cases (cdr (assoc 'test-cases desc))))
    (concat 
     (topcoder-test-function-java desc)
     "public static void main(String[] args) {\n"
;;      (mapconcat (function (lambda (p)
;; 			    (concat (topcoder-type-java (car p))
;; 				    " " (cdr p) ";")))
;; 		params "\n")
;;      "\n"
;;      (topcoder-type-java retval) " res_expected;\n"
     (let ((n 0)
	   (s "")
	   (l test-cases))
       (while l
	 (setq s (concat s (topcoder-test-java desc (car l) n)))
	 (setq n (1+ n))
	 (setq l (cdr l)))
       s)
     "}\n")))

(defun topcoder-tests (language desc)
  (cond ((eq language 'cpp)
	 (cons (topcoder-tests-cpp desc) ""))
	((eq language 'java)
	 (cons (topcoder-tests-java desc) ""))
	(t "")))

(defun topcoder-comment (language string)
  (concat "// " string "\n"))

(defun topcoder-insert-template (desc)
  (let* ((language (cdr (assoc 'language desc)))
	 (begin-mark (topcoder-comment language "@begin_tests"))
	 (end-mark (topcoder-comment language "@end_tests"))
	 (test-code (topcoder-tests language desc))
	 (desc1 (nconc
		 (list
		  (cons 'tests
			(concat begin-mark (car test-code) end-mark))
		  (cons 'tests-ext
			(concat begin-mark (cdr test-code) end-mark))
		  (cons 'library
			(concat
			 (topcoder-comment language "@begin_lib")
			 (topcoder-comment language "@end_lib"))))
		 desc)))
    (cond ((eq language 'cpp)
	   (topcoder-parse-template topcoder-template-cpp desc1))
	  ((eq language 'java)
	   (topcoder-parse-template topcoder-template-java desc1))
	  ((eq language 'cs)
	   (topcoder-parse-template topcoder-template-cs desc1))
	  ((eq language 'vb)
	   (topcoder-parse-template topcoder-template-vb desc1)))))

(defun topcoder-get-class-name ()
  (if (boundp 'topcoder-class-name)
      topcoder-class-name
    (if (string-match "^[A-Za-z]+" (buffer-name))
	(match-string 0 (buffer-name))
      "ClassName")))

(defun topcoder-get-language ()
  (if (boundp 'topcoder-language)
      topcoder-language
    (if (string-match "\\.\\([a-z]+\\)$" (buffer-name))
	(let ((ls (match-string 1 (buffer-name))))
	  (if (member ls '("cpp" "java" "cs" "vb"))
	      (intern ls)
	    'cpp))
      'cpp)))

(defun topcoder-set-problem (desc)
  (print desc)
  (let ((language (cdr (assoc 'language desc)))
	(class-name (cdr (assoc 'class-name desc))))
    (find-file (topcoder-source-file-name class-name (symbol-name language)))
    (topcoder-mode t)
    (if (= (buffer-size) 0)
	(topcoder-insert-template desc))
    (set (make-local-variable 'topcoder-class-name) class-name)
    (set (make-local-variable 'topcoder-language) language)
    (set (make-local-variable 'compile-command)
	 (topcoder-compile-command))
    (with-current-buffer (get-buffer-create (concat "*" class-name "*"))
      (buffer-disable-undo)
      (erase-buffer)
      (insert (cdr (assoc 'statement desc)))
      (buffer-enable-undo)
      (w3m-region (point-min) (point-max))
      (display-buffer (current-buffer) t))
    (run-hooks 'topcoder-set-problem-hook))
  "ok")

(defun topcoder-postprocess (source)
  (while (string-match "^.*@begin_tests\\(.\\|\n\\)*@end_tests.*$" source)
    (setq source (replace-match "" t t source)))
  (if (string-match "^.*@begin_lib.*\n" source)
      (setq source (replace-match "" t t source)))
  (if (string-match "^.*@end_lib.*\n" source)
      (setq source (replace-match "" t t source)))
  source)

(defun topcoder-compile-command ()
  (let ((language (topcoder-get-language)))
    (cond ((eq language 'java)
	   (concat "javac " (buffer-file-name)))
	  (t (concat "g++ " topcoder-cpp-compile-flags
		     " -o " (topcoder-get-class-name)
		     " " (buffer-file-name))))))

(defun topcoder-compile ()
  (interactive)
  (compile (topcoder-compile-command)))

(defun topcoder-import-library ()
  (interactive)
  (save-excursion
    (let* ((language (topcoder-get-language))
	   (lib (cond ((eq language 'cpp)
		       topcoder-code-library-cpp)
		      ((eq language 'java)
		       topcoder-code-library-java)
		      ((eq language 'vb)
		       topcoder-code-library-vb)
		      ((eq language 'cs)
		       topcoder-code-library-cs)
		      (t nil)))
	   (keywords (mapcar 'car lib)))
      (goto-char (point-min))
      (when (re-search-forward "^.*@begin_lib\\(.\\|\n\\)*@end_lib.*\n" nil t)
	(replace-match "// @begin_lib\n// @end_lib\n" t t)
	(while
	    (progn
	      (goto-char (point-min))
	      (let ((case-fold-search nil))
		(re-search-forward (concat "\\<\\("
					   (regexp-opt keywords) "\\)\\>")
				   nil t)))
	  (let ((str (match-string 0)))
	    (goto-char (point-min))
	    (search-forward "@begin_lib" nil t)
	    (insert "\n" (cdr (assoc str lib)))
	    (setq keywords (remove str keywords))))))))

(defun topcoder-get-source (class-name language)
  (save-excursion
    (find-file (topcoder-source-file-name class-name (symbol-name language)))
    (topcoder-postprocess (buffer-string))))

(defun topcoder-set-source (class-name language source)
;; TODO
  "ok")

(defun topcoder-run-process (program)
  (let* ((process
	 (start-process-shell-command
	  "TopCoderTest" "*TopCoder Test*" program))
	 (buffer (process-buffer process)))
    (save-excursion
      (set-buffer buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (buffer-enable-undo)
      (topcoder-test-mode)
      (insert (concat "Running " program "\n"))
      (set-marker (process-mark process) (point)))
    (display-buffer buffer t nil)))

(defun topcoder-test-program ()
  (interactive)
  (let* ((language (topcoder-get-language))
	 (command
	  (cond ((eq language 'java)
		 (concat "java " (topcoder-get-class-name)))
		(t (concat "./" (topcoder-get-class-name))))))
    (topcoder-run-process command)))

(defvar topcoder-mode-map
  (easy-mmode-define-keymap
   '(("\C-c\C-i" . topcoder-import-library)
     ("\C-c\C-c" . topcoder-compile)
     ("\C-c\C-t" . topcoder-test-program))))

(define-minor-mode topcoder-mode
  "Toggle TopCoder mode."
  nil
  " TopCoder"
  topcoder-mode-map)

(defface topcoder-test-passed-face
  '((t (:foreground "blue")))
  "Header of a passed test result in TopCoder Test mode"
  :group 'topcoder)

(defface topcoder-test-failed-face
  '((t (:weight bold :foreground "red")))
  "Header of a failed test result in TopCoder Test mode"
  :group 'topcoder)

(defconst topcoder-font-lock-keywords
  '(("^--- test [0-9]+: ok ---$" . 'topcoder-test-passed-face)
    ("^--- test [0-9]+: failed ---$" . 'topcoder-test-failed-face)))

(defun topcoder-test-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "TopCoder Test")
  (setq font-lock-defaults
	'(topcoder-font-lock-keywords nil nil nil nil)))

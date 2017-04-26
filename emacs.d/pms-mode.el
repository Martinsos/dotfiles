;;;;;;;;; PMS (PhotoMathSolver) mode.

;; Define several categories of keywords.
(setq pms-keywords '("procedure" "list" "function" "search" "functor" "IN" "OUT" "IN_FILTER"
                     "SEARCH" "MASK" "for" "break" "return" "proxy" "keep" "tags" "->" "flag_subcall"))
(setq pms-builtins '("N" "N1" "N2" "N3" "N4" "Z" "Z1" "Z2" "Z3" "Z4" "R" "R1" "R2" "R3" "R4" "P" "P1" "P2"
                     "P3" "P4" "D" "D1" "D2" "D3" "D4" "F" "F1" "F2" "F3" "F4" "M" "M1" "M2" "M3" "M4" "FN"
                     "FN1" "FN2" "FN3" "FN4" "var" "positive" "neg" "add" "sub" "div" "mul" "muli" "bracket"
                     "deg" "periodic" "sin" "cos" "tan" "cot" "asin" "acos" "atan" "acot" "sinh" "cosh" "tanh"
                     "coth" "asinh" "acosh" "atanh" "acoth" "sec" "csc" "eq" "lt" "lte" "gt" "gte" "system"
                     "abs" "root" "root2" "log" "log10" "ln" "pow" "exp" "factorial" "sign" "integral"
                     "definiteintegral" "derivation" "true" "false" "add_sub" "add_sub_sign" "set" "elem_of"
                     "elem_not_of" "union" "intsec" "diff" "ooint" "coint" "ocint" "ccint" "list" "vert_list"
                     "neq" "order" "alt_form" "vert_flat" "int" "float" "approx" "big_int" "const_var" "lcm"
                     "gcd" "mod" "max" "min" "seach_block" "inverse" "sanitize_block" "error" "args" "call"
                     "hd_del" "fr_one" "delete" "hidden" "shell" "sbracket" "sum" "mul" "frac" "mfrac" "pow"
                     "system" "vert_list" "vert_flat" "foobar" "preorder" "root" "sum" "mul" "multisum"
                     "multisum_root" "multisum_tail" "multimul" "multimul_root" "frac_chain" "frac_root"
                     "mul_pair" "root_addend" "root_mul" "rel_add_mull" "rel_restrictions" "vlist"
                     "non_frac_mul"))

;; Generate regex string for each category of keywords.
(setq pms-keywords-regexp (regexp-opt pms-keywords 'symbols))
(setq pms-builtins-regexp (regexp-opt pms-builtins 'symbols))

;; Create the list for font-lock.
;; Each category of keyword is given a particular face.
(setq pms-font-lock-keywords
      `(
        (,pms-builtins-regexp . font-lock-builtin-face)
        (,pms-keywords-regexp . font-lock-keyword-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))

;;
(define-derived-mode pms-mode prog-mode "pms mode"
  "Major mode for editing PMS (PhotoMathSolver) language."
  ;; code for syntax highlighting
  (progn
    ;; Use above defined coloring.
    (setq font-lock-defaults '((pms-font-lock-keywords)))
    ;; Recognize // as line comment.
    (modify-syntax-entry ?\/ ". 12b")
    (modify-syntax-entry ?\n "> b")
  ))

;; Clear memory.
(setq pms-keywords nil)
(setq pms-builtins nil)
(setq pms-keywords-regexp nil)
(setq pms-builtins-regexp nil)

;; add the mode to the `features' list
(provide 'pms-mode)

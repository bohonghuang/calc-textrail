;;; calc-textrail.el --- calc-trail-buffer to Tex converter -*- lexical-binding: t -*-

;;; Commentary:

;; This package can convert the content in calc-trail-buffer to Tex code and preview it automatically.

;;; Code:
(require 'parsec)
(require 'dash)
(require 'latex-math-preview)
(require 'calc)

(defcustom calc-textrail-result-display-method 'equal
  "The method used for results display.")

(defvar calc-textrail-redo-stack '()
  "")

(defun calc-textrail-parsec-whitespace ()
  (parsec-many (parsec-ch ? )))

(defun calc-textrail-parsec-comma ()
  (parsec-ch ?,)
  (calc-textrail-parsec-whitespace))

(defun calc-textrail-parsec-real-number ()
  (string-to-number (parsec-re "-\\{0,1\\}[0-9]+\\.\\{0,1\\}[0-9]+\\{0,1\\}")))

(defun calc-textrail-parsec-complex-number ()
  (parsec-between (parsec-ch ?\() (parsec-ch ?\)) (cons (calc-textrail-parsec-number) (parsec-and (calc-textrail-parsec-comma) (calc-textrail-parsec-whitespace) (calc-textrail-parsec-number)))))

(setq calc-textrail-operator-alist
      `(("*" . "$1 \\times $2")
        ("/" . "\\frac{$1}{$2}")
        ("+" . "$1 + $2")
        ("-" . "$1 - $2")
        ("sin" . "\\sin{$1}")
        ("cos" . "\\cos{$1}")
        ("tan" . "\\tan{$1}")
        ("()" . "$1 + $2j")
        ("sqrt" . "\\sqrt{$1}")
        ("^" . "{$1}^{$2}")
        ("asin" . "\\arcsin{$1}")
        ("acos" . "\\arccos{$1}")
        ("atan" . "\\arctan{$1}")
        ("abs" . "\\left|$1\\right|")
        ("_" . "-$1")
        ("frac" . "$1")
        ("flt" . "$1")
        ("ln" . "\\ln{$1}")
        ("log" . "\\log_{$2}{$1}")
        ("pi" . "\\pi")
        ("im" . 1)
        ("re" . 1)
        ("max" . "\\max\\left($1,$2\\right)")
        ("min" . "\\min\\left($1,$2\\right)")
        ("inv" . "\\frac{1}{$1}")))

(setq calc-textrail-operator-priority-alist
      `(("^" . (2 . 1))
        ("/" . (,most-negative-fixnum . ,most-positive-fixnum))
        ("sqrt" . (,most-negative-fixnum . 1))
        ("abs" . nil)
        ("*" . -1)
        ("+" . -2)
        ("-" . -2)
        ("_" . ,most-negative-fixnum)
        ("()" . ,most-negative-fixnum)
        ("frac" . (,most-negative-fixnum . ,most-positive-fixnum))
        ("flt" . (,most-negative-fixnum . ,most-positive-fixnum))))

(defun calc-textrail-operator-get-priority-out (operator)
  (pcase (assoc operator calc-textrail-operator-priority-alist)
    (`nil 0)
    (`(,_ . `nil) most-positive-fixnum)
    (`(,_ . (,prior-in . ,prior-out)) prior-out)
    (`(,_ . ,prior) prior)))

(defun calc-textrail-operator-get-priority-in (operator n)
  (pcase (assoc operator calc-textrail-operator-priority-alist)
    (`nil 0)
    (`(,_ . nil) most-negative-fixnum)
    (`(,_ . (,prior-in . ,prior-out))
     (pcase prior-in
       (`nil 0)
       ((pred numberp) prior-in)
       ((pred listp) (nth n prior-in))))
    (`(,_ . ,prior) prior)))

(defun calc-textrail-parsec-operator-arg ()
  (parsec-ch ?$)
  (string-to-number (parsec-re "[0-9]+")))

(defun calc-textrail-parsec-operator-arg-max ()
  (let ((max-number 0))
    (parsec-many (parsec-none-of ?$))
    (parsec-endby (setq max-number (max max-number (calc-textrail-parsec-operator-arg))) (parsec-many (parsec-none-of ?$)))
    max-number))

(defun calc-textrail-get-operator-consumption (operator)
  (pcase (cdr (assoc operator calc-textrail-operator-alist))
    ((and x (pred integerp)) x)
    ((and x (pred stringp)) (parsec-with-input x (calc-textrail-parsec-operator-arg-max)))
    (x (error "Undefined operator: %s" operator))))

(defun calc-textrail-parsec-number ()
  (parsec-or (parsec-try (calc-textrail-parsec-complex-number)) (calc-textrail-parsec-real-number)))

(defun calc-textrail-operator ()
  (parsec-re "[^0-9][^[:space:]]*"))

(defun calc-textrail-element-to-tree (number)
  (pcase number
    (`(,real . ,img) `(("()" . (,(calc-textrail-element-to-tree real) ,(calc-textrail-element-to-tree img))) . ,number))
   (real (if (>= real 0) real `(("_" . (,(- real))) . ,number)))))

(defun calc-textrail-parse-trees (trail)
  (let ((stack '()))
    (parsec-with-input trail
      (while (not (parsec-peek-p (parsec-eof)))
        (pcase (parsec-or (parsec-try (parsec-and (calc-textrail-parsec-whitespace) (parsec-return (calc-textrail-parsec-number) (parsec-until (parsec-or (parsec-eol-or-eof))))))
                          (parsec-return (parsec-and (calc-textrail-parsec-whitespace) (cons (parsec-return (calc-textrail-operator) (calc-textrail-parsec-whitespace)) (parsec-option nil (calc-textrail-parsec-number)))) (parsec-until (parsec-eol-or-eof))))
          ((and x (or (pred numberp) (and `(,real . ,img) (guard (and (numberp real) (numberp img))))))
           (push (calc-textrail-element-to-tree x) stack))
          (`("pop" . nil) (pop stack))
          (`("push" . nil) (push (car stack) stack))
          (`("cln" . ,_))
          (`(,op . ,res) (let ((args '()))
                                   (dotimes (_ (calc-textrail-get-operator-consumption op))
                                     (push (pop stack) args))
                                   (push `((,op . ,args) . ,res) stack)))
          (x (error "Unexpected input: %s" x)))))
    stack))

(defun calc-textrail-tree-to-string (tree &optional show-result paren-required)
  (pcase tree
    (`((,op . ,args) . ,res)
     (let* ((op-tex (pcase (cdr (assoc op calc-textrail-operator-alist))
                      ((and x (pred integerp))
                       (concat  "\\operatorname{" op "}" (-reduce #'concat (--map (concat "{$" (number-to-string it) "}") (number-sequence 1 x)))))
                      (x x)))
            (form (-reduce-from (pcase-lambda (acc `(,i . ,x))
                                  (replace-regexp-in-string
                                   (regexp-quote (format "$%d" (+ i 1)))
                                   (pcase x
                                     (`((,op-child . ,args-child) . ,res-child)
                                      (let* ((op-prior (calc-textrail-operator-get-priority-in op i))
                                             (op-child-prior (calc-textrail-operator-get-priority-out op-child)))
                                        (calc-textrail-tree-to-string x
                                                                      (pcase show-result
                                                                        ((and x `underbrace) x)
                                                                        (x nil))
                                                                      (> op-prior op-child-prior))))
                                     (x (calc-textrail-tree-to-string x)))
                                   acc
                                   nil
                                   'literal))
                                op-tex
                                (--map-indexed `(,it-index . ,it) args)))
            (form (if paren-required (concat "\\left(" form "\\right)") form)))
       (pcase show-result
         (`nil form)
         (`underbrace (format "\\underbrace{%s}_{%s}" form (calc-textrail-tree-to-string res)))
         (`equal (format "%s=%s" form (calc-textrail-tree-to-string res))))))
    ((pred numberp) (number-to-string tree))
    (x (let ((form (calc-textrail-tree-to-string (calc-textrail-element-to-tree x))))
         (if paren-required (concat "\\left(" form "\\right)") form)))))

(defun calc-textrail-enter-handler ())

(defun calc-textrail-convert-string (trail)
  (concat "\\begin{aligned}" (--reduce (concat acc "\\\\" it) (--map (replace-regexp-in-string "=" "&=" (calc-textrail-tree-to-string it calc-textrail-result-display-method)) (calc-textrail-parse-trees trail))) "\\end{aligned}"))

(defun calc-textrail-preview ()
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer (calc-trail-buffer)
      (let ((buffer-string (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (insert "$$" (calc-textrail-convert-string buffer-string) "$$")
          (backward-char 3)
          (condition-case nil
              (progn (latex-math-preview-expression))
            (error (latex-math-preview-expression))))
        (switch-to-buffer-other-window buffer)))))

(defvar calc-textrail-auto-preview-timer nil
  "Idle timer for Tex auto preview.")

(defvar calc-textrail-auto-preview-delay 0.5
  "Time delay for Tex auto preview.")

(defun calc-trail-pop (&optional n)
  (let ((n (or n 1))
        (stack '()))
    (with-current-buffer (calc-trail-buffer)
      (read-only-mode -1)
      (dotimes (i n)
        (end-of-buffer)
        (backward-delete-char 1)
        (let* ((end (point))
              (start (progn (beginning-of-line) (point))))
          (push (buffer-substring-no-properties start end) stack)
          (delete-region start end)))
      (read-only-mode +1)
      stack)))

(defun calc-trail-push (elems)
  (with-current-buffer (calc-trail-buffer)
    (read-only-mode -1)
    (-each elems (lambda (elem)
                   (end-of-buffer)
                   (insert elem "\n")))
    (read-only-mode +1)
    (length elems)))

(defun calc-textrail-stack-changed ()
  (when calc-textrail-auto-preview-timer (cancel-timer calc-textrail-auto-preview-timer))
  (setq calc-textrail-auto-preview-timer (run-with-idle-timer calc-textrail-auto-preview-delay nil (lambda () (calc-textrail-preview) (setq calc-textrail-auto-preview-timer nil)))))

(defun calc-common-command-handler (&rest _)
  (calc-textrail-stack-changed)
  (setq calc-textrail-redo-stack nil))

(defun calc-textrail-pop-handler (&rest _)
  (calc-trail-push '(" pop"))
  (calc-common-command-handler))

(defun calc-textrail-push-handler (&rest _)
  (calc-trail-push '("push"))
  (calc-common-command-handler))

(defun calc-textrail-undo-handler (&rest _)
  (--each (calc-trail-pop) (push it calc-textrail-redo-stack))
  (calc-textrail-stack-changed))

(defun calc-textrail-redo-handler (&rest _)
  (calc-trail-push (list (pop calc-textrail-redo-stack)))
  (calc-textrail-stack-changed))

(define-minor-mode calc-textrail-mode
  "Minor mode for auto refresh calc-trail Tex preview."
  :init-value nil
  (if calc-textrail-mode
      (progn (advice-add #'calc-enter-result :after #'calc-common-command-handler)
             (advice-add #'calcDigit-nondigit :before #'calc-common-command-handler)
             (advice-add #'calc-pop :after #'calc-textrail-pop-handler)
             (advice-add #'calc-enter :after #'calc-textrail-push-handler)
             (advice-add #'calc-undo :after #'calc-textrail-undo-handler)
             (advice-add #'calc-redo :after #'calc-textrail-redo-handler))
    (advice-remove #'calc-enter-result #'calc-common-command-handler)
    (advice-remove #'calcDigit-nondigit #'calc-common-command-handler)
    (advice-remove #'calc-pop #'calc-textrail-pop-handler)
    (advice-remove #'calc-enter #'calc-textrail-push-handler)
    (advice-remove #'calc-undo #'calc-textrail-undo-handler)
    (advice-remove #'calc-redo #'calc-textrail-redo-handler)))

(define-key calc-mode-map (kbd "<f5>") #'calc-textrail-preview)

(provide 'calc-textrail)


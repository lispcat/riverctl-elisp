;;; package --- An elisp library to configure RiverWM.

;;; Commentary:

;;; Code:

(require 'cl-macs)
(require 'subr-x)

(defvar riverctl-logfile "/tmp/riverctl.log"
  "File to log all riverctl commands to.")

(defun riverctl (&rest tree)
  "The main riverctl function. Parse TREE and evaluate."
  (declare (indent defun))
  (let* ((parsed-lists (riverctl-parse-tree tree))
         (fixed-lst (mapcar #'riverctl-fix-kbd parsed-lists))
         (result (mapconcat #'riverctl-run fixed-lst "\n")))
    (concat "\n" result "\n")))

(defun riverctl-parse-tree (tree)
  "Parse a nested TREE into sublists by traversing every path."
  (let ((result '()))
    (cl-labels
        ((fn-traverse (node path)
           ;; current list has more than one item (not the end of branch)
           (dolist (item node)
             (cond ((listp item)
                    ;; case: item is list, recurse and add to path
                    (fn-traverse item path))
                   (t
                    ;; case: item is atom, add to path
                    (setq path (append path (list item)))
                    (when (equal item (car (last node)))
                      ;; case: item is end of list, end of branch
                      (setq result (append result (list path)))))))))
      (fn-traverse tree '())
      result)))

(defun riverctl-fix-kbd--replace (m)
  "Convert modifier symbol M to River modifier representation.
Used in `riverctl-fix-kbd--replace'."
  (pcase m
    ("s" "Super")
    ("M" "Alt")
    ("S" "Shift")
    ("C" "Control")
    (_ m)))

(defun riverctl-fix-kbd (lst)
  "Replace every \"kbd ___\" in LST with appropriate riverctl keybind.
The string following \"kbd\" should be in Emacs kbd format.

Note that some special keys are not equivalent to Emacs' kbd representation.
For example:
- Emacs: \"<up>\"
- River: \"Up\"

Refer to here for all the special key names:
https://codeberg.org/ifreund/zig-xkbcommon/src/branch/master/src/xkbcommon_keysyms.zig"
  (let (ret)
    (while lst
      (let ((top (pop lst)))
        (if (not (eq top 'kbd))
            (setq ret (append ret (list top)))
          (let* ((strkeys (pop lst))
                 (strkeys-lst (split-string strkeys "-"))
                 (strkeys-lst
                  (if (eq (length strkeys-lst) 1)
                      (append '("None") strkeys-lst)
                    strkeys-lst))
                 (strkeys-conv
                  (append (mapcar #'riverctl-fix-kbd--replace (butlast strkeys-lst))
                          (last strkeys-lst)))
                 (strkeys-joined
                  (append (list (mapconcat #'identity (butlast strkeys-conv) "+"))
                          (last strkeys-conv)))
                 (mod (make-symbol (nth 0 strkeys-joined)))
                 (key (make-symbol (nth 1 strkeys-joined))))
            (setq ret (append ret (list mod key)))))))
    ret))

(defun riverctl-run (lst)
  "Convert LST to a formatted string, then executed with `riverctl-run-shell'."
  (let ((cmd
         (concat "riverctl "
                 (mapconcat (lambda (arg) (format "%S" arg))
                            lst
                            " "))))
    (riverctl-run-shell cmd)))

(defun riverctl-run-shell (str)
  "Run the command STR and log to `riverctl-logfile'."
  (let* ((mesg (format "$ %s" str))
         (runcmd (format "echo '%s' ; %s 2>&1 | tee -a %s"
                         mesg str riverctl-logfile)))
    (with-temp-buffer
      (call-process-shell-command runcmd nil (current-buffer))
      (string-chop-newline
       (buffer-string)))))

;; TODO: consider switching to cons instead of append?
(defun riverctl-prepend (sequence body)
  "Prepend every element in SEQUENCE with BODY using `mapcar'."
  (declare (indent defun))
  (mapcar (lambda (e)
            (append (list e) body))
          sequence))


(provide 'riverctl)
;;; riverctl.el ends here

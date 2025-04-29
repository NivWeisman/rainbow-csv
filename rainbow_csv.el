;;; rainbow-csv.el --- Rainbow CSV column highlighting -*- lexical-binding: t; -*-

;; Author: Niv Weisman (nivweisman@gmail.com)
;; Version: 1.0
;; Keywords: csv, highlighting, colors
;; URL: 
;; Package-Requires: ((emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This Emacs Lisp script provides dynamic column highlighting for CSV files,
;; similar to the Rainbow CSV extension for VS Code. It highlights each CSV
;; field with a different foreground color. The colors can be toggled between
;; original colors and a set of colors that are 30% lighter than the originals.
;;
;; The parser in this implementation is robust enough to handle quoted fields
;; and escaped quotes so that delimiters within quotes do not break field splitting.
;;
;; To use, load this file and the mode will be automatically activated for
;; buffers in `csv-mode` if added to the hook. You can also manually enable
;; the highlighting using `M-x rainbow-csv-enable`.
;;
;;; License:
;;
;; This software is released under the MIT License.
;;
;;; Code:

;; Define a custom group for customization.
(defgroup rainbow-csv nil
  "Rainbow CSV highlighting for CSV fields."
  :group 'csv)

;; Custom variable to choose between lighter and original colors.
(defcustom rainbow-csv-use-lighter-colors t
  "If non-nil, use a set of colors that are 30% lighter than the originals for CSV highlighting.
If nil, use the original colors."
  :type 'boolean
  :group 'rainbow-csv)

;; List of original colors for CSV highlighting.
(defcustom rainbow-csv-original-colors
  '("#FF0000" "#008800" "#0000FF" "#DD7700" "#990099" "#009999")
  "List of original colors for CSV highlighting."
  :type '(repeat color)
  :group 'rainbow-csv)

;; List of lighter colors for CSV highlighting (30% lighter than original).
(defcustom rainbow-csv-lighter-colors
  '("#FF4D4D" "#4DAC4D" "#4D4DFF" "#E7A04D" "#B84DB8" "#4DB8B8")
  "List of 30% lighter colors for CSV highlighting."
  :type '(repeat color)
  :group 'rainbow-csv)

;; Variable to hold the list of generated faces.
(defvar rainbow-csv--faces nil
  "List of faces generated for CSV column highlighting based on the current color set.")

;;;###autoload
(defun rainbow-csv--generate-faces ()
  "Generate and store a list of faces for CSV column highlighting.
If `rainbow-csv-use-lighter-colors` is non-nil, use `rainbow-csv-lighter-colors`;
otherwise, use `rainbow-csv-original-colors`. Each generated face sets its foreground to the chosen color."
  (let ((colors (if rainbow-csv-use-lighter-colors
                    rainbow-csv-lighter-colors
                  rainbow-csv-original-colors)))
    (setq rainbow-csv--faces
          (mapcar (lambda (color)
                    ;; Generate a new face named "rainbow-csv-face-<COLOR>".
                    (let ((face (make-face (intern (format "rainbow-csv-face-%s" (replace-regexp-in-string "#" "" color))))))
                      ;; Set the foreground of the face to the color.
                      (set-face-foreground face color)
                      face))
                  colors))))

;; Immediately generate the faces upon loading the file.
(rainbow-csv--generate-faces)

;;;###autoload
(defun rainbow-csv--clear-overlays (start end)
  "Remove all Rainbow CSV overlays between START and END.
This function searches for overlays that have the property `rainbow-csv-overlay`
and removes them to ensure that old highlighting is cleared before reapplying."
  (remove-overlays start end 'rainbow-csv-overlay t))

;;;###autoload
(defun rainbow-csv--parse-fields (line-start line-end)
  "Parse CSV fields from LINE-START to LINE-END and return a list of field positions.
Each element in the returned list is a cons cell (FIELD-BEGIN . FIELD-END).
This function properly handles fields that are enclosed in double quotes,
including escaped quotes (i.e. double double-quotes), so that commas inside quotes
are not treated as field delimiters."
  (let ((fields '())
        (pos line-start))
    (while (< pos line-end)
      (let ((field-begin pos)
            field-end)
        (if (eq (char-after pos) ?\")
            ;; Handle quoted field.
            (progn
              ;; Skip the opening quote.
              (setq pos (1+ pos))
              (catch 'quote-end
                (while (< pos line-end)
                  (if (eq (char-after pos) ?\")
                      (if (and (< (1+ pos) line-end)
                               (eq (char-after (1+ pos)) ?\"))
                          ;; Found escaped quote, skip both quotes.
                          (setq pos (+ pos 2))
                        ;; Found the closing quote.
                        (progn
                          (setq pos (1+ pos))
                          (throw 'quote-end nil)))
                    (setq pos (1+ pos)))))
              (setq field-end pos)
              ;; After closing quote, if a comma is present, skip it.
              (when (and (< pos line-end) (eq (char-after pos) ?,))
                (setq pos (1+ pos))))
          ;; Handle unquoted field.
          (while (and (< pos line-end) (not (eq (char-after pos) ?,)))
            (setq pos (1+ pos)))
          (setq field-end pos)
          ;; Skip comma after field, if present.
          (when (and (< pos line-end) (eq (char-after pos) ?,))
            (setq pos (1+ pos))))
        (push (cons field-begin field-end) fields)))
    (nreverse fields)))

;;;###autoload
(defun rainbow-csv--jit-lock (start end)
  "Apply Rainbow CSV highlighting between START and END.
This function is designed to work with Emacs’s JIT-lock mechanism.
It processes each line in the region, clears any existing overlays,
parses the line into CSV fields using `rainbow-csv--parse-fields`,
and then applies overlays with a cycling foreground color for each field."
  (save-excursion
    (goto-char start)
    ;; Process each line between START and END.
    (while (< (point) end)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Clear existing rainbow-csv overlays in this line.
        (rainbow-csv--clear-overlays line-start line-end)
        ;; Parse the CSV fields.
        (let ((fields (rainbow-csv--parse-fields line-start line-end))
              (col 0))
          (dolist (cell fields)
            (let* ((face (nth (mod col (length rainbow-csv--faces))
                              rainbow-csv--faces))
                   (ov (make-overlay (car cell) (cdr cell))))
              ;; Set the overlay's face and mark it as a rainbow csv overlay.
              (overlay-put ov 'face face)
              (overlay-put ov 'rainbow-csv-overlay t)
              (setq col (1+ col)))))
        (forward-line 1)))))

;;;###autoload
(defun rainbow-csv-enable ()
  "Enable Rainbow CSV column highlighting in the current buffer.
Registers `rainbow-csv--jit-lock` with the JIT-lock system so that
CSV fields are dynamically highlighted with their designated colors."
  (jit-lock-register #'rainbow-csv--jit-lock))

;;;###autoload
(defun rainbow-csv-disable ()
  "Disable Rainbow CSV column highlighting in the current buffer.
Unregisters `rainbow-csv--jit-lock` from JIT-lock and removes all
Rainbow CSV overlays from the buffer."
  (jit-lock-unregister #'rainbow-csv--jit-lock)
  (rainbow-csv--clear-overlays (point-min) (point-max)))

;; Optionally, add Rainbow CSV highlighting to `csv-mode` automatically.
(add-hook 'csv-mode-hook 'rainbow-csv-enable)

(provide 'rainbow-csv)

;;; rainbow-csv.el ends here


(in-package #:autolith)

(-> layout-column-widths (list integer &key (:gap-width integer) (:minimum-widths (option list)) (:fill-p boolean)) list)
(defun layout-column-widths (rows total-width &key (gap-width 1) minimum-widths fill-p)
  "Allocate table cells using cl-termdown's shared column layout."
  (termdown:column-widths rows total-width :gap-width gap-width
                          :minimum-widths minimum-widths :fill-p fill-p))

(-> layout-fit-text (string integer &key (:alignment (member :left :right))) string)
(defun layout-fit-text (text width &key (alignment ':left))
  "Fit one table cell through cl-termdown."
  (termdown:fit-text text width :alignment alignment))

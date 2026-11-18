(in-package #:autolith)

;;;; -- Context Inspection --

(-> application-context-entry (application) list)
(defun application-context-entry (application)
  "Return aligned, styled contributor and payload-free delivery diagnostics."
  (let ((registrations (context-contributor-registrations)))
    (multiple-value-bind (identifier delivery)
        (context--diagnostic-delivery (application-conversation application))
      (append
       (list (terminal-span ':brand (format nil "# context contributors~%")))
       (if registrations
           (apply #'append
                  (application-context--table-rows
                   '("contributor" "source" "function")
                   (mapcar
                    (lambda (registration)
                      (list (getf registration :identifier)
                            (format nil "~(~A~)" (getf registration :source))
                            (context--function-label
                             (or (getf registration :definition-name)
                                 (getf registration :function)))))
                    registrations)))
           (list (terminal-span ':dim (format nil "  none~%"))))
       (list (terminal-span ':brand (format nil "~%# request-local context"))
             (terminal-span ':dim
                            (format nil "~@[ · conversation ~A~]~%"
                                    (and identifier (conversation-identifier-display identifier)))))
       (cond
         ((null delivery)
          (list (terminal-span ':dim "  none assembled")))
         ((not (or (context-delivery-diagnostic-contributions delivery)
                   (context-delivery-diagnostic-omitted delivery)
                   (context-delivery-diagnostic-failures delivery)))
          (list (terminal-span ':dim "  none active")))
         (t
          (append
           (application-context--contribution-rows
            "active" (context-delivery-diagnostic-contributions delivery))
           (application-context--contribution-rows
            "omitted by budget" (context-delivery-diagnostic-omitted delivery))
           (when (context-delivery-diagnostic-failures delivery)
             (append
              (list (terminal-span ':failure (format nil "~%contributor failures~%")))
              (apply #'append
                     (application-context--table-rows
                      '("contributor" "failure")
                      (mapcar (lambda (failure)
                                (list (first failure) (format nil "~A" (rest failure))))
                              (context-delivery-diagnostic-failures delivery)))))))))))))

(-> application-context--contribution-rows (string list) list)
(defun application-context--contribution-rows (heading contributions)
  "Return a size table and compact policy details for CONTRIBUTIONS under HEADING."
  (when contributions
    (let ((rows
            (application-context--table-rows
             '("contribution" "tokens ≈" "instruction chars" "evidence chars")
             (mapcar
              (lambda (contribution)
                (list (context-contribution-diagnostic-identifier contribution)
                      (context-contribution-diagnostic-token-estimate contribution)
                      (context-contribution-diagnostic-instruction-character-count contribution)
                      (context-contribution-diagnostic-evidence-character-count contribution)))
              contributions))))
      (append
       (list (terminal-span ':notice
                            (format nil "~%~A · ~D estimated tokens~%" heading
                                    (reduce #'+ contributions
                                            :key #'context-contribution-diagnostic-token-estimate))))
       (first rows)
       (loop for contribution in contributions
             for row in (rest rows)
             append
             (append row
                     (list
                      (terminal-span
                       ':dim
                       (format nil "    ~(~A~) · ~(~A~) · ~(~A~) · priority ~D · contributor ~A~%"
                               (context-contribution-diagnostic-source contribution)
                               (context-contribution-diagnostic-class contribution)
                               (context-contribution-diagnostic-lifetime contribution)
                               (context-contribution-diagnostic-priority contribution)
                               (context-contribution-diagnostic-contributor contribution))))))))))

(-> application-context--table-rows (list list) list)
(defun application-context--table-rows (headings rows)
  "Return styled table rows padded by visible cell width, with right-aligned numbers."
  (let* ((values (cons headings rows))
         (text-rows
           (mapcar (lambda (row)
                     (mapcar (lambda (cell)
                               (sanitize-text (princ-to-string cell) :single-line-p t))
                             row))
                   values))
         (widths
           (loop for column below (length headings)
                 collect (loop for row in text-rows
                               maximize (clinedi:text-cell-width (nth column row))))))
    (loop for row in values
          for texts in text-rows
          for header-p = t then nil
          collect
          (cons (terminal-span ':plain "  ")
           (loop for cell in row
                 for text in texts
                 for tail on texts
                 for width in widths
                 for column from 0
                 for padding = (make-string (- width (clinedi:text-cell-width text))
                                            :initial-element #\Space)
                 append
                 (list
                  (terminal-span
                   (cond
                     (header-p
                      ':dim)
                     ((zerop column)
                      ':code)
                     (t
                      ':plain))
                   (if (numberp cell)
                       (concatenate 'string padding text)
                       (concatenate 'string text (if (rest tail) padding ""))))
                  (terminal-span ':plain (if (rest tail) "  " (string #\Newline)))))))))

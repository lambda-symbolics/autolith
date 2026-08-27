(in-package #:autolith)

;;;; -- Autolith Skill Policy --

(defparameter *skill-selection-character-limit* (* 128 1024)
  "The maximum selected skill instruction characters injected in one request.")

(defparameter *skill-selection-count-limit* 32
  "The maximum distinct skills selected during one logical user turn.")

(defparameter *skill-status-entry-limit* 100
  "The maximum skills or diagnostics rendered by one /skills invocation.")

(defparameter *skill-warning-character-limit* 1000
  "The maximum characters in one request-local skill warning.")

(defparameter *skill-warning-aggregate-character-limit* 4000
  "The maximum selected-skill warning characters injected in one request.")

(defvar *skill-logical-turn-active-p* nil
  "True while one logical user turn accepts explicit skill selections.")

(defvar *skill-logical-turn-selection-names* nil
  "Exact skill names selected during the current logical user turn.")

(define-condition skill-selection-error (autolith-error)
  ((name
    :initarg :name
    :reader skill-selection-error-name
    :type string
    :documentation "The exact case-sensitive skill name that was requested.")
   (reason
    :initarg :reason
    :reader skill-selection-error-reason
    :type (member :inactive-turn :unknown-skill :selection-limit)
    :documentation "The machine-readable reason selection could not proceed."))
  (:documentation "A skill could not be selected for the active logical turn."))

(defmethod tool-failure-code ((condition skill-selection-error))
  "Expose the structured selection reason as the tool failure code."
  (skill-selection-error-reason condition))

;;;; -- Logical-Turn Selection --

(-> call-with-skill-logical-turn (user-message-input function) t)
(defun call-with-skill-logical-turn (input function)
  "Call FUNCTION with an empty, dynamically scoped skill selection."
  (declare (ignore input))
  (let ((*skill-logical-turn-active-p* t)
        (*skill-logical-turn-selection-names* nil))
    (funcall function)))

(-> skill--logical-turn-record (skill-metadata) boolean)
(defun skill--logical-turn-record (metadata)
  "Record exact skill NAME from METADATA in the active logical turn.

Return true only when its name was newly added. Signal SKILL-SELECTION-ERROR
when there is no active logical turn in which request-local selection can
survive."
  (unless *skill-logical-turn-active-p*
    (error 'skill-selection-error
           :message
           "A skill can be selected only while an agent turn is active."
           :name (skill-metadata-name metadata)
           :reason ':inactive-turn))
  (let ((name (skill-metadata-name metadata)))
    (if (member name *skill-logical-turn-selection-names* :test #'string=)
        nil
        (progn
          (when (>= (length *skill-logical-turn-selection-names*)
                    *skill-selection-count-limit*)
            (error 'skill-selection-error
                   :message
                   (format nil
                           "A logical turn may select at most ~D distinct skills."
                           *skill-selection-count-limit*)
                   :name name
                   :reason ':selection-limit))
          (setf *skill-logical-turn-selection-names*
                (append *skill-logical-turn-selection-names* (list name)))
          t))))

(-> skill-record-steering-input ((or string user-message-input)) null)
(defun skill-record-steering-input (input)
  "Leave skill selection unchanged when steering input arrives."
  (declare (ignore input))
  nil)

(-> skill-select-for-logical-turn
    (configuration string)
    (values skill-metadata boolean))
(defun skill-select-for-logical-turn (configuration name)
  "Select exact discovered skill NAME for CONFIGURATION's active logical turn.

Return the selected metadata and true when this call newly selected it. Only
SKILL.LOAD selects a skill; catalog text and durable conversation text do not."
  (unless *skill-logical-turn-active-p*
    (error 'skill-selection-error
           :message
           "A skill can be selected only while an agent turn is active."
           :name name
           :reason ':inactive-turn))
  (let ((metadata
          (skill-catalog-find
           (skill-catalog-for-configuration configuration)
           name)))
    (unless metadata
      (error 'skill-selection-error
             :message
             (format nil
                     "No discovered skill has the exact case-sensitive name ~S."
                     name)
             :name name
             :reason ':unknown-skill))
    (values metadata (skill--logical-turn-record metadata))))



;;;; -- Model-Visible Catalog Adapter --

(-> skill--library-catalog-prefix () string)
(defun skill--library-catalog-prefix ()
  "Return the provider-neutral cl-skills catalog introduction."
  (format nil
          "## Skills~2%A skill is a reusable instruction set stored in native SKILL.sexp or standard SKILL.md. The entries below contain metadata and exact source locations only. Descriptions may be shortened to keep this catalog bounded.~2%### Available skills~%"))

(-> skill--library-catalog-guidance () string)
(defun skill--library-catalog-guidance ()
  "Return the provider-neutral cl-skills selection guidance."
  (format nil
          "~%### Skill rules~%When a task names a listed skill or clearly matches its description, select that skill by exact name through the host application before other task actions. Select every applicable skill. Treat selected instructions as request-local unless the host application documents another lifetime.~2%Before acting, read every selected instruction body completely from request-local context. Resolve linked relative paths from the source file's directory and load only resources needed for the task. Prefer provided scripts and assets. If a skill cannot be read or applied, state that briefly and continue with the best fallback."))

(-> skill--catalog-prefix () string)
(defun skill--catalog-prefix ()
  "Return the Autolith model-visible skill catalog introduction."
  (format nil
          "## Skills~2%An Autolith skill is a reusable instruction set stored in SKILL.sexp or standard SKILL.md. The entries below contain metadata and exact source locations only. Descriptions may be shortened to keep this catalog bounded.~2%### Available skills~%"))

(-> skill--catalog-guidance () string)
(defun skill--catalog-guidance ()
  "Return Autolith skill.load and progressive-disclosure guidance."
  (format nil
          "~%### Skill rules~%When the user names a listed skill or the task clearly matches a description, call `skill.load` with its exact name before other task actions. Call it once for every applicable skill. Do not read the skill source through `resource.read`; `skill.load` makes Autolith inject only its instruction body ephemerally into subsequent provider requests in this logical turn. Do not carry a skill into later turns unless it is selected again.~2%Before acting, read every selected instruction body completely from request-local context. Resolve linked relative paths from the source file's directory and load only resources needed for the task. Prefer provided scripts and assets. If a skill cannot be read or applied, state that briefly and continue with the best fallback."))

(-> skill--replace-catalog-section (string string string) string)
(defun skill--replace-catalog-section (catalog source replacement)
  "Replace the one exact SOURCE section in CATALOG with REPLACEMENT."
  (let ((position (search source catalog)))
    (unless position
      (error "cl-skills returned a catalog without its expected protocol section."))
    (concatenate 'string
                 (subseq catalog 0 position)
                 replacement
                 (subseq catalog (+ position (length source))))))

(-> skill-catalog-render
    (skill-catalog &key (:character-budget (integer 1)))
    (values string (integer 0) (integer 0)))
(defun skill-catalog-render
    (catalog &key (character-budget *skill-catalog-character-budget*))
  "Render CATALOG through cl-skills with Autolith-specific selection guidance."
  (let* ((library-prefix    (skill--library-catalog-prefix))
         (library-guidance  (skill--library-catalog-guidance))
         (autolith-prefix   (skill--catalog-prefix))
         (autolith-guidance (skill--catalog-guidance))
         (host-overhead
           (- (+ (length autolith-prefix) (length autolith-guidance))
              (+ (length library-prefix) (length library-guidance))))
         (library-budget (max 1 (- character-budget host-overhead))))
    (handler-case
        (multiple-value-bind (rendered included omitted)
            (cl-skills:skill-catalog-render
             catalog
             :character-budget library-budget)
          (let ((adapted
                  (skill--replace-catalog-section
                   (skill--replace-catalog-section
                    rendered library-prefix autolith-prefix)
                   library-guidance autolith-guidance)))
            (values adapted included omitted)))
      (cl-skills:skill-catalog-render-error (condition)
        (let ((minimum-required
                (+ host-overhead
                   (skill-catalog-render-error-minimum-required condition))))
          (error 'skill-catalog-render-error
                 :message
                 (format nil
                         "Skill catalog budget ~D is below the required ~D characters."
                         character-budget
                         minimum-required)
                 :character-budget character-budget
                 :minimum-required minimum-required))))))

;;;; -- Autolith Skill Roots --

(-> skill-roots (configuration) list)
(defun skill-roots (configuration)
  "Return project, user, optional site, and bundled skill roots by precedence."
  (remove-duplicates
   (remove
    nil
    (list
     (merge-pathnames
      ".autolith/skills/"
      (workspace-project-root
       (configuration-working-directory configuration)))
     (merge-pathnames "skills/"
                      (configuration-config-root configuration))
     (let ((site-root (configuration-site-config-root configuration)))
       (when site-root
         (merge-pathnames "skills/" site-root)))
     (merge-pathnames "skills/"
                      (configuration-source-root configuration))))
   :test #'equal
   :from-end t))

(-> skill-catalog-for-configuration (configuration) skill-catalog)
(defun skill-catalog-for-configuration (configuration)
  "Discover the current request's skill catalog for CONFIGURATION."
  (skill-catalog-discover
   (skill-roots configuration)
   :cache-root (configuration-cache-root configuration)))


;;;; -- Request-Local Skill Instructions --

(-> skill--explicit-instruction (skill-metadata string) string)
(defun skill--explicit-instruction (metadata instructions)
  "Return a request-local contribution containing selected INSTRUCTIONS."
  (format nil
          "Skill ~A is selected for this request. Its complete current instruction body from ~A follows. Apply it for this request only; do not carry it into later turns unless selected again.~2%~A"
          (skill-metadata-name metadata)
          (namestring (skill-metadata-pathname metadata))
          instructions))

(-> skill--read-failure-instruction (skill-metadata skill-read-error) string)
(defun skill--read-failure-instruction (metadata condition)
  "Return an ephemeral warning for a selected unreadable skill."
  (format nil
          "Skill ~A was selected for this request but could not be read from ~A: ~A Continue with the best fallback and do not claim that its instructions were applied."
          (skill-metadata-name metadata)
          (namestring (skill-metadata-pathname metadata))
          condition))

(-> skill--diagnostic-summary (skill-catalog) (option string))
(defun skill--diagnostic-summary (catalog)
  "Return a bounded summary of non-routine CATALOG diagnostics."
  (let* ((diagnostics
           (remove ':missing-root
                   (skill-catalog-diagnostics catalog)
                   :key #'skill-diagnostic-kind))
         (counts nil))
    (dolist (diagnostic diagnostics)
      (let* ((kind (skill-diagnostic-kind diagnostic))
             (entry (assoc kind counts)))
        (if entry
            (incf (rest entry))
            (push (cons kind 1) counts))))
    (when counts
      (format nil
              "~D skill entr~:@P had diagnostics and were not silently applied (~{~(~A~): ~D~^, ~}). Run /skills for bounded details."
              (length diagnostics)
              (loop for (kind . count) in (nreverse counts)
                    append (list kind count))))))

(-> skill--catalog-instruction (skill-catalog) string)
(defun skill--catalog-instruction (catalog)
  "Render CATALOG with a bounded summary of discovery diagnostics."
  (multiple-value-bind (rendered included omitted)
      (skill-catalog-render catalog)
    (declare (ignore included omitted))
    (let ((summary (skill--diagnostic-summary catalog)))
      (if (and summary
               (<= (+ (length rendered) 2 (length summary))
                   *skill-catalog-character-budget*))
          (format nil "~A~2%~A" rendered summary)
          rendered))))

(-> skill--context-identifier (string string) string)
(defun skill--context-identifier (prefix name)
  "Return a stable context contribution identifier from PREFIX and skill NAME."
  (format nil "~A-~A" prefix name))

(-> skill--truncate-string (string (integer 1)) string)
(defun skill--truncate-string (text character-limit)
  "Return TEXT truncated to exactly CHARACTER-LIMIT characters at most."
  (if (<= (length text) character-limit)
      text
      (if (<= character-limit 3)
          (subseq text 0 character-limit)
          (concatenate
           'string
           (subseq text 0 (- character-limit 3))
           "..."))))

(-> skill--bounded-warning (string) string)
(defun skill--bounded-warning (warning)
  "Return WARNING truncated to the exact request-local warning limit."
  (skill--truncate-string warning *skill-warning-character-limit*))

(-> skill--warning-contribution (skill-metadata string) context-contribution)
(defun skill--warning-contribution (metadata warning)
  "Return one mandatory request-local WARNING for selected skill METADATA."
  (make-context-contribution
   :identifier
   (skill--context-identifier "skill-warning" (skill-metadata-name metadata))
   :instruction (skill--bounded-warning warning)
   :priority 910
   :class ':mandatory
   :deduplication-key
   (skill--context-identifier "skill-warning" (skill-metadata-name metadata))))

(-> skill--missing-selection-contribution (string) context-contribution)
(defun skill--missing-selection-contribution (name)
  "Return a request-local warning when selected skill NAME disappeared."
  (let ((identifier (skill--context-identifier "skill-warning" name)))
    (make-context-contribution
     :identifier identifier
     :instruction
      (skill--bounded-warning
       (format nil
               "Skill ~A was selected for this request but its valid source definition is no longer discoverable. Continue with the best fallback and do not claim that its instructions were applied."
               name))
     :priority 910
     :class ':mandatory
     :deduplication-key identifier)))


(-> skill--warning-contribution-p (context-contribution) boolean)
(defun skill--warning-contribution-p (contribution)
  "Return true when CONTRIBUTION is one selected-skill warning."
  (let ((identifier (context-contribution-identifier contribution)))
    (and (>= (length identifier) (length "skill-warning-"))
         (string= identifier
                  "skill-warning-"
                  :end1 (length "skill-warning-")
                  :end2 (length "skill-warning-")))))

(-> skill--warning-overflow-contribution ((integer 1) (integer 1))
    context-contribution)
(defun skill--warning-overflow-contribution (omitted-count character-limit)
  "Return one bounded mandatory warning for OMITTED-COUNT collapsed warnings."
  (let ((identifier "skill-warning-overflow"))
    (make-context-contribution
     :identifier identifier
     :instruction
     (skill--truncate-string
      (format nil
              "~D additional selected-skill warning~:P were omitted because their aggregate output reached the ~D-character limit. Treat those skills as unavailable and do not claim their instructions were applied."
              omitted-count
              *skill-warning-aggregate-character-limit*)
      character-limit)
     :priority 910
     :class ':mandatory
     :deduplication-key identifier)))

(-> skill--bound-warning-contributions (list) list)
(defun skill--bound-warning-contributions (contributions)
  "Bound warning payloads in CONTRIBUTIONS and collapse every excess warning."
  (let* ((warnings
           (remove-if-not #'skill--warning-contribution-p contributions))
         (ordinary
           (remove-if #'skill--warning-contribution-p contributions))
         (total
           (loop for warning in warnings
                 sum (length
                      (context-contribution-instruction warning)))))
    (if (<= total *skill-warning-aggregate-character-limit*)
        contributions
        (let ((kept nil)
              (used 0)
              (remaining (length warnings)))
          (dolist (warning warnings)
            (let* ((warning-length
                     (length
                      (context-contribution-instruction warning)))
                   (next-remaining (1- remaining))
                   (overflow
                     (skill--warning-overflow-contribution
                      (max 1 next-remaining)
                      *skill-warning-aggregate-character-limit*))
                   (overflow-length
                     (length
                      (context-contribution-instruction overflow))))
              (if (and (plusp next-remaining)
                       (<= (+ used warning-length overflow-length)
                           *skill-warning-aggregate-character-limit*))
                  (progn
                    (push warning kept)
                    (incf used warning-length)
                    (setf remaining next-remaining))
                  (return))))
          (let* ((omitted (- (length warnings) (length kept)))
                 (available
                   (max 1
                        (- *skill-warning-aggregate-character-limit*
                           used)))
                 (overflow
                   (skill--warning-overflow-contribution omitted available)))
            (append ordinary
                    (nreverse kept)
                    (list overflow)))))))

(-> skill--append-selected-contribution
    (skill-catalog string list (integer 0))
    (values list (integer 0)))
(defun skill--append-selected-contribution
    (catalog name contributions selected-characters)
  "Return CONTRIBUTIONS and SELECTED-CHARACTERS after adding NAME from CATALOG."
  (let ((metadata (skill-catalog-find catalog name)))
    (cond
      ((null metadata)
       (values
        (append contributions
                (list (skill--missing-selection-contribution name)))
        selected-characters))
      (t
       (handler-case
           (let* ((instructions (skill-metadata-read metadata))
                  (instruction
                    (skill--explicit-instruction metadata instructions))
                  (next-total (+ selected-characters (length instruction))))
             (if (> next-total *skill-selection-character-limit*)
                 (values
                  (append
                   contributions
                   (list
                    (skill--warning-contribution
                     metadata
                     (format nil
                             "Skill ~A was selected but omitted because selected skill instructions exceed the ~D-character aggregate limit. Continue with the best fallback and report the omission."
                             (skill-metadata-name metadata)
                             *skill-selection-character-limit*))))
                  selected-characters)
                 (values
                  (append
                   contributions
                   (list
                    (make-context-contribution
                     :identifier
                     (skill--context-identifier
                      "skill-selected"
                      (skill-metadata-name metadata))
                     :instruction instruction
                     :priority 920
                     :class ':mandatory
                     :deduplication-key
                     (skill--context-identifier
                      "skill-selected"
                      (skill-metadata-name metadata)))))
                  next-total)))
         (skill-read-error (condition)
           (values
            (append
             contributions
             (list
              (skill--warning-contribution
               metadata
               (skill--read-failure-instruction metadata condition))))
            selected-characters)))))))

(-> skill--request-contributions-for-catalog
    (skill-catalog conversation)
    list)
(defun skill--request-contributions-for-catalog (catalog conversation)
  "Return request-local contributions selected from metadata CATALOG."
  (declare (ignore conversation))
  (let* ((skills (skill-catalog-skills catalog))
         (selection-names
           (if *skill-logical-turn-active-p*
               (copy-list *skill-logical-turn-selection-names*)
               nil)))
    (when (or skills selection-names)
      (let ((contributions
              (list (make-context-contribution
                     :identifier "skill-catalog"
                     :instruction (skill--catalog-instruction catalog)
                     :priority 900
                     :class ':mandatory
                     :deduplication-key "skill-catalog")))
            (selected-characters 0))
          (dolist (name selection-names)
            (unless (skill-catalog-find catalog name)
              (setf contributions
                    (append
                     contributions
                     (list (skill--missing-selection-contribution name))))))
          (dolist (metadata skills)
            (when (member (skill-metadata-name metadata)
                          selection-names
                          :test #'string=)
              (multiple-value-setq (contributions selected-characters)
                (skill--append-selected-contribution
                 catalog
                 (skill-metadata-name metadata)
                 contributions
                 selected-characters))))
        (skill--bound-warning-contributions contributions)))))

(-> skill-request-contributions
    (configuration conversation)
    list)
(defun skill-request-contributions (configuration conversation)
  "Return catalog and selected skill contributions for one provider request."
  (skill--request-contributions-for-catalog
   (skill-catalog-for-configuration configuration)
   conversation))

(-> skill--load-tool-visible-p (request-context) boolean)
(defun skill--load-tool-visible-p (request)
  "Return true when REQUEST exposes the exact skill.load tool."
  (not
   (null
    (loop for namespace
            across (request-context-tool-namespaces request)
          for namespace-name =
            (and
             (json-object-p namespace)
             (json-get namespace "name"))
          for tools =
            (and
             (json-object-p namespace)
             (json-get namespace "tools"))
          thereis
          (and
           (stringp namespace-name)
           (string= namespace-name "skill")
           (vectorp tools)
           (loop for tool across tools
                 for tool-name =
                   (and
                    (json-object-p tool)
                    (json-get tool "name"))
                 thereis
                 (and
                  (stringp tool-name)
                  (string= tool-name "load"))))))))

(-> skill-context-contributor (request-context) list)
(defun skill-context-contributor (request)
  "Return request-local skill contributions for normal provider REQUEST."
  (unless (or (request-context-compaction-p request)
              (not (skill--load-tool-visible-p request)))
    (skill-request-contributions
     (request-context-configuration request)
     (request-context-conversation request))))

(register-context-contributor
 "skills" 'skill-context-contributor :source ':built-in)


;;;; -- Skill Status --

(-> skill--diagnostic-line (skill-diagnostic) string)
(defun skill--diagnostic-line (diagnostic)
  "Return one concise human-readable skill DIAGNOSTIC line."
  (format nil
          "~(~A~)  ~A  ~A"
          (skill-diagnostic-kind diagnostic)
          (namestring (skill-diagnostic-pathname diagnostic))
          (skill-diagnostic-message diagnostic)))

(-> skill-status (configuration) string)
(defun skill-status (configuration)
  "Return discovered skills and diagnostics for CONFIGURATION."
  (let* ((catalog (skill-catalog-for-configuration configuration))
         (skills (skill-catalog-skills catalog))
         (diagnostics
           (remove ':missing-root
                   (skill-catalog-diagnostics catalog)
                   :key #'skill-diagnostic-kind))
         (shown-skills
           (subseq skills
                   0
                   (min (length skills)
                        *skill-status-entry-limit*)))
         (shown-diagnostics
           (subseq diagnostics
                   0
                   (min (length diagnostics)
                        *skill-status-entry-limit*)))
         (omitted-skills (- (length skills) (length shown-skills)))
         (omitted-diagnostics
           (- (length diagnostics) (length shown-diagnostics))))
    (with-output-to-string (stream)
      (format stream "Skills:~%")
      (if shown-skills
          (loop for metadata in shown-skills
                for first-p = t then nil
                do
                   (unless first-p
                     (terpri stream))
                   (format stream
                           "~A  ~A~%  ~A"
                           (skill-metadata-name metadata)
                           (namestring (skill-metadata-pathname metadata))
                           (skill-metadata-description metadata)))
          (format stream
                  "none discovered~%~%Create a directory named for the skill with SKILL.md or SKILL.sexp beneath one of:~%~{  ~A~^~%~}"
                  (mapcar #'namestring (skill-roots configuration))))
      (when (plusp omitted-skills)
        (format stream
                "~%... ~D more skill~:P omitted."
                omitted-skills))
      (when shown-diagnostics
        (format stream
                "~2%Diagnostics:~%~{~A~^~%~}"
                (mapcar #'skill--diagnostic-line shown-diagnostics)))
      (when (plusp omitted-diagnostics)
        (format stream
                "~%... ~D more diagnostic~:P omitted."
                omitted-diagnostics)))))

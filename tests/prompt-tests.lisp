(in-package #:autolith)

;;;; -- System Prompt Tests --

(-> prompt-tests--contains (string string string) null)
(defun prompt-tests--contains (text snippet description)
  "Assert TEXT contains SNIPPET."
  (test-assert (search snippet text) description))

(-> prompt-tests--absent (string string string) null)
(defun prompt-tests--absent (text snippet description)
  "Assert TEXT does not contain SNIPPET."
  (test-assert (not (search snippet text)) description))

(-> test-system-prompt () null)
(defun test-system-prompt ()
  "Test Org-templated system-prompt gating, slots, and the public entrypoint."
  (let ((configuration (test-configuration)))
    (unwind-protect
         (progn
           (test-assert (probe-file (system-prompt--template-path))
                        "the Org system prompt template is shipped with Autolith")
           (let ((prompt (system-prompt configuration)))
             (prompt-tests--contains prompt "You are Autolith"
                                     "the rendered prompt keeps the Autolith identity")
             (prompt-tests--contains prompt "Current workspace agenda: empty."
                                     "an empty agenda uses the empty-agenda section")
             (prompt-tests--contains prompt "Your main power is the live image"
                                     "a mutable session uses the live-image section")
             (prompt-tests--contains prompt "Use web.run"
                                     "cached web search advertises web.run")
             (prompt-tests--contains prompt (system-prompt--current-date)
                                     "the prompt embeds today's date")
             (prompt-tests--contains prompt "Workspace instructions from"
                                     "workspace AGENTS.md is included")
             (prompt-tests--absent prompt "{{{:"
                                   "no leftover org-templater slots remain")
             (prompt-tests--absent prompt "#+TITLE"
                                   "Org keyword lines are stripped")
             (prompt-tests--absent prompt "* Commentary"
                                   "review commentary is omitted")
             (prompt-tests--contains prompt "RECURSIVE INFERENCE IS AVAILABLE"
                                     "RLM guidance rides along with the registered rlm tools")
             (prompt-tests--absent prompt "SIMPLE TECHNICAL ENGLISH MODE IS ACTIVE"
                                   "STE is omitted when the preference is off")
             (prompt-tests--absent prompt "HURRY-UP MODE IS ACTIVE"
                                   "hurry-up is omitted unless requested")
             (prompt-tests--absent prompt "hosted web_search"
                                   "hosted search is omitted unless the request hosts it")
             (prompt-tests--absent prompt "This session was started with --immutable"
                                   "immutable guidance is omitted for a live image"))
           (let ((prompt (let ((*system-prompt-hosted-web-search-p* t))
                           (system-prompt configuration))))
             (prompt-tests--contains prompt "hosted web_search"
                                     "hosted search guidance follows the request binding")
             (prompt-tests--absent prompt "Use web.run"
                                   "hosted search takes precedence over web.run"))
           (let ((prompt (system-prompt configuration :hurry-up-p t)))
             (prompt-tests--contains prompt "HURRY-UP MODE IS ACTIVE"
                                     "the hurry-up keyword inserts hurry-up guidance"))
           (setf (slot-value configuration 'web-search-mode) "disabled")
           (let ((prompt (system-prompt configuration)))
             (prompt-tests--absent prompt "WEB SEARCH IS AVAILABLE"
                                   "disabled web search omits both search vehicles"))
           (setf (slot-value configuration 'immutable-p) t)
           (let ((prompt (system-prompt configuration)))
             (prompt-tests--contains prompt "This session was started with --immutable"
                                     "an immutable session uses the immutable-image section")
             (prompt-tests--contains prompt "The self namespace is inspection-only"
                                     "an immutable session uses inspection-only self tools")
             (prompt-tests--absent prompt "Your main power is the live image"
                                   "an immutable session omits live-image guidance"))
           (setf (slot-value configuration 'immutable-p) nil)
           (preferences-set-simple-technical-english configuration t)
           (prompt-tests--contains (system-prompt configuration)
                                   "SIMPLE TECHNICAL ENGLISH MODE IS ACTIVE"
                                   "STE follows the durable preference"))
      (uiop:delete-directory-tree (test-configuration-root configuration)
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

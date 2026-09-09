(in-package #:autolith)

;;;; -- Parallel Web Search with RLM Extraction --

(defparameter *web-search-endpoint* "https://api.parallel.ai/v1/search"
  "The Parallel search API endpoint.")

(defparameter *web-search-default-results* 5
  "The number of pages web.search fetches by default.")

(defparameter *web-search-maximum-results* 8
  "The largest number of pages one web.search call may fetch.")

(defparameter *web-search-fetch-concurrency* 4
  "The largest number of pages web.search fetches at the same time.")

(defparameter *web-search-page-character-limit* 20000
  "The largest number of characters web.search keeps from one page body.")

(defparameter *web-search-rlm-tokens* 100000
  "The token budget of one web.search extraction and synthesis subtree.")

(defclass web-search-tool (tool)
  ()
  (:documentation
   "A tool that searches with Parallel, fetches the top pages, and returns
one extracted answer with cited sources."))

(defmethod tool-storm-guard-exempt-p ((tool web-search-tool))
  "Exempt read-only web extraction from the mutating-call storm guard."
  (declare (ignore tool))
  t)

(defmethod tool-execute ((tool web-search-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search with Parallel, fetch the top pages, and extract one cited answer."
  (declare (ignore tool context))
  (let ((objective (tool-argument arguments "query" :required t))
        (mode (web-search--mode arguments))
        (queries (web-search--queries arguments))
        (limit (web-search--result-limit arguments)))
    (unless (non-empty-string-p objective)
      (error 'tool-error
             :message "web.search requires a non-empty string query."
             :tool-name "web.search"))
    (handler-case
        (multiple-value-bind (results warnings search-id)
            (web-search--search objective queries mode)
          (let ((pages (web-search--select-pages results limit)))
            (when (null pages)
              (return-from tool-execute
                (tool-success
                 (format nil
                         "The Parallel search returned no results for this query.~
                          ~@[~2%Parallel warnings:~%~{- ~A~%~}~]"
                         warnings))))
            (web-search--answer objective pages warnings search-id)))
      (tool-error (condition)
        (error condition))
      (error (condition)
        (error 'tool-error
               :message (format nil "web.search failed: ~A" condition)
               :tool-name "web.search")))))

(-> web-search-parameters () json-object)
(defun web-search-parameters ()
  "Return the web.search parameter schema."
  (tool-object-schema
   (json-object
    "query"          (tool-string-property
                      "The search objective and the information the answer must cover.")
    "search_queries" (web--string-array-schema
                      "Optional explicit search queries for Parallel.")
    "max_results"    (tool-integer-property
                      "How many top results to fetch, from 1 to 8; defaults to 5.")
    "mode"           (web--enum-schema '("fast" "turbo" "advanced")
                                       "Parallel search mode; defaults to fast."))
   '("query")))

(-> web-search--mode (json-object) string)
(defun web-search--mode (arguments)
  "Return the validated Parallel mode from ARGUMENTS."
  (let ((mode (or (tool-argument arguments "mode") "fast")))
    (unless (and (stringp mode)
                 (member mode '("fast" "turbo" "advanced") :test #'string=))
      (error 'tool-error
             :message "web.search mode must be fast, turbo, or advanced."
             :tool-name "web.search"))
    mode))

(-> web-search--queries (json-object) list)
(defun web-search--queries (arguments)
  "Return the validated optional search queries from ARGUMENTS."
  (let ((queries (tool-argument arguments "search_queries")))
    (when queries
      (unless (and (vectorp queries)
                   (plusp (length queries))
                   (every (lambda (query)
                            (and (stringp query) (non-empty-string-p query)))
                          queries))
        (error 'tool-error
               :message
               "web.search search_queries must be a non-empty array of non-empty strings."
               :tool-name "web.search"))
      (coerce queries 'list))))

(-> web-search--result-limit (json-object) (integer 1 *))
(defun web-search--result-limit (arguments)
  "Return the validated page limit from ARGUMENTS."
  (let ((limit (or (tool-argument arguments "max_results")
                   *web-search-default-results*)))
    (unless (and (integerp limit) (plusp limit))
      (error 'tool-error
             :message "web.search max_results must be a positive integer."
             :tool-name "web.search"))
    (min limit *web-search-maximum-results*)))

(-> web-search--api-key () string)
(defun web-search--api-key ()
  "Return the Parallel API key from the environment."
  (or (uiop:getenv "PARALLEL_API_KEY")
      (error 'tool-error
             :message
             "web.search requires the PARALLEL_API_KEY environment variable."
             :tool-name "web.search")))

(-> web-search--request (string list string) json-object)
(defun web-search--request (objective queries mode)
  "Return the Parallel search request for OBJECTIVE, QUERIES, and MODE.

QUERIES defaults to OBJECTIVE because the Parallel API requires search_queries."
  (let ((request (json-object "objective" objective "mode" mode)))
    (setf (gethash "search_queries" request)
          (coerce (or queries (list objective)) 'vector))
    request))

(-> web-search--excerpts (json-object) list)
(defun web-search--excerpts (entry)
  "Return ENTRY's excerpt strings, or NIL."
  (let ((excerpts (json-get entry "excerpts")))
    (when (vectorp excerpts)
      (loop for excerpt across excerpts
            when (stringp excerpt)
              collect excerpt))))

(-> web-search--result (json-object integer) list)
(defun web-search--result (entry index)
  "Return ENTRY as one ordered web.search result plist."
  (list ':index index
        ':url (json-get entry "url")
        ':title (json-get entry "title")
        ':publish-date (json-get entry "publish_date")
        ':excerpts (web-search--excerpts entry)))

(-> web-search--warnings (json-object) list)
(defun web-search--warnings (response)
  "Return RESPONSE's warning strings, or NIL."
  (let ((warnings (json-get response "warnings")))
    (when (vectorp warnings)
      (loop for warning across warnings
            when warning
              collect (typecase warning
                        (string warning)
                        (hash-table (or (json-get warning "message")
                                        (json-encode warning)))
                        (t (format nil "~A" warning)))))))

(-> web-search--search (string list string) (values list list t))
(defun web-search--search (objective queries mode)
  "Call the Parallel search API for OBJECTIVE, QUERIES, and MODE.

Return ordered result plists, warning strings, and the search identifier."
  (let ((api-key (web-search--api-key)))
    (multiple-value-bind (body status)
        (handler-case
            (dexador:post *web-search-endpoint*
                          :headers (list (cons "x-api-key" api-key)
                                         (cons "content-type" "application/json"))
                          :content (json-encode
                                    (web-search--request objective queries mode)))
          (error (condition)
            (error 'tool-error
                   :message (format nil "Parallel search failed: ~A" condition)
                   :tool-name "web.search")))
      (unless (= status 200)
        (error 'tool-error
               :message (format nil "Parallel search returned HTTP ~D." status)
               :tool-name "web.search"))
      (let* ((response (json-decode body))
             (entries (json-get response "results")))
        (values (when (vectorp entries)
                  (loop for entry across entries
                        for index from 0
                        when (json-object-p entry)
                          collect (web-search--result entry index)))
                (web-search--warnings response)
                (json-get response "search_id"))))))

(-> web-search--select-pages (list integer) list)
(defun web-search--select-pages (results limit)
  "Return the first LIMIT deduplicated RESULTS as ordered page plists."
  (let ((seen nil)
        (pages nil))
    (loop for result in results
          for url = (getf result ':url)
          while (< (length pages) limit)
          when (and (non-empty-string-p url)
                    (not (member url seen :test #'string=)))
            do (push url seen)
               (push result pages))
    (nreverse pages)))

(-> web-search--page-body (list) string)
(defun web-search--page-body (page)
  "Return the truncated Markdown body of PAGE."
  (let ((markdown (web-gist--retrieve (getf page ':url))))
    (if (> (length markdown) *web-search-page-character-limit*)
        (subseq markdown 0 *web-search-page-character-limit*)
        markdown)))

(-> web-search--fetch-slice (list integer integer) list)
(defun web-search--fetch-slice (pages worker worker-count)
  "Fetch the PAGES assigned to WORKER and return one outcome per page."
  (loop for page in pages
        for index from 0
        when (= (mod index worker-count) worker)
          collect
          (handler-case
              (list page (web-search--page-body page) nil)
            (error (condition)
              (list page nil (format nil "~A" condition))))))

(-> web-search--fetch-pages (list) list)
(defun web-search--fetch-pages (pages)
  "Fetch PAGES concurrently and return them in order with bodies or errors."
  (when pages
    (let* ((count (length pages))
           (worker-count (max 1 (min *web-search-fetch-concurrency* count)))
           (outcomes
            (mapcan #'join-thread
                    (loop for worker below worker-count
                          collect
                          ;; Bind the index now: a thread can start after
                          ;; LOOP advances WORKER, and then it reads the
                          ;; wrong index.
                          (let ((worker-index worker))
                            (make-thread
                             (lambda ()
                               (web-search--fetch-slice
                                pages worker-index worker-count))
                             :name "autolith web-search fetch"))))))
      (loop for page in pages
            collect
            (let ((outcome (find page outcomes :key #'first)))
              (append page
                      (list ':body (second outcome)
                            ':fetch-error (third outcome))))))))

(-> web-search--excerpt-pages (list) list)
(defun web-search--excerpt-pages (pages)
  "Return PAGES with their Parallel excerpts as bodies."
  (loop for page in pages
        for excerpts = (getf page ':excerpts)
        when excerpts
          collect (append page
                          (list ':body
                                (format nil "~{~A~^~%~%~}" excerpts)))))

(-> web-search--page-view (list) list)
(defun web-search--page-view (page)
  "Return PAGE as one labeled extraction view."
  (list ':label (or (getf page ':title) (getf page ':url))
        ':content
        (format nil "Page URL: ~A~@[~%Published: ~A~]~2%~A"
                (getf page ':url)
                (getf page ':publish-date)
                (getf page ':body))))

(-> web-search--extraction-task (list string) string)
(defun web-search--extraction-task (page objective)
  "Return PAGE's extraction task for OBJECTIVE."
  (format nil
          "Extract every fact on this page that helps answer the objective ~
below. Prefix each fact with the page URL. If nothing on this page is ~
relevant, return exactly: NO RELEVANT INFORMATION.~%Objective: ~A~%Page URL: ~A"
          objective
          (getf page ':url)))

(-> web-search--subtasks (list string) list)
(defun web-search--subtasks (pages objective)
  "Return one extraction subtask plist per PAGE."
  (loop for page in pages
        collect
        (list ':task (web-search--extraction-task page objective)
              ':context (list (web-search--page-view page)))))

(-> web-search--source-lines (list) list)
(defun web-search--source-lines (pages)
  "Return one numbered source line per PAGE."
  (loop for page in pages
        for number from 1
        collect
        (format nil "~D. ~@[~A - ~]~A~@[ - published ~A~]~%"
                number
                (getf page ':title)
                (getf page ':url)
                (getf page ':publish-date))))

(-> web-search--synthesis-task (string list) string)
(defun web-search--synthesis-task (objective pages)
  "Return the synthesis task for OBJECTIVE and source PAGES."
  (format nil
          "Answer this objective using only the attached page extractions. ~
Ignore extractions that only say NO RELEVANT INFORMATION. Extraction views ~
appear in source order, so view N belongs to source [N]. Cite facts as [N] ~
matching this source list.~%Objective: ~A~%~{~A~}"
          objective
          (web-search--source-lines pages)))

(-> web-search--result-text
    (string list list &key (:skipped list) (:warnings list) (:search-id t)
            (:trace t))
    string)
(defun web-search--result-text (answer pages results
                                &key skipped warnings search-id trace)
  "Return the final web.search text from ANSWER, source PAGES, and RESULTS."
  (with-output-to-string (stream)
    (write-string answer stream)
    (format stream "~2%Sources:~%~{~A~}" (web-search--source-lines pages))
    (let ((failures
            (loop for result in results
                  for number from 1
                  when (getf result ':error)
                    collect
                    (format nil "- source ~D (~A): ~A~%"
                            number
                            (getf (nth (1- number) pages) ':url)
                            (getf result ':error)))))
      (when failures
        (format stream "~2%Failed extractions:~%~{~A~}" failures)))
    (when skipped
      (format stream
              "~2%Skipped pages:~%~{~A~}"
              (loop for page in skipped
                    collect
                    (format nil "- ~A: ~A~%"
                            (getf page ':url)
                            (getf page ':fetch-error)))))
    (when warnings
      (format stream
              "~2%Parallel warnings:~%~{~A~}"
              (loop for warning in warnings
                    collect (format nil "- ~A~%" warning))))
    (when search-id
      (format stream "~2%Parallel search: ~A~%" search-id))
    (when trace
      (format stream "~2%Trace: ~A~%" trace))))

(-> web-search--answer (string list list t) tool-result)
(defun web-search--answer (objective pages warnings search-id)
  "Fetch, extract, and synthesize one answer for OBJECTIVE from PAGES."
  (let* ((fetched (web-search--fetch-pages pages))
         (readable (remove-if (lambda (page) (getf page ':fetch-error)) fetched))
         (skipped (remove-if-not (lambda (page) (getf page ':fetch-error)) fetched))
         (sources (if readable readable (web-search--excerpt-pages pages))))
    (unless sources
      (error 'tool-error
             :message "web.search found results but no readable page content."
             :tool-name "web.search"))
    ;; RLM-MAP and synthesis resolve the inference environment when they
    ;; run, so this tool needs no eager provider lookup here.
    (let* ((budget (rlm-budget-create :calls (+ (length sources) 1)
                                      :tokens *web-search-rlm-tokens*))
           (extraction-results
            (rlm-map (web-search--subtasks sources objective)
                     :budget budget)))
      (unless (some (lambda (result) (getf result ':value)) extraction-results)
        (error 'tool-error
               :message
               (format nil
                       "web.search extraction failed for every source: ~{~A~^; ~}"
                       (mapcar (lambda (result) (getf result ':error))
                               extraction-results))
               :tool-name "web.search"))
      (multiple-value-bind (answer trace)
          (rlm-synthesize-inference-results
           ':web-search-pages
           (web-search--synthesis-task objective sources)
           extraction-results
           :budget budget)
        (tool-success
         (web-search--result-text
          (if (stringp answer) answer (rlm--result-sexp answer))
          sources extraction-results
          :skipped skipped
          :warnings warnings
          :search-id search-id
          :trace trace))))))

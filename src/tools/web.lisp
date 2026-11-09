(in-package #:autolith)

;;;; -- Standalone Provider Web Search --

(defparameter *web-run-description*
  "Tool for accessing the internet.


---

## Examples of different commands available in this tool

Examples of different commands available in this tool:
* `search_query`: {\"search_query\": [{\"q\": \"What is the capital of France?\"}, {\"q\": \"What is the capital of belgium?\"}]}. Searches the internet for a given query (and optionally with a domain or recency filter)
* `image_query`: {\"image_query\":[{\"q\": \"waterfalls\"}]}.
* `open`: {\"open\": [{\"ref_id\": \"turn0search0\"}, {\"ref_id\": \"https://www.openai.com\", \"lineno\": 120}]}
* `click`: {\"click\": [{\"ref_id\": \"turn0fetch3\", \"id\": 17}]}
* `find`: {\"find\": [{\"ref_id\": \"turn0fetch3\", \"pattern\": \"Annie Case\"}]}
* `screenshot`: {\"screenshot\": [{\"ref_id\": \"turn1view0\", \"pageno\": 0}, {\"ref_id\": \"turn1view0\", \"pageno\": 3}]}
* `finance`: {\"finance\":[{\"ticker\":\"AMD\",\"type\":\"equity\",\"market\":\"USA\"}]}, {\"finance\":[{\"ticker\":\"BTC\",\"type\":\"crypto\",\"market\":\"\"}]}
* `weather`: {\"weather\":[{\"location\":\"San Francisco, CA\"}]}
* `sports`: {\"sports\":[{\"fn\":\"standings\",\"league\":\"nfl\"}, {\"fn\":\"schedule\",\"league\":\"nba\",\"team\":\"GSW\",\"date_from\":\"2025-02-24\"}]}
* `time`: {\"time\":[{\"utc_offset\":\"+03:00\"}]}

---

## Usage hints

To use this tool efficiently:
* Use multiple commands and queries in one call to get more results faster; e.g. {\"search_query\": [{\"q\": \"bitcoin news\"}], \"finance\":[{\"ticker\":\"BTC\",\"type\":\"crypto\",\"market\":\"\"}], \"find\": [{\"ref_id\": \"turn0search0\", \"pattern\": \"Annie Case\"}, {\"ref_id\": \"turn0search1\", \"pattern\": \"John Smith\"}]}
* Use \"response_length\" to control the number of results returned by this tool, omit it if you intend to pass \"short\" in
* Only write required parameters; do not write empty lists or nulls where they could be omitted.
* `search_query` must have length at most 4 in each call. If it has length > 3, response_length must be medium or long
* If you find yourself in a situation where you accidentally call the `web.run` tool, it's best just to send an empty query: {\"search_query\": [{\"q\": \"\"}]}.

---

## Decision boundary

If the user makes an explicit request to search the internet, find latest information, look up, etc (or to not do so), you must obey their request.
When you make an assumption, always consider whether it is temporally stable; i.e. whether there's even a small (>10%) chance it has changed. If it is unstable, you must verify with browsing the internet for verification.

<situations_where_you_must_browse_the_internet>
Below is a list of scenarios where browsing the internet MUST be used. PAY CLOSE ATTENTION: you MUST browse the internet in these cases. If you're unsure or on the fence, you MUST bias towards browsing the internet.
- The information could have changed recently: for example news; prices; laws; schedules; product specs; sports scores; economic indicators; political/public/company figures (e.g. the question relates to 'the president of country A' or 'the CEO of company B', which might change over time); rules; regulations; standards; software libraries that could be updated; exchange rates; recommendations (i.e., recommendations about various topics or things might be informed by what currently exists / is popular / is safe / is unsafe / is in the zeitgeist / etc.); and many many many more categories -- again, if you're on the fence, you MUST browse the internet!
- For news queries, prioritize more recent events, ensuring you compare publish dates and the date that the event happened.
- The user is seeking recommendations that could lead them to spend substantial time or money -- researching products, restaurants, travel plans, etc.
- The user wants (or would benefit from) direct quotes, links, or precise source attribution.
- A specific page, paper, dataset, PDF, or site is referenced and you haven't been given its contents.
- You're unsure about a fact, the topic is niche or emerging, or you suspect there's at least a 10% chance you will incorrectly recall it
- High-stakes accuracy matters (medical, legal, financial guidance). For these you generally should search by default because this information is highly temporally unstable
- The user explicitly says to search, browse, verify, or look it up.
</situations_where_you_must_browse_the_internet>

---

## Citations

Results from `web.run` include internal reference IDs such as `turn2search5`. Use
those reference IDs only in calls to `web.run`; do not expose them in the final
response.

Cite sources in the final response using Markdown links:

- Cite a single source as `[descriptive source title](https://example.com/page)`.
- Cite multiple sources with separate Markdown links, for example
  `[first source](https://example.com/one)`, `[second source](https://example.com/two)`.
- Link directly to the page that supports the claim. Do not link to search result
  pages or use bare URLs.

Formatting of citations:

- Place each citation as near as possible to the claim it supports, normally at
  the end of the sentence or paragraph and after punctuation.
- Do not place citations inside code fences.
- Do not put citations on a line by themselves or collect all citations at the
  end of the response.

If you browse the internet, cite statements supported by web sources. Each cited
source must directly support the associated claim. Prefer primary and
authoritative sources, and use sources from different domains when the response
benefits from multiple perspectives.

---

## Special cases

If these conflict with any other instructions, these should take precedence.

<special_cases>
- When the user asks for information about how to use OpenAI products, (ChatGPT, the OpenAI API, etc.), you should check the code in local env and only browse as fallback, when you browse restrict your sources to official OpenAI websites using the domains filter, unless otherwise requested.
- When using search to answer technical questions, you must only rely on primary sources (research papers, official documentation, etc.)
- Clearly indicate when you are making an inference from sources.
</special_cases>

---

## Word limits

Responses may not excessively quote or draw on a specific source. There are several limits here:
- **Limit on verbatim quotes:**
  - You may not quote more than 25 words verbatim from any single non-lyrical source, unless the source is reddit.
  - For song lyrics, verbatim quotes must be limited to at most 10 words.
  - Long quotes from reddit are allowed, as long as you indicate that those are direct quotes via a markdown blockquote starting with \">\", copy verbatim, and link the source.
- **Word limits:**
  - Each webpage source in the sources has a word limit label formatted like \"[wordlim N]\", in which N is the maximum number of words in the whole response that are attributed to that source. If omitted, the word limit is 200 words.
  - Non-contiguous words derived from a given source must be counted to the word limit.
  - The summarization limit N is a maximum for each source.
- **Copyright compliance:**
  - You must avoid providing full articles, long verbatim passages, or extensive direct quotes due to copyright concerns.
  - If the user asked for a verbatim quote, the response should provide a short compliant excerpt and then answer with paraphrases and summaries.
  - Again, this limit does not apply to reddit, as long as you indicate that those are direct quotes via a markdown blockquote starting with \">\", copy verbatim, and link the source."
  "The Codex standalone web.run instructions at reference commit ba42e6866cef4baed7ad92c73e6be8cd42e49d8b.")

(-> web--object-schema (json-object &key (:required list)) json-object)
(defun web--object-schema (properties &key required)
  "Return the open JSON object schema used by Codex's web.run command model."
  (cl-llm-provider-api:provider-open-object-schema properties :required required))

(-> web--array-schema (json-object string) json-object)
(defun web--array-schema (items description)
  "Return an array schema containing ITEMS and described by DESCRIPTION."
  (cl-llm-provider-api:provider-array-schema items description))

(-> web--enum-schema (list string) json-object)
(defun web--enum-schema (values description)
  "Return the string enum schema containing VALUES and DESCRIPTION."
  (cl-llm-provider-api:provider-enum-schema values description))

(-> web--string-array-schema (string) json-object)
(defun web--string-array-schema (description)
  "Return a documented array schema containing strings."
  (web--array-schema (json-object "type" "string") description))

(-> web--search-query-schema () json-object)
(defun web--search-query-schema ()
  "Return the schema for a Codex web.run search query."
  (web--object-schema
   (json-object
    "q"       (tool-string-property "Search query.")
    "recency" (tool-integer-property
                "Recency filter, expressed as a number of recent days.")
    "domains" (web--string-array-schema
                "Domains to restrict the search to."))
   :required '("q")))

(-> web--open-operation-schema () json-object)
(defun web--open-operation-schema ()
  "Return the schema for a Codex web.run open-page command."
  (web--object-schema
   (json-object
    "ref_id" (tool-string-property "Reference id or URL to open.")
    "lineno" (tool-integer-property "Line number to position the page at."))
   :required '("ref_id")))

(-> web--click-operation-schema () json-object)
(defun web--click-operation-schema ()
  "Return the schema for a Codex web.run link-click command."
  (web--object-schema
   (json-object
    "ref_id" (tool-string-property "Reference ID of the page containing the numbered link.")
    "id"     (tool-integer-property "Numbered link id to open."))
   :required '("ref_id" "id")))

(-> web--find-operation-schema () json-object)
(defun web--find-operation-schema ()
  "Return the schema for a Codex web.run find-in-page command."
  (web--object-schema
   (json-object
    "ref_id"  (tool-string-property "Reference id or URL to search within.")
    "pattern" (tool-string-property "Text pattern to find."))
   :required '("ref_id" "pattern")))

(-> web--screenshot-operation-schema () json-object)
(defun web--screenshot-operation-schema ()
  "Return the schema for a Codex web.run PDF screenshot command."
  (web--object-schema
   (json-object
    "ref_id" (tool-string-property "Reference id or URL to screenshot.")
    "pageno" (tool-integer-property "Zero-indexed PDF page number."))
   :required '("ref_id" "pageno")))

(-> web--finance-operation-schema () json-object)
(defun web--finance-operation-schema ()
  "Return the schema for a Codex web.run finance lookup."
  (web--object-schema
   (json-object
    "ticker" (tool-string-property "Ticker symbol to look up.")
    "type"   (web--enum-schema '("equity" "fund" "crypto" "index")
                                "Asset type to look up.")
    "market" (tool-string-property
              "ISO 3166-1 alpha-3 country code, OTC, or an empty string for cryptocurrency."))
   :required '("ticker" "type")))

(-> web--weather-operation-schema () json-object)
(defun web--weather-operation-schema ()
  "Return the schema for a Codex web.run weather lookup."
  (web--object-schema
   (json-object
    "location" (tool-string-property "Location in Country, Area, City format.")
    "start"    (tool-string-property "Start date in YYYY-MM-DD format. Defaults to today.")
    "duration" (tool-integer-property "Number of days to return. Defaults to 7."))
   :required '("location")))

(-> web--sports-operation-schema () json-object)
(defun web--sports-operation-schema ()
  "Return the schema for a Codex web.run sports lookup."
  (web--object-schema
   (json-object
    "tool"      (web--enum-schema '("sports") "Tool name for sports requests.")
    "fn"        (web--enum-schema '("schedule" "standings")
                                   "Sports function to call.")
    "league"    (web--enum-schema
                  '("nba" "wnba" "nfl" "nhl" "mlb" "epl" "ncaamb" "ncaawb" "ipl")
                  "League to look up.")
    "team"      (tool-string-property
                 "Team to look up, using the common 3 or 4 letter alias used in broadcasts.")
    "opponent"  (tool-string-property "Opponent to use with team when narrowing the lookup.")
    "date_from" (tool-string-property "Start date in YYYY-MM-DD format.")
    "date_to"   (tool-string-property "End date in YYYY-MM-DD format.")
    "num_games" (tool-integer-property "Number of games to return.")
    "locale"    (tool-string-property "Locale for the lookup."))
   :required '("fn" "league")))

(-> web--time-operation-schema () json-object)
(defun web--time-operation-schema ()
  "Return the exact time lookup object accepted by Codex's web.run tool."
  (web--object-schema
   (json-object "utc_offset"
                (tool-string-property "UTC offset formatted like +03:00."))
   :required '("utc_offset")))

(-> web-run-parameters () json-object)
(defun web-run-parameters ()
  "Return the Codex standalone web.run command schema.

The subscription Responses Lite backend reserves this namespaced function and
validates its schema. Keep the command surface aligned with Codex reference
commit ba42e6866cef4baed7ad92c73e6be8cd42e49d8b."
  (web--object-schema
   (json-object
    "search_query"    (web--array-schema
                       (web--search-query-schema)
                       "Query the internet search engine for a given list of queries.")
    "image_query"     (web--array-schema
                       (web--search-query-schema)
                       "Query the image search engine for a given list of queries.")
    "open"            (web--array-schema
                       (web--open-operation-schema)
                       "Open pages by reference id or URL.")
    "click"           (web--array-schema
                       (web--click-operation-schema)
                       "Open links from previously opened pages.")
    "find"            (web--array-schema
                       (web--find-operation-schema)
                       "Find text patterns in pages.")
    "screenshot"      (web--array-schema
                       (web--screenshot-operation-schema)
                       "Take screenshots of PDF pages.")
    "finance"         (web--array-schema
                       (web--finance-operation-schema)
                       "Look up prices for the given stock symbols.")
    "weather"         (web--array-schema
                       (web--weather-operation-schema)
                       "Look up weather forecasts.")
    "sports"          (web--array-schema
                       (web--sports-operation-schema)
                       "Look up sports schedules and standings.")
    "time"            (web--array-schema
                       (web--time-operation-schema)
                       "Get time for the given UTC offsets.")
    "response_length" (web--enum-schema '("short" "medium" "long")
                                         "Set the length of the response to be returned."))))

(-> web--search-endpoint (configuration) string)
(defun web--search-endpoint (configuration)
  "Return the standalone provider search endpoint for CONFIGURATION."
  (let ((endpoint (string-right-trim '(#\/) (config :provider-endpoint configuration))))
    (unless (uiop:string-suffix-p endpoint "/responses")
      (error 'tool-error
             :message   "The configured provider has no standalone search endpoint."
             :tool-name "web.run"))
    (format nil "~A/alpha/search"
            (subseq endpoint 0 (- (length endpoint) (length "/responses"))))))

(-> web--search-content-item (json-object string) (option json-object))
(defun web--search-content-item (item role)
  "Return ITEM as a standalone-search text content item for ROLE, or NIL."
  (let ((type (json-get item "type"))
        (text (json-get item "text")))
    (when (and (stringp type)
               (stringp text)
               (or (and (string= role "user")
                        (member type '("input_text" "text") :test #'string=))
                   (and (string= role "assistant")
                        (member type '("output_text" "text") :test #'string=))))
      (json-object "type" (if (string= role "user")
                              "input_text"
                              "output_text")
                   "text" text))))

(-> web--search-message (json-object) (option json-object))
(defun web--search-message (item)
  "Return a text-only recent user or assistant ITEM for standalone search."
  (let ((role (json-get item "role")))
    (when (and (string= (or (json-get item "type") "") "message")
               (stringp role)
               (member role '("user" "assistant") :test #'string=)
               (vectorp (json-get item "content")))
      (let ((content
              (loop for part across (json-get item "content")
                    for normalized =
                      (and (json-object-p part)
                           (web--search-content-item part role))
                    when normalized
                      collect normalized)))
        (when content
          (json-object "type" "message"
                       "role" role
                       "content" (coerce content 'vector)))))))

(-> web--search-history (conversation configuration) vector)
(defun web--search-history (conversation configuration)
  "Return Codex's recent visible text-message tail for standalone web search."
  (let* ((messages
           (loop for item in (conversation-input-items-for-family
                              conversation
                              (model-family (config :model configuration)
                                            (config :model-provider configuration)))
                 for message = (and (json-object-p item)
                                    (web--search-message item))
                 when message
                   collect message))
         (user-indices
           (loop for message in messages
                 for index from 0
                 when (string= (json-get message "role") "user")
                   collect index))
         (start (if (>= (length user-indices) 2)
                    (nth (- (length user-indices) 2) user-indices)
                    0)))
    (coerce (nthcdr start messages) 'vector)))

(-> web--external-web-access (configuration) (or (eql t) (satisfies json-false-p) string))
(defun web--external-web-access (configuration)
  "Return Codex's standalone endpoint access value for CONFIGURATION."
  (cond
    ((string= (config :web-search-mode configuration) "cached")
     (json-false))
    ((string= (config :web-search-mode configuration) "indexed")
     "indexed")
    ((string= (config :web-search-mode configuration) "live")
     t)
    (t
     (json-false))))

(defgeneric provider-web-search-request-headers (provider credentials conversation)
  (:documentation
   "Return authenticated standalone web search headers for PROVIDER."))

(defmethod provider-web-search-request-headers
    ((provider codex-subscription-provider)
     (credentials oauth-credentials)
     (conversation conversation))
  "Return ChatGPT standalone web search headers."
  (provider--codex-request-headers
   provider credentials conversation :accept "application/json"))

(defmethod provider-web-search-request-headers
    ((provider grok-subscription-provider)
     (credentials oauth-credentials)
     (conversation conversation))
  "Return Grok standalone web search headers."
  (grok--request-headers
   provider credentials conversation :accept "application/json"))

(-> web--search-request (tool-context model-provider json-object) json-object)
(defun web--search-request (context provider commands)
  "Return the Codex standalone search request represented by COMMANDS."
  (let ((configuration (tool-context-configuration context)))
    (json-object
     "id" (provider-session-id provider)
     "model" (config :model configuration)
     "input" (web--search-history (tool-context-conversation context)
                                  configuration)
     "commands" commands
     "settings"
     (json-object
      "allowed_callers" (json-array "direct")
      "external_web_access" (web--external-web-access configuration))
     "max_output_tokens" 4000)))

(defmethod tool-execute ((tool web-run-tool) (context tool-context) (arguments hash-table))
  "Run one Codex-compatible standalone web command batch."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (provider      (provider-create configuration))
         (conversation  (tool-context-conversation context)))
    (when (string= (config :web-search-mode configuration) "disabled")
      (error 'tool-error
             :message   "Provider web search is disabled by configuration."
             :tool-name "web.run"))
    (with-credentials (credentials (provider-credential-manager provider))
      (multiple-value-bind (body status headers)
          (dexador:post (web--search-endpoint configuration)
                        :headers (provider-web-search-request-headers
                                  provider credentials conversation)
                        :content (json-encode
                                  (web--search-request context provider arguments)))
        (declare (ignore headers))
        (unless (= status 200)
          (error 'tool-error
                 :message   (format nil "Provider web search returned HTTP ~D." status)
                 :tool-name "web.run"))
        (let ((output (json-get (json-decode body) "output")))
          (unless (non-empty-string-p output)
            (error 'tool-error
                   :message   "Provider web search returned no text output."
                   :tool-name "web.run"))
          (tool-success output))))))

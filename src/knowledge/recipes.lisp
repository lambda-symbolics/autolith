(in-package #:autolith)

;;;; -- Starter Recipes --

(defparameter *knowledge-recipes*
  '((:recipe
     :id          "appointments"
     :description "Calculate departure times from event start times, travel times, and early-arrival buffers."
     :predicates  ((:predicate "event.start"
                    :arguments ((:name "event" :type :entity) (:name "time" :type :instant))
                    :key       ("event")
                    :template  "{event} starts at {time}")
                   (:predicate "event.travel"
                    :arguments ((:name "event" :type :entity) (:name "duration" :type :duration))
                    :key       ("event")
                    :template  "travel to {event} takes {duration}")
                   (:predicate "event.buffer"
                    :arguments ((:name "event" :type :entity) (:name "duration" :type :duration))
                    :key       ("event")
                    :template  "arrive {duration} early for {event}")
                   (:predicate "event.departure"
                    :arguments ((:name "event" :type :entity) (:name "time" :type :instant))
                    :key       ("event")
                    :template  "leave for {event} at {time}"))
     :rules       ((:id          "departure"
                    :premises    (("event.start" "?event" "?start")
                                  ("event.travel" "?event" "?travel")
                                  ("event.buffer" "?event" "?buffer"))
                    :bindings    (("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                    :conclusion  ("event.departure" "?event" "?leave")
                    :description "Leave the travel time plus the buffer before an event starts.")))
    (:recipe
     :id          "service-dependencies"
     :description "Services, their status, and what they depend on; a service is at risk while a dependency is down."
     :predicates  ((:predicate "service.status"
                    :arguments ((:name "service" :type :entity) (:name "status" :type :string))
                    :key       ("service")
                    :template  "{service} is {status}")
                   (:predicate "service.depends-on"
                    :arguments ((:name "service" :type :entity) (:name "dependency" :type :entity))
                    :template  "{service} depends on {dependency}")
                   (:predicate "service.at-risk"
                    :arguments ((:name "service" :type :entity) (:name "dependency" :type :entity))
                    :template  "{service} is at risk because {dependency} is down"))
     :rules       ((:id          "dependency-down"
                    :premises    (("service.depends-on" "?service" "?dependency")
                                  ("service.status" "?dependency" "down"))
                    :conclusion  ("service.at-risk" "?service" "?dependency")
                    :description "A service is at risk while something it depends on is down."))))
  "The starter recipes a person or the model can use as a first vocabulary.")


;;;; -- Public Functions --

(-> knowledge-recipe (string) (option list))
(defun knowledge-recipe (identifier)
  "Return the starter recipe named IDENTIFIER, or NIL."
  (find identifier *knowledge-recipes*
        :key  (lambda (recipe) (getf (rest recipe) :id))
        :test #'string=))

(-> knowledge-recipe-identifiers () list)
(defun knowledge-recipe-identifiers ()
  "Return the names of the starter recipes, in catalog order."
  (mapcar (lambda (recipe) (getf (rest recipe) :id)) *knowledge-recipes*))

(-> knowledge-recipe-schemas (list) list)
(defun knowledge-recipe-schemas (recipe)
  "Return RECIPE's predicates as schemas ready to define."
  (mapcar (lambda (predicate)
            (knowledge-make-schema :predicate (second predicate)
                                   :arguments (getf (rest (rest predicate)) :arguments)
                                   :key       (getf (rest (rest predicate)) :key)
                                   :template  (getf (rest (rest predicate)) :template)))
          (getf (rest recipe) :predicates)))

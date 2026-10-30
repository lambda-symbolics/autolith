;;;; -- Shared Supervisor Types --

(in-package #:autolith-supervisor)

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))

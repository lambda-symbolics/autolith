(require :asdf)

;; Nix's SBCL closure already contains Autolith's locked dependency graph.
(asdf:initialize-source-registry)

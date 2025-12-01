(defsystem "aoc"
  :version "0.0.0"
  :depends-on (:transducers :parcom :arrow-macros :filepaths)
  :serial t
  :components ((:module "src" :components ((:file "package")))))

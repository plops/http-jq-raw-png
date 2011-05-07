(asdf:defsystem serv
  :depends-on (:sb-posix :sb-bsd-sockets)
  :components
  ((:file "packages")
   (:file "png" :depends-on ("packages"))
   (:file "serv" :depends-on ("packages"))))

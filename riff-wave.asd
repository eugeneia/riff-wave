;;;; System definition for RIFF-WAVE.

(defpackage riff-wave-asd
  (:documentation
   "System definition for RIFF-WAVE.")
  (:use :cl :asdf))

(in-package :riff-wave-asd)

(defsystem riff-wave
  :description "Functions to read and write WAVE files."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "packages")
	       (:file "constants" :depends-on ("packages"))
	       (:file "write-wave" :depends-on ("packages"
						"constants")))
  :depends-on ("bytes"))

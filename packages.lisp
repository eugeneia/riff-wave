;;;; Package definitions for RIFF-WAVE.

(defpackage :riff-wave.constants
  (:documentation
   "Constants of the WAVE format.")
  (:use :cl)
  (:export :+riff-id+
	   :+wave-id+
	   :+fmt-subchunk-id+
	   :+fmt-subchunk-size+
	   :+pcm-format-id+
	   :+data-subchunk-id+))

(defpackage :riff-wave.write
  (:documentation
   "Functions to write WAVE files.")
  (:use :cl
	:riff-wave.constants
	:bytes)
  (:export :write-wave-header
	   :write-sample))

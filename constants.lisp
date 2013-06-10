;;;; Constants according to the WAVE format as by
;;;; https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
;;;; Note: Identifiers were converted to little endian order.

(in-package :riff-wave.constants)

(defconstant +riff-id+ #x46464952
  "'RIFF' header identifier.")

(defconstant +wave-id+ #x45564157
  "'WAVE' header identifier.")

(defconstant +fmt-subchunk-id+ #x20746d66
  "'fmt ' header identifier.")

(defconstant +fmt-subchunk-size+ 16
  "'fmt ' header size.")

(defconstant +pcm-format-id+ 1
  "PCM format identifier.")

(defconstant +data-subchunk-id+ #x61746164
  "'data' header identifier.")

(defconstant +header-size+ 36
  "Size of header chunk.")

(defconstant +8bit-max+ 255
  "Maximum value for eight bit samples.")

(defconstant +16bit-max-2c+ 32767
  "Maximum value for 16 bit two's complement signed samples.")

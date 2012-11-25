;;;; Export integer arrays in WAVE format.

(in-package :riff-wave.write)

(defconstant +header-size+ 36
  "Size of header chunk.")

(defconstant +8bit-max+ 255
  "Maximum value for eight bit samples.")

(defconstant +16bit-max-2c+ 32767
  "Maximum value for 16 bit two's complement signed samples.")

(defun write-wave-header (sample-rate sample-size n-channels length
			  stream)
  "Write PCM WAVE file header to STREAM as specified by SAMPLE-RATE in
hertz, SAMPLE-SIZE in bytes, the number of channels N-CHANNELS and the
number of samples LENGTH."
  (flet ((write-two-bytes (integer)
	   (write-bytes integer 2 stream))
	 (write-four-bytes (integer)
	   (write-bytes integer 4 stream)))
    (let* ((block-alignment (* n-channels sample-size))
	   (data-size (* block-alignment length))
	   (byte-rate (* block-alignment sample-rate)))
      ;; RIFF header id
      (write-four-bytes +riff-id+)
      ;; Chunk size
      (write-four-bytes (+ +header-size+ data-size))
      ;; WAVE header id
      (write-four-bytes +wave-id+)
      ;; Format header id
      (write-four-bytes +fmt-subchunk-id+)
      ;; Format subchunk size
      (write-four-bytes +fmt-subchunk-size+)
      ;; Audio format (PCM)
      (write-two-bytes +pcm-format-id+)
      ;; Number of channels
      (write-two-bytes n-channels)
      ;; Sample rate
      (write-four-bytes sample-rate)
      ;; Byte rate
      (write-four-bytes byte-rate)
      ;; Block alignment
      (write-two-bytes block-alignment)
      ;; Bits per sample
      (write-two-bytes (* sample-size 8))
      ;; Data header id
      (write-four-bytes +data-subchunk-id+)
      ;; Data length in bytes
      (write-four-bytes data-size)
      ;; Actual data should follow, return nothing
      (values))))

(defun write-sample (sample sample-size stream)
  "Write SAMPLE (a number ranging from -1 to 1) represented by
SAMPLE-SIZE bytes to STREAM."
  (ecase sample-size
    (1 (write-byte (ceiling (* (/ (1+ sample) 2) +8bit-max+)) stream))
    (2 (let* ((float-value (* sample +16bit-max-2c+))
	      (value (if (> 0 float-value)
			 (ceiling float-value)
			 (floor float-value))))
	 (write-byte (ldb (byte 8 0) value) stream)
	 (write-byte (ldb (byte 8 8) value) stream)))))

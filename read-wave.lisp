;;;; Functions to read WAVE files.

(in-package :riff-wave.read)

(defun read-wave-header (stream)
  "Read WAVE header from STREAM and return sample-rate, sample-size,
  n-channels and length."
  (flet ((read-two-bytes ()
	   (read-bytes stream 2))
         (read-four-bytes ()
	   (read-bytes stream 4)))
    (let* ((riff-id
            (unless (= +riff-id+ (read-four-bytes))
              (error "RIFF magic number mismatch (!= #x~x)." +riff-id+)))
           (chunk-size (read-four-bytes))
           (wave-id
            (unless (= +wave-id+ (read-four-bytes))
              (error "WAVE magic number mismatch (!= #x~x)." +wave-id+)))
           (fmt-subchunk-id
            (unless (= +fmt-subchunk-id+ (read-four-bytes))
              (error "FMT subchunk magic number mismatch (!= #x~x)."
                     +fmt-subchunk-id+)))
           (fmt-subchunk-size (read-four-bytes))
           (audio-format
            (unless (= +pcm-format-id+ (read-two-bytes))
              (error "Audio format is unknown (not PCM).")))
           (n-channels (read-two-bytes))
           (sample-rate (read-four-bytes))
           (byte-rate (read-four-bytes))
           (block-alignment (read-two-bytes))
           (sample-size (/ (read-two-bytes) 8)) ; Stored in n-bits
           (data-subchunk-id
            (unless (= +data-subchunk-id+ (read-four-bytes))
              (error "DATA subchunk magic number mismatch (!= #x~x)"
                     +data-subchunk-id+)))
           ;; LENGTH is byte length divided by N-CHANNELS and
           ;; SAMPLE-SIZE.
           (length (/ (read-four-bytes) n-channels sample-size)))
      ;; Ignore superfluous header fields.
      (declare (ignore riff-id wave-id fmt-subchunk-id chunk-size
                       fmt-subchunk-size audio-format byte-rate
                       block-alignment data-subchunk-id))
      ;; STREAM should now be at the sample body. Return the sampling
      ;; parameters.
      (values sample-rate sample-size n-channels length))))

(defun read-sample (stream sample-size)
  "Read sample of SAMPLE-SIZE from STREAM."
  (* 2 (ecase sample-size
         (1 (/ (read-byte stream) +8bit-max+))
         (2 (/ (read-bytes stream 2) +16bit-max-2c+ 2)))))

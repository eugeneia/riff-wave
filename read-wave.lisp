;;;; Functions to read WAVE files.

(in-package :riff-wave.read)

(defun read-wave-header (stream)
  "Read WAVE header from STREAM and return sample-rate, sample-size,
  n-channels and length."
  (flet ((read-two-bytes ()
	   (read-bytes stream 2))
         (read-four-bytes ()
	   (read-bytes stream 4))
         (skip-bytes (n)
           (dotimes (i n)
             (declare (ignore i))
             (read-byte stream))))
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
           ;; Seek to data chunk.
           (other-chunks (loop while (not (= +data-subchunk-id+
                                             (read-four-bytes)))
                            do (let ((chunk-size (read-four-bytes)))
                                 (skip-bytes chunk-size))))
           ;; LENGTH is byte length divided by N-CHANNELS and
           ;; SAMPLE-SIZE.
           (length (/ (read-four-bytes) n-channels sample-size)))
      ;; Ignore superfluous header fields.
      (declare (ignore riff-id wave-id fmt-subchunk-id chunk-size
                       fmt-subchunk-size audio-format byte-rate
                       block-alignment other-chunks))
      ;; STREAM should now be at the sample body. Return the sampling
      ;; parameters.
      (values sample-rate sample-size n-channels length))))

(defun sample-1 (sample)
  "Return value for SAMPLE represented in one unsigned byte."
  (1- (/ sample +8bit-max+ 1/2)))

(defun sample-2 (sample)
  "Return value for SAMPLE represented in two's complement of two byte
size."
  (/ (if (> sample +16bit-max-2c+)
         ;; Negative
         (- sample #.(expt 2 16)) ; two's complement: 2^bits-N
         ;; Positive
         sample)
     +16bit-max-2c+))

(defun read-sample (stream sample-size)
  "Read sample of SAMPLE-SIZE from STREAM."
  (ecase sample-size
    (1 (sample-1 (read-byte stream)))
    (2 (sample-2 (read-bytes stream 2)))))

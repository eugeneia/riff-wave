;;;; Functions to read WAVE files.

(in-package :riff-wave.read)

(defun read-wave-header (stream)
  "→ _sample-rate_, _sample-size_, _n-channels_, _length_

   *Arguments and Values:*

   _stream_—a _binary input stream_.

   _sample-rate_—the sample rate in Hertz specified by the WAVE
   header. Represented as a positive _integer_.

   _sample-size_—the sample size specified by the WAVE header. May be be
   1 or 2 indicating 8-bit or 16-bit samples respectively.

   _n-channels_—the number of channels specified by the WAVE header.
   Represented as a positive _integer_.

   _length_—the contained number of samples per channel specified by the
   WAVE header. Represented as an unsigned _integer_.

   *Description:*

   {read-wave-header} reads a PCM WAVE header at _stream_ and returns the
   _sample-rate_, _sample-size_, _n-channels_, and _length_ of the WAVE
   stream specified by the header. _Stream_ is advanced to the beginning
   of the first sample and {read-sample} may be used to read _length_
   times _n-samples_ of _sample-size_ from _stream_.

   E.g. if a WAVE stream contains two channels and has a length of four,
   then there should be eight samples in the stream, with each even
   sample belonging to the first channel and each odd sample belonging to
   the second channel of the stream.

   *Exceptional Situations:*

   Signals an error of _type_ {error} if _stream_ does not contain a
   RIFF/WAVE header using the PCM audio format."
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
  "→ _sample_

  *Arguments and Values:*

  _stream_—a _binary input stream_.

  _sample-size_—the sample size of the WAVE _stream_.

  _sample_—either an {(unsigned-byte 8)} or an {(unsigned-byte 16)}
  depending on _sample-size_.

  *Description:*

  {read-sample} reads and returns a sample of _sample-size_ from
  _stream_."
  (ecase sample-size
    (1 (sample-1 (read-byte stream)))
    (2 (sample-2 (read-bytes stream 2)))))

;;;; Export integer arrays in WAVE format.

(in-package :riff-wave.write)

(defun write-wave-header (sample-rate sample-size n-channels length
			  stream)
  "*Arguments and Values:*

   _sample-rate_—an {(unsigned-byte 32)} denoting the sample rate in
   Hertz.

   _sample-size_—one for 8-bit samples; Two for 16-bit samples.

   _n-channels_—a positive _integer_ denoting the number of channels.

   _length_—an unsigned _integer_ denoting the number of samples per
   channel following the WAVE header.

   _stream_—a _binary output stream_.

   *Description:*

   {write-wave-header} writes the PCM WAVE header specified by
   _sample-rate_, _sample-size_, _n-channels_ and _length_ to
   _stream_. {write-sample} should be used to write _n-channels_ times
   _length_ samples of _sample-size_ to _stream_."
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
  "*Arguments and Values:*

   _sample_—the sample value represented as a {(real -1 1)}.

   _sample-size_—one to write an 8-bit sample; Two to write a 16-bit
   sample.

   _stream_—a _binary output stream_.

   {write-sample} writes _sample_ encoded in _sample-size_ bytes to
  _stream_."
  (ecase sample-size
    (1 (write-byte (ceiling (* (/ (1+ sample) 2) +8bit-max+)) stream))
    (2 (let* ((float-value (* sample +16bit-max-2c+))
	      (value (if (> 0 float-value)
			 (ceiling float-value)
			 (floor float-value))))
         (write-bytes value 2 stream)))))

(uiop:define-package :advent/2021/src/day-16
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:pq #:priority-queue))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-16)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/16.txt")
    (car <>)
    (bit-smasher:hex->bits <>)
    ))

(defun str-to-bits (str)
  (-<> str
    (loop for c across <> collecting (parse-integer (string c)))
    (make-array (length <>) :element-type 'bit :initial-contents <>)))

(defun parse-literal (binary ostart)
  (loop with continue? = t
        with bits = (make-array 0 :element-type 'bit)
        with start = (+ ostart 6)
        while continue?
        do (setf bits (concatenate 'bit-vector bits
                                   (subseq binary (1+ start) (+ 5 start))))
           (setf continue? (= 1 (aref binary start)))
           (incf start 5)
        finally
           (return (values (bit-smasher:bits->int bits)
                           start
                           ;; (* 4 (ceiling (/ start 4)))
                           ))))

(defun parse-packet (binary start)
  (let* ((version-binary (subseq binary start (+ 3 start)))
         (id-binary (subseq binary (+ 3 start) (+ 6 start)))
         (version (bit-smasher:bits->int version-binary))
         (id (bit-smasher:bits->int id-binary)))
    ;; (print version)
    (if (= id 4)
        (return-from parse-packet (multiple-value-bind (packet packet-end) (parse-literal binary start)
                                    (format t "Detected literal packet in range ~a,~a w/ value ~a~%" start packet-end packet)
                                    (values (list version id packet)
                                            packet-end)))
        (let ((length-type-id (aref binary (+ start 6))))
          (if (zerop length-type-id)
              (loop with subpackets-total-length
                      = (bit-smasher:bits->int (subseq binary (+ start 7) (+ start 7 15)))
                        initially (format t "Aggregate packet w/ start ~a, total subpacket lenght of ~a~%"
                                          start subpackets-total-length)
                    with end = (+ start 7 15)
                    while (/= (- end (+ start 7 15)) subpackets-total-length)
                    for (packet packet-end) = (multiple-value-list (parse-packet binary end))
                    collecting packet into packets
                    do (format t "Just got packet ~a with end ~a~%" packet packet-end)
                       (format t "The diff with start: ~a is ~a for an expected total subpacket length of ~a~%" start (- packet-end (+ start 7 15)) subpackets-total-length)
                       (setf end packet-end)
                    finally
                       (return (values (list version id packets)
                                       end)))
              (loop with num-subpackets
                      = (bit-smasher:bits->int (subseq binary (+ start 7) (+ start 7 11)))
                    with end = (+ start 7 11)
                      initially
                         (format t "Aggregate packet w/ ~a subpackets~%"
                                 num-subpackets)
                    for i below num-subpackets
                    for (packet packet-end) = (multiple-value-list (parse-packet binary end))
                    collecting packet into packets
                    do (setf end packet-end)
                    finally
                       (return (values (list version id packets)
                                       end))))))))

(defun parse-packets (binary)
  (parse-packet binary 0)
  ;; (loop with packet-end = 0
  ;;       with packets = nil
  ;;       while (< packet-end (length binary))
  ;;       do (progn (format t "Making root packet at ~a~%" packet-end)
  ;;                 (multiple-value-bind (packet end) (parse-packet binary packet-end)
  ;;                   (setf packets (append packets packet))
  ;;                   (setf packet-end end)))
  ;;       finally
  ;;          (return packets))
  )

(defparameter *packets* (parse-packets *input*))

(defun packet-valsum (packet)
  (destructuring-bind (version id subpackets) packet
    (if (numberp subpackets)
        version
        (+ version (reduce #'+ (mapcar #'packet-valsum subpackets))))))

(defun eval-packets (packet)
  (destructuring-bind (version id subpackets) packet
    (ecase id
      (4 subpackets)
      (0 (reduce #'+ (mapcar #'eval-packets subpackets)))
      (1 (reduce #'* (mapcar #'eval-packets subpackets)))
      (2 (reduce #'min (mapcar #'eval-packets subpackets)))
      (3 (reduce #'max (mapcar #'eval-packets subpackets)))
      (5 (if (reduce #'> (mapcar #'eval-packets subpackets)) 1 0))
      (6 (if (reduce #'< (mapcar #'eval-packets subpackets)) 1 0))
      (7 (if (reduce #'= (mapcar #'eval-packets subpackets)) 1 0)))))

(uiop:define-package :advent/2021/src/day-16
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:pq #:priority-queue))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-16)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/16.txt")
    (car <>)
    (bit-smasher:hex->bits <>)
    ))

(defun str-to-bits (str)
  (-<> str
    (loop for c across <> collecting (parse-integer (string c)))
    (make-array (length <>) :element-type 'bit :initial-contents <>)))

(defun parse-literal (binary start)
  (loop with continue? = t
        with bits = (make-array 0 :element-type 'bit)
        with start = (+ start 6)
        while continue?
        do (setf bits (concatenate 'bit-vector bits
                                   (subseq binary (1+ start) (+ 5 start))))
           (setf continue? (= 1 (aref binary start)))
           (incf start 5)
        finally
           (format t "Would have returned ~a~%" start)
           (return (values (bit-smasher:bits->int bits)
                           (* 4 (ceiling (/ start 4)))))))

(defun parse-packet (binary start)
  (let* ((version-binary (subseq binary start (+ 3 start)))
         (id-binary (subseq binary (+ 3 start) (+ 6 start)))
         (version (bit-smasher:bits->int version-binary))
         (id (bit-smasher:bits->int id-binary)))
    (if (= id 4)
        (return-from parse-packet (multiple-value-bind (packet packet-end) (parse-literal binary start)
                                    (format t "Detected literal packet in range ~a,~a w/ value ~a~%" start packet-end packet)
                                    (values (list version id packet)
                                            packet-end)))
        (let ((length-type-id (aref binary (+ start 6))))
          (if (zerop length-type-id)
              (loop with subpackets-total-length
                      = (bit-smasher:bits->int (subseq binary (+ start 7) (+ start 7 15)))
                        initially (format t "Aggregate packet w/ total subpacket lenght of ~a~%"
                                          subpackets-total-length)
                    with start = (+ start 7 15)
                    with end = start
                    while (/= (- end start) subpackets-total-length)
                    for (packet packet-end) = (multiple-value-list (parse-packet binary end))
                    collecting packet into packets
                    do (setf end packet-end)
                    finally
                       (return (values (list version id packets)
                                       end)))
              (loop with num-subpackets
                      = (bit-smasher:bits->int (subseq binary (+ start 7) (+ start 7 11)))
                    with end = (+ start 7 11)
                    for i below num-subpackets
                    for (packet packet-end) = (multiple-value-list (parse-packet binary end))
                    collecting packet into packets
                    do (setf end packet-end)
                    finally
                       (return (values (list version id packets)
                                       end))))))))

(defun parse-packets (binary)
  (loop with start = 0
        for (packet packet-end) = (multiple-value-list (parse-packet binary start))
        collecting packet into packet-list
        do (when (= (length binary) (setf start packet-end))
             (return packet-list))))

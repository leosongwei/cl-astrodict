;; Usage: sbcl --load cl-astrodict.lisp

(defun make-dynamic-string ()
  (make-array 0 :adjustable t
              :fill-pointer 0
              :element-type 'character))
;; (let ((s (make-dynamic-string)))
;;   (vector-push-extend #\a s)
;;   (vector-push-extend #\b s)
;;   s)

(defun int-oct-vector (uint32)
  (let* ((a0 (mod (ash uint32 -24) 256))
         (a1 (mod (ash uint32 -16) 256))
         (a2 (mod (ash uint32 -8) 256))
         (a3 (mod uint32 256)))
    (make-array 4 :element-type '(unsigned-byte 8)
                :initial-contents `(,a0 ,a1 ,a2 ,a3))))
;; (int-oct-vector #x0A0B0C0D) ==> #(10 11 12 13)

(defun write-oct (vector stream)
  (dotimes (i (length vector))
    (write-byte (aref vector i) stream))
  (length vector))

;;;; English to Chinese

(let ((list-dict nil))
  (with-open-file (s "astrodict_160626ec.txt" :direction :input
                     :external-format :utf-8)
    (do ((line (read-line s nil nil) (read-line s nil nil)))
        ((eq line nil) nil)
      (if (> (length line) 3)
          (let ((word (make-dynamic-string))
                (inter (make-dynamic-string))
                (wordp t))
            (dotimes (i (length line))
              (case (aref line i)
                (#\tab (setf wordp nil))
                (otherwise
                 (vector-push-extend (aref line i) (if wordp
                                                       word
                                                       inter)))))
            (push (cons word inter) list-dict)))))
  (defparameter *dict-ec* (reverse list-dict)))

(with-open-file (s-idx "astrodict-ec.idx"
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
  (with-open-file (s-dict "astrodict-ec.dict"
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (let ((dict-cursor 0)
          (idx-length 0))
      (dolist (pair *dict-ec*)
        (let* ((word (car pair))
               (inter (cdr pair))
               (word-oct (sb-ext:string-to-octets word :external-format :utf-8))
               (inter-oct (sb-ext:string-to-octets inter :external-format :utf-8))
               (length-inter (length inter-oct)))
          (write-oct inter-oct s-dict)
          (incf idx-length (write-oct word-oct s-idx))
          (write-byte 0 s-idx) (incf idx-length)
          (let* ((offset-oct (int-oct-vector dict-cursor))
                 (length-oct (int-oct-vector length-inter)))
            (incf idx-length (write-oct offset-oct s-idx))
            (incf idx-length (write-oct length-oct s-idx)))
          (incf dict-cursor length-inter)))
      (with-open-file (s-ifo "astrodict-ec.ifo" :direction :output
                             :external-format :utf-8
                             :if-exists :supersede)
        (format s-ifo "StarDict's dict ifo file~%")
        (format s-ifo "version=2.4.2~%")
        (format s-ifo "wordcount=~A~%" (length *dict-ec*))
        (format s-ifo "idxfilesize=~A~%" idx-length)
        (format s-ifo "bookname=~A~%" "英汉天文学名词数据库")
        (format s-ifo "author=~A~%" "中国天文学会天文学名词审定委员会")
        (format s-ifo "description=~A~%" "英汉天文学名词数据库")
        (format s-ifo "sametypesequence=h~%")))))

;;;; Chinese to English

(let ((list-dict nil))
  (with-open-file (s "astrodict_160626ce.txt" :direction :input
                     :external-format :utf-8)
    (do ((line (read-line s nil nil) (read-line s nil nil)))
        ((eq line nil) nil)
      (if (> (length line) 3)
          (let ((word (make-dynamic-string))
                (inter (make-dynamic-string))
                (wordp t))
            (dotimes (i (length line))
              (case (aref line i)
                (#\tab (setf wordp nil))
                (otherwise
                 (vector-push-extend (aref line i) (if wordp
                                                       word
                                                       inter)))))
            (push (cons word inter) list-dict)))))
  (defparameter *dict-ec* (reverse list-dict)))

(with-open-file (s-idx "astrodict-ce.idx"
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
  (with-open-file (s-dict "astrodict-ce.dict"
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (let ((dict-cursor 0)
          (idx-length 0))
      (dolist (pair *dict-ec*)
        (let* ((word (car pair))
               (inter (cdr pair))
               (word-oct (sb-ext:string-to-octets word :external-format :utf-8))
               (inter-oct (sb-ext:string-to-octets inter :external-format :utf-8))
               (length-inter (length inter-oct)))
          (write-oct inter-oct s-dict)
          (incf idx-length (write-oct word-oct s-idx))
          (write-byte 0 s-idx) (incf idx-length)
          (let* ((offset-oct (int-oct-vector dict-cursor))
                 (length-oct (int-oct-vector length-inter)))
            (incf idx-length (write-oct offset-oct s-idx))
            (incf idx-length (write-oct length-oct s-idx)))
          (incf dict-cursor length-inter)))
      (with-open-file (s-ifo "astrodict-ce.ifo" :direction :output
                             :external-format :utf-8
                             :if-exists :supersede)
        (format s-ifo "StarDict's dict ifo file~%")
        (format s-ifo "version=2.4.2~%")
        (format s-ifo "wordcount=~A~%" (length *dict-ec*))
        (format s-ifo "idxfilesize=~A~%" idx-length)
        (format s-ifo "bookname=~A~%" "汉英天文学名词数据库")
        (format s-ifo "author=~A~%" "中国天文学会天文学名词审定委员会")
        (format s-ifo "description=~A~%" "汉英天文学名词数据库")
        (format s-ifo "sametypesequence=h~%")))))

(sb-ext:exit)

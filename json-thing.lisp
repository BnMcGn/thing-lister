(in-package :json-thing-lister)


(defun create-json-thing-lister (url-base)
  (let ((spliturl (split-sequence #\/ url-base)))
    (when (= 0 (length (car spliturl)))
      (setf spliturl (cdr spliturl)))
    (let ((baselen (length spliturl)))
      (lambda ()
        (funcall
         (cdr (assoc
               (nth baselen *regular-web-input*)
               '(list
                 ("thing" . #'json-thing)
                 ("available-things" . #'json-available)
                 ("")
                )))
        (let ((command (nth baselen *regular-web-input*)))
          (cond
            ((string-equal command "thing")
             ))))
        (cond  )
      *regular-web-input*
      *key-web-input*
      ))

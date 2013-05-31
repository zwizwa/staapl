; Emacs code

(defvar staapl_app)
(setq staapl_app "/home/tom/staapl/app/")



(defun staapl-usb () (interactive)
  (staapl-usb-process)  ;; make sure it's there
  (switch-to-buffer "*staapl-usb*"))

(defun staapl-usb-process ()
  "Spawn fake usb."
  (let ((process (get-buffer-process "*staapl-usb*")))
    (if process process
      (let ((buffer (make-comint
                     "staapl-usb"
                     "mzscheme"
                     nil  ;; initial input file
                     (concat staapl_app 
                             "pk2-2550-48.dict"
                             ; "pk2-picstamp.dict"
                             ))))
        (get-buffer-process buffer)))))



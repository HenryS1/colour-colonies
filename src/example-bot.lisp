(defpackage :example-bot (:use :cl :herodotus :trivia :trivia.ppcre :iterate))

(define-json-model player-command (player command details))
(define-json-model game-input ((previous-turn player-command) player-positions))

(defun read-input ()
  (let ((input (game-input:from-json (read-line t nil nil))))
    (read-line t nil nil)
    input))

(defclass state ()
  ((built-squares :accessor built-squares 
                  :initarg :built-squares 
                  :iniform (make-hash-table :test 'equal))
   (player-positions :accessor player-positions
                     :initarg :player-positions 
                     :initform (make-hash-table :test 'equal))))

(defun update-state (previous-turn state)
  (iter (for player-command in-vector previous-turn)
        (cond ((string= (command player-command) "build")
               (let ((player-position (gethash (player player-command) (player-positions state))))
                 (setf (gethash player-position (built-squares state))
                       (player player-command))))
              ((string= (command player-command) "move")
               (let ((player-position (gethash (player player-command) (player-positions state))))
                 (remhash player-position (player-positions state))
                 (setf (gethash (coerce 'list (gethash "ending-position" (details player-command)))
                                (player-positions state))
                       (player player-command))))
              (t ()))))

(defun initialise-state (player-positions state)
  (setf (player-positions state) player-positions))

(defun run ()
  (let ((state (make-instance 'state)))
    (iter (for input = (read-input))
          (if (and (previous-turn state) (> (length (previous-turn state)) 0))
              (update-state (previous-turn input) state)
              (initialise-state (player-positions input) state))
          (select-move state))))

(defpackage colour-colonies
  (:use :cl :alexandria :trivia 
        :herodotus
        :trivia.ppcre :runtime 
        :n-player-game :iterate
        :bind))

(in-package :colour-colonies)

(defclass player ()
  ((player-position :accessor player-position :initarg :player-position)
   (name :accessor name :initarg :name)
   (id :accessor id :initarg :id)
   (colour :accessor colour :initarg :colour)
   (score :accessor score :initarg :score :initform 0)))

(defclass colour-colonies (game-state)
  ((turns-remaining :accessor turns-remaining :initarg :turns-remaining :initform 200)
   (current-turn :accessor current-turn :initarg :current-turn :initform 0)
   (rows :accessor rows :initarg :rows :initform 20)
   (cols :accessor cols :initarg :cols :initform 40)
   (player-ids :accessor player-ids :initarg :player-ids :initform (make-hash-table :test 'equal))
   (move-history :accessor move-history :initarg :move-history :initform nil)
   (players :accessor players :initarg :players :initform (make-hash-table :test 'equal))
   (squares :accessor squares :initform (make-hash-table :test 'equal))))

(defun player-positions (game)
  (iter (for (pos player) in-hashtable (players game))
        (collect (cons (colour player) (player-position player)))))

(defun coord-is-open (coord game)
  (destructuring-bind (row . column) coord
    (and (not (gethash coord (squares game)))
         (not (gethash coord (players game)))
         (>= (- (cols game) 1) column 0)
         (>= (- (rows game) 1) row 0))))

(defun has-open-neighbour (player game)
  (iter (with (row . col) = (player-position player))
        (for r from (- row 1) to (+ row 1))
        (iter (for c from (- col 1) to (+ col 1))
              (for coord = (cons r c))
              (when (coord-is-open coord game)
                (return-from has-open-neighbour t)))
        (finally (return nil))))

(defmethod player-can-take-turn ((game colour-colonies) bot-id)
  (let ((player (gethash bot-id (player-ids game))))
    (or (not (gethash (player-position player) (squares game)))
        (has-open-neighbour player game))))

(defun some-player-can-take-turn (game)
  (iter (for (player-position player) in-hashtable (players game))
        (when (player-can-take-turn player) (leave t))
        (finally (return nil))))

(defmethod is-finished? ((game colour-colonies))
  (with-slots (squares turns-remaining rows cols players) game
    (or (= turns-remaining 0)
        (= (hash-table-count squares) (* rows cols))
        (not (some-player-can-take-turn game)))))

(define-json-model player-command (player command details))

(defun build (player game)
  (if (not (gethash (player-position player) (squares game)))
      (progn
        (push (make-instance 'player-command :player (colour player) 
                             :command 'build
                             :details (alist-hash-table (list (cons 'position
                                                                    (player-position player)))))
              (car (move-history game)))
        (setf (gethash (player-position player) (squares game)) (colour player)))
      (format t "Can't build on occupied square ~a~%" (player-position player))))

(defun move (player row column game)
  (let ((coord (cons row column)))
    (if (coord-is-open coord game)
        (progn (push (make-instance 'player-command :player (colour player)
                                    :command 'move
                                    :details (alist-hash-table 
                                              (list (cons 'starting-position 
                                                          (player-position player))
                                                    (cons 'ending-position
                                                          coord))))
                     (car (move-history game)))
               (remhash (player-position player) (players game))
               (setf (gethash coord (players game)) player))
        (format t "Can't move to occupied square ~a~%" coord))))

(defun null-move (player game)
  (push (make-instance 'player-command :player (colour player)
                       :command 'null-move
                       :details (make-hash-table))
        (car (move-history game))))

(defmethod update-game-state ((game colour-colonies) bot-output bot-id)
  (when bot-output
    (let (*read-eval*
          (player (gethash bot-id (player-ids game))))
      (match (car bot-output)
        ((ppcre "build")
         (build player game))
        ((ppcre "move (\\d+) (\\d+)" (read row) (read col))
         (move player row col game))
        (t (null-move player game))))))

(defmethod update-game-turn ((game colour-colonies))
  (decf (turns-remaining game))
  (format t "turn ~a moves ~a~%" (current-turn game) (to-json (car (move-history game))))
  (push nil (move-history game)))

(defmethod get-bot-input ((game colour-colonies))
  (format nil "~a" (to-json (alist-hash-table 
                             (list (cons 'previous-turn (or (cadr (move-history game)) (vector)))
                                   (cons 'player-positions (alist-hash-table 
                                                            (player-positions game))))))))

(defparameter *base-path* (directory-namestring #.*compile-file-truename*))

(defun run-bots ()
  (loop for i from 1 to 2
     for bot-base-path in (mapcar (lambda (dir) (merge-pathnames dir *base-path* ))
                                  '("bot1/" "bot2/"))
     for definition = (read-bot-definition (merge-pathnames "definition.json" bot-base-path))
     collect (start-bot-from-definition definition (format nil "~a" bot-base-path))))

(defun random-position (rows cols other)
  (iter (for row = (random rows))
        (for col = (random cols))
        (until (or (not other) (or (/= row (car other))
                                   (/= col (cdr other)))))
        (finally (return (cons row col)))))

(defun populate-players (bots rows cols)
  (iter (with player-ids = (make-hash-table :test 'equal))
        (with players = (make-hash-table :test 'equal))
        (for bot in bots)
        (for colour in '(blue red))
        (for other previous position)
        (for position in (list (cons (floor rows 2) 0) (cons (floor rows 2) (- cols 1))))
        (for player = (make-instance 'player :player-position position 
                                     :name (bot-name bot) 
                                     :id (bot-id bot)
                                     :colour colour))
        (setf (gethash position players) player)
        (setf (gethash (bot-id bot) player-ids) player)
        (finally (return (cons player-ids players)))))

(defclass colour-colonies-game (colour-colonies concrete-game) ())

(defun run-game ()
  (format t "running game~%")
  (bind ((bots (run-bots))
         (rows 20)
         (cols 20)
         ((player-ids . players) (populate-players bots rows cols))
         (game (make-instance 'colour-colonies-game :rows rows :cols cols
                              :bots bots :player-ids player-ids :players players)))
    (n-player-game game)))

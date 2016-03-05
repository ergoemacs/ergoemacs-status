;;; ergoemacs-status.el --- Adapatave Status Bar / Mode Line -*- lexical-binding: t -*-
;; 
;; Filename: ergoemacs-status.el
;; Description: Adaptive Status Bar / Mode Line for Emacs
;; Author: Matthew Fidler
;; Maintainer: Matthew Fidler
;; Created: Fri Mar  4 14:13:50 2016 (-0600)
;; Version: 0.1
;; Package-Requires: ((powerline "2.3") (mode-icons "0.1.0"))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'powerline)
(require 'mode-icons)

(defcustom ergoemacs-status-popup-languages nil
  "Allow Swapping of major-modes when clicking the mode-name."
  :type 'boolean
  :group 'ergoemacs-status)

(defcustom ergoemacs-status-change-buffer 'ergoemacs-status-group-function
  "Method of changing buffer."
  :type '(choice
	  (function :tag "Function to group buffers.")
	  (const :tag "Switch to next/previous user/emacs buffer." 'ergoemacs)
	  (const :tag "Use emacs default method." nil))
  :group 'ergoemacs-status)

(defvar ergoemacs-status--major-mode-menu-map nil)

(defun ergoemacs-status--major-mode-menu-map ()
  "Popup major modes and information about current mode."
  (interactive)
  (or ergoemacs-status--major-mode-menu-map
      (set (make-local-variable 'ergoemacs-status--major-mode-menu-map)
	   (let ((map (and ergoemacs-status-popup-languages
			   ;; Mode in menu
			   (memq major-mode ergoemacs-menu--get-major-modes)
			   (key-binding [menu-bar languages])))
		 mmap)
	     (if (not map)
		 (mouse-menu-major-mode-map)
	       (setq mmap (mouse-menu-major-mode-map))
	       (define-key map [major-mode-sep-b] '(menu-item  "---"))
	       (define-key map [major-mode] (cons (nth 1 mmap) mmap))
	       map)))))

(defun ergoemacs-status-buffer-list ()
  "List of buffers shown in popup menu."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defun ergoemacs-status-group-function (&optional buffer)
  "What group does the current buffer belong to?"
  (if (char-equal ?\* (aref (buffer-name buffer) 0))
      "Emacs Buffer"
    "User Buffer"))

(defun ergoemacs-status-menu (&optional buffer)
  (let* ((cb (or buffer (current-buffer)))
	 (group (with-current-buffer cb
		  (if (functionp ergoemacs-status-change-buffer)
		      (funcall ergoemacs-status-change-buffer)
		    '("Common"))))
	 (groups '())
	 (buf-list (sort
		    (mapcar
		     ;; for each buffer, create list: buffer, buffer name, groups-list
		     ;; sort on buffer name; store to bl (buffer list)
		     (lambda (b)
		       (let (tmp0 tmp1 tmp2)
			 (with-current-buffer b
			   (setq tmp0 b
				 tmp1 (buffer-name b)
				 tmp2 (if (functionp ergoemacs-status-change-buffer)
					  (funcall ergoemacs-status-change-buffer)
					"Common"))
			   (unless (or (string= group tmp2) (assoc tmp2 groups))
			     (push (cons tmp2 (intern tmp2)) groups))
			   (list tmp0 tmp1 tmp2 (intern tmp1)))))
		     (ergoemacs-status-buffer-list))
		    (lambda (e1 e2)
		      (or (and (string= (nth 2 e2) (nth 2 e2))
			       (not (string-lessp (nth 1 e1) (nth 1 e2))))
			  (not (string-lessp (nth 2 e1) (nth 2 e2)))))))
	 menu menu2 tmp)
    (dolist (item buf-list)
      (if (string= (nth 2 item) group)
	  (unless (eq cb (nth 0 item))
	    (push `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item)))) menu))
	(if (setq tmp (assoc (nth 2 item) menu2))
	    (push `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item))))
		  (cdr tmp))
	  (push (list (nth 2 item) `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item))))) menu2))))
    (setq menu `(keymap ,(if (or menu (> (length menu2) 1))
			     group
			   (car (car menu2)))
			,@(if (or menu (> (length menu2) 1))
			      (mapcar
			       (lambda(elt)
				 `(,(intern (car elt)) menu-item ,(car elt) (keymap ,@(cdr elt))))
			       menu2)
			    (cdr (car menu2)))
			,(when (and menu (>= (length menu2) 1))
			   '(sepb menu-item "--"))
			,@menu))
    menu))

(defun ergoemacs-status-next-buffer (event)
  "Next ergoemacs buffer"
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let ((emacs-buffer-p (string-match-p "^[*]" (buffer-name))))
      (cond
       ((functionp ergoemacs-status-change-buffer)
	(popup-menu (ergoemacs-status-menu)))
       ((not ergoemacs-status-change-buffer)
	(next-buffer))
       (emacs-buffer-p
	(ergoemacs-next-emacs-buffer))
       (t
	(ergoemacs-next-user-buffer))))))

(defun ergoemacs-status-previous-buffer (event)
  "Prevous ergoemacs buffer"
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let ((emacs-buffer-p (string-match-p "^[*]" (buffer-name))))
      (cond
       ((functionp ergoemacs-status-change-buffer)
	(popup-menu (ergoemacs-status-menu)))
       ((not ergoemacs-status-change-buffer)
	(previous-buffer))
       (emacs-buffer-p
	(ergoemacs-previous-emacs-buffer))
       (t
	(ergoemacs-previous-user-buffer))))))

(defcustom ergoemacs-status-read-only-status t
  "Include minor-modes in `mode-line-format'."
  :type 'boolean
  :group 'ergoemacs-status)

(defcustom ergoemacs-status-minor-modes t
  "Include minor-modes in `mode-line-format'."
  :type 'boolean
  :group 'ergoemacs-status)

(defcustom ergoemacs-status-coding t
  "Include coding system information in `mode-line-format'"
  :type 'boolean
  :group 'ergoemacs-status)

(defcustom ergoemacs-status-use-vc t
  "Include vc in mode-line."
  :type 'boolean
  :group 'ergoemacs-status)

(defun ergoemacs-status--property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  ;; Taken from powerline by Donald Ephraim Curtis, Jason Milkins and
  ;; Nicolas Rougier
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun ergoemacs-status--ensure-list (item)
  (or (and (listp item) item) (list item)))

(defun ergoemacs-status--add-text-property (str prop val)
  ;; Taken from powerline by Donald Ephraim Curtis, Jason Milkins and
  ;; Nicolas Rougier
  ;; Changed so that prop is not just 'face
  ;; Also changed to not force list, or add a nil to the list
  (mapconcat
   (lambda (mm)
     (let ((cur (get-text-property 0 prop mm)))
       (if (not cur)
	   (propertize mm prop val)
	 (propertize mm prop (append (ergoemacs-status--ensure-list cur) (list val))))))
   (ergoemacs-status--property-substrings str prop)
   ""))

(defun ergoemacs-status--if--1 (lst-or-string)
  (cond
   ((consp lst-or-string)
    (catch 'found-it
      (dolist (elt lst-or-string)
	(cond
	 ((functionp elt)
	  (let ((tmp (funcall elt)))
	    (if (or (and tmp (stringp tmp)) (listp tmp))
		(throw 'found-it tmp)
	      ;; Otherwise, assume its a boolean.
	      ;; If didn't
	      (when (and (booleanp tmp)
			 (not tmp))
		(throw 'found-it "")))))
	 
	 ((and elt (stringp elt))
	  (throw 'found-it elt))
	 ((and elt (consp elt))
	  (throw 'found-it elt))
	 ((and (symbolp elt) (boundp elt) (or (consp (symbol-value elt)) (stringp (symbol-value elt))))
	  (throw 'found-it (symbol-value elt)))))
      ""))
   ((and lst-or-string (stringp lst-or-string))
    lst-or-string)
   (t "")))

(defun ergoemacs-status--if (str &optional face pad)
  "Render STR as mode-line data using FACE and optionally PAD import on left (l), right (r) or both (b)."
  (let* ((rendered-str (format-mode-line (ergoemacs-status--if--1 str)))
	 (padded-str (concat
		      (when (and (> (length rendered-str) 0) (memq pad '(l left b both))) " ")
		      rendered-str
		      (when (and (> (length rendered-str) 0) (memq pad '(r right b both))) " "))))
    (if face
	(ergoemacs-status--add-text-property padded-str 'face face)
      padded-str)))

(defvar ergoemacs-old-mode-line-format nil)

(defun ergoemacs-status--set-buffer-file-coding-system (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively #'set-buffer-file-coding-system)))

(defun ergoemacs-status--encoding ()
  "Encoding mode-line."
  (ergoemacs-save-buffer-state
   (propertize (format "%s" (coding-system-type buffer-file-coding-system))
	      'mouse-face 'mode-line-highlight
	      'help-echo (format "mouse-1: Change buffer coding system\n%s"
				 (coding-system-doc-string buffer-file-coding-system))
	      'local-map '(keymap
			   (mode-line keymap
				      (mouse-1 . ergoemacs-status--set-buffer-file-coding-system))))))

(defun ergoemacs-status--sep (dir &rest args)
  "Separator"
  (let ((separator (and (fboundp #'powerline-current-separator)
			(intern (format "powerline-%s-%s"
					(powerline-current-separator)
					(or (and (eq dir 'left)
						 (car powerline-default-separator-dir))
					    (cdr powerline-default-separator-dir)))))))
    (when (fboundp separator)
      (let ((img (apply separator args)))
	(when (and (listp img) (eq 'image (car img)))
	  (ergoemacs-save-buffer-state
	   (propertize " " 'display img
		       'face (plist-get (cdr img) :face))))))))

(defvar ergoemacs-status--lhs nil)
(defvar ergoemacs-status--center nil)
(defvar ergoemacs-status--rhs nil)

(defun ergoemacs-minor-mode-menu-from-indicator (indicator &optional dont-popup)
  "Show menu for minor mode specified by INDICATOR.
Interactively, INDICATOR is read using completion.
If there is no menu defined for the minor mode, then create one with
items `Turn Off', `Hide' and `Help'."
  (interactive
   (list (completing-read
	  "Minor mode indicator: "
	  (describe-minor-mode-completion-table-for-indicator))))
  (let* ((minor-mode (or (and (stringp indicator) (lookup-minor-mode-from-indicator indicator))
			 (and (symbolp indicator) indicator)))
         (mm-fun (or (get minor-mode :minor-mode-function) minor-mode)))
    (unless minor-mode (error "Cannot find minor mode for `%s'" indicator))
    (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
           (menu (and (keymapp map) (lookup-key map [menu-bar])))
	   (hidden (memq minor-mode ergoemacs-status--space-hidden-minor-modes)))
      (setq menu
            (if menu
                (if hidden
		    (mouse-menu-non-singleton menu)
		  `(,@(mouse-menu-non-singleton menu)
		    (sep-minor-mode-ind menu-item "--")
		    (hide menu-item ,(if dont-popup
					 "Show this minor-mode"
				       "Hide this minor-mode")
			  (lambda () (interactive)
			    (ergoemacs-minor-mode-hide ',mm-fun ,dont-popup)))))
	      `(keymap
                ,indicator
                (turn-off menu-item "Turn Off minor mode" ,mm-fun)
		,(if hidden nil
		   `(hide menu-item ,(if dont-popup
				     "Show this minor-mode"
				   "Hide this minor-mode")
		      (lambda () (interactive)
			(ergoemacs-minor-mode-hide ',mm-fun ,dont-popup))))
                (help menu-item "Help for minor mode"
                      (lambda () (interactive)
                        (describe-function ',mm-fun))))))
      (if dont-popup menu
	(popup-menu menu)))))

(defun ergoemacs-status--minor-mode-mouse (click-group click-type string)
  "Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING."
  ;; Taken from Powerline
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (ergoemacs-minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defvar ergoemacs-status--space-hidden-minor-modes nil
  "List of minor modes hidden due to space limitations.")

;; Adapted from powerline.
(defvar ergoemacs-status--hidden-minor-modes '(isearch-mode ergoemacs-status-mode)
  "List of hidden modes and their indicators.")

(defun ergoemacs-minor-mode-hide (minor-mode &optional show)
  "Hide a minor-mode based on mode indicator MINOR-MODE."
  (if show
      (setq ergoemacs-status--hidden-minor-modes (delq minor-mode ergoemacs-status--hidden-minor-modes))
    (unless (memq minor-mode ergoemacs-status--hidden-minor-modes)
      (push minor-mode ergoemacs-status--hidden-minor-modes)))
  (force-mode-line-update))

(defun ergoemacs-minor-mode-hidden-menu (&optional _event)
  "Display a list of the hidden minor modes."
  (interactive "@e")
  (popup-menu
   `(keymap
     ,@(mapcar
       (lambda(m)
	 `(,m menu-item ,(format "%s" m) ,(ergoemacs-minor-mode-menu-from-indicator m t)))
       (let (ret)
	 (dolist (elt (append ergoemacs-status--hidden-minor-modes
			      ergoemacs-status--space-hidden-minor-modes))
	   (when (and (boundp elt) (symbol-value elt))
	     (push elt ret)))
	 ret)))))

(defun ergoemacs-minor-mode-alist ()
  "Get a list of the minor-modes"
  (let (ret)
    (dolist (a (reverse minor-mode-alist))
      (unless (memq (car a) ergoemacs-status--hidden-minor-modes)
	(push a ret)))
    ret))

(defvar ergoemacs-status--minor-modes-p t
  "Determine if `ergoemacs-status--minor-modes' generates space")

(defvar ergoemacs-status--minor-modes-available nil)
(defun ergoemacs-status--minor-modes-available (mode-line face1 face2 &optional reduce)
  (let (lhs rhs center)
    (setq ergoemacs-status--minor-modes-p nil)
    (unwind-protect
	(setq lhs (ergoemacs-status--eval-lhs mode-line face1 face2 reduce)
	      rhs (ergoemacs-status--eval-rhs mode-line face1 face2 reduce)
	      center (ergoemacs-status--eval-center mode-line face1 face2))
      (setq ergoemacs-status--minor-modes-p t
	    ergoemacs-status--minor-modes-available (- (ergoemacs-status--eval-width)
						     (+ (ergoemacs-status--eval-width lhs)
							(ergoemacs-status--eval-width rhs)
							(ergoemacs-status--eval-width center)))))))
(defun ergoemacs-status--minor-modes ()
  "Get minor modes"
  (let* ((width 0)
	 (ret ""))
    (when ergoemacs-status--minor-modes-p
      (setq ergoemacs-status--space-hidden-minor-modes nil
	    ret (replace-regexp-in-string
		 " +$" ""
		 (concat
		  (mapconcat (lambda (mm)
			       (if (or (not (numberp ergoemacs-status--minor-modes-available))
				       (< width ergoemacs-status--minor-modes-available))
				   (let ((cur (propertize mm
							  'mouse-face 'mode-line-highlight
							  'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
							  'local-map (let ((map (make-sparse-keymap)))
								       (define-key map
									 [mode-line down-mouse-1]
									 (ergoemacs-status--minor-mode-mouse 'minor 'menu mm))
								       (define-key map
									 [mode-line mouse-2]
									 (ergoemacs-status--minor-mode-mouse 'minor 'help mm))
								       (define-key map
									 [mode-line down-mouse-3]
									 (ergoemacs-status--minor-mode-mouse 'minor 'menu mm))
								       (define-key map
									 [header-line down-mouse-3]
									 (ergoemacs-status--minor-mode-mouse 'minor 'menu mm))
								       map))))
				     (setq width (+ width (ergoemacs-status--eval-width cur) 1))
				     (if (or (not (numberp ergoemacs-status--minor-modes-available))
					     (< width ergoemacs-status--minor-modes-available))
					 cur
				       (push (lookup-minor-mode-from-indicator mm) ergoemacs-status--space-hidden-minor-modes)
				       ""))
				 (push (lookup-minor-mode-from-indicator mm) ergoemacs-status--space-hidden-minor-modes)
				 ""))
			     (split-string (format-mode-line (ergoemacs-minor-mode-alist)))
			     " ")))))
    (when (or (not ergoemacs-status--minor-modes-p)
	      ergoemacs-status--space-hidden-minor-modes
	      (catch 'found
		(dolist (elt ergoemacs-status--hidden-minor-modes)
		  (when (and (boundp elt) (symbol-value elt))
		    (throw 'found t)))
		nil))
      (setq ret (concat ret " "
			(propertize (if (and (fboundp #'mode-icons-propertize-mode))
					(mode-icons-propertize-mode "+" (list "+" #xf151 'FontAwesome))
				      "+")
				    'mouse-face 'mode-line-highlight
				    'help-echo "Hidden Minor Modes\nmouse-1: Display hidden minor modes"
				    'local-map (let ((map (make-sparse-keymap)))
						 (define-key map [mode-line down-mouse-1] 'ergoemacs-minor-mode-hidden-menu)
						 (define-key map [mode-line down-mouse-3] 'ergoemacs-minor-mode-hidden-menu)
						 map)))))
    ret))

(defvar ergoemacs-status-position-map
  (let ((map (copy-keymap mode-line-column-line-number-mode-map)))
    (define-key map [mode-line down-mouse-3]
      (lookup-key map [mode-line down-mouse-1]))
    (define-key (lookup-key map [mode-line down-mouse-3])
      [size-indication-mode]
      '(menu-item "Display Buffer Size"
		  size-indication-mode
		  :help "Toggle displaying file size in the mode-line"
		  :button (:toggle . size-indication-mode)))
    (define-key map [mode-line down-mouse-1] 'ignore)
    (define-key map [mode-line mouse-1] 'ergoemacs-status-goto-line)
    map)
  "Position map (includes size-indication-mode)")

(defun ergoemacs-status-goto-line (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively #'goto-line)))

(defun ergoemacs-status-position ()
  "ergoemacs-status mode line position."
  (let ((col (propertize
	      (or (and line-number-mode "%3c") "")
	      'mouse-face 'mode-line-highlight
	      'local-map ergoemacs-status-position-map)))
    (when  (>= (current-column) 80)
      (setq col (propertize col 'face 'error)))
    (concat (propertize
	     (or (and column-number-mode "%4l") "")
	     'face 'bold
	     'mouse-face 'mode-line-highlight
	     'local-map ergoemacs-status-position-map)
	    (or (and column-number-mode line-number-mode ":") "")
	    col)))

(defun ergoemacs-status-size-indication-mode ()
  (when size-indication-mode
    (propertize
	      "%I"
	      'mouse-face 'mode-line-highlight
	      'local-map ergoemacs-status-position-map)))

(defun ergoemacs-status--atom ()
  (setq ergoemacs-status--lhs
	'(((ergoemacs-status-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p t)
	  ((ergoemacs-status-size-indication-mode) :reduce 2 :pad b)
	  ((ergoemacs-status-position) :reduce 2 :pad b)
	  ((ergoemacs-status-use-vc powerline-vc) :reduce 1 :pad r))
	ergoemacs-status--center
	'(((ergoemacs-status--minor-modes)  :reduce 4)
	  ((mode-icons--generate-narrow powerline-narrow "%n") :reduce 4)
	  (" " :reduce 4))
	ergoemacs-status--rhs
	'((global-mode-string :pad r)
	  ((ergoemacs-status-coding (lambda() (not (string= "undecided" (ergoemacs-status--encoding)))) ergoemacs-status--encoding) :pad b :reduce 2)
	  ((ergoemacs-status-coding (lambda() (not (string= ":" (mode-line-eol-desc)))) mode-icons--mode-line-eol-desc mode-line-eol-desc) :pad l :reduce 2)
	  ((mode-icons--generate-major-mode-item powerline-major-mode) :pad l)))
  (force-mode-line-update))

(defun ergoemacs-status--center ()
  (setq ergoemacs-status--lhs
	'(((mode-icons--generate-major-mode-item powerline-major-mode) :pad b)
	  ((ergoemacs-status-use-vc powerline-vc) :reduce 1 :pad r)
	  ((ergoemacs-status-size-indication-mode) :reduce 2 :pad b)
	  ((ergoemacs-status-position) :reduce 2 :pad b))
	ergoemacs-status--center
	'(((ergoemacs-status-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p r)
	  )
	ergoemacs-status--rhs
	'((global-mode-string :pad r)
	  ((ergoemacs-status-coding (lambda() (not (string= "undecided" (ergoemacs-status--encoding)))) ergoemacs-status--encoding) :pad b :reduce 2)
	  ((ergoemacs-status-coding (lambda() (not (string= ":" (mode-line-eol-desc)))) mode-icons--mode-line-eol-desc mode-line-eol-desc) :pad l :reduce 2)
	  ((ergoemacs-status--minor-modes)  :reduce 4)
	  ((mode-icons--generate-narrow powerline-narrow "%n") :reduce 4 :last-p t)))
  (force-mode-line-update))

(defun ergoemacs-status--xah ()

  (setq ergoemacs-status--lhs
	'(((ergoemacs-status-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p t)
	  ((ergoemacs-status-size-indication-mode) :reduce 2 :pad b)
	  ((ergoemacs-status-position) :reduce 2 :pad b)
	  ;; ((ergoemacs-status-use-vc powerline-vc) :reduce 1 :pad r)
	  ((mode-icons--generate-major-mode-item powerline-major-mode) :pad b))
	ergoemacs-status--center nil
	ergoemacs-status--rhs nil)
  (force-mode-line-update))

(ergoemacs-status--center)

(defun ergoemacs-status--eval-center (mode-line face1 _face2 &optional reduce)
  (ergoemacs-status--stack ergoemacs-status--center (list mode-line face1) 'center reduce))

(defun ergoemacs-status--eval-lhs (mode-line face1 _face2 &optional reduce)
  (ergoemacs-status--stack ergoemacs-status--lhs (list mode-line face1) 'left reduce))

(defun ergoemacs-status--eval-rhs (mode-line face1 _face2 &optional reduce)
  (ergoemacs-status--stack ergoemacs-status--rhs (list mode-line face1) 'right reduce))

(defun ergoemacs-status--stack (mode-line-list face-list dir &optional reduction-level)
  "Stacks mode-line elements"
  (let ((face-i (if (eq dir 'center) 0 -1))
	(len (length face-list))
	(last-face (nth 0 face-list))
	cur-face final tmp
	(mode-line-list mode-line-list)
	last-reduced-p)
    (when (eq dir 'right)
      (setq mode-line-list (reverse mode-line-list)))
    (setq final (apply 'append
		       (mapcar
			(lambda(elt)
			   (unless (eq dir 'center)
			    (setq face-i (1+ face-i)))
			  (let* ((ifs (car elt))
				 (plist (cdr elt))
				 (reduce (plist-get plist :reduce))
				 tmp
				 ret)
			    (if (and reduce (integerp reduce)
				     reduction-level (integerp reduction-level)
				     (<= reduce reduction-level))
				(progn
				  (unless (eq dir 'center)
				    (setq face-i (- face-i 1)))
				  (setq last-reduced-p t)
				  nil)
			      (when (and (not (eq dir 'center)) (plist-get plist :last-p))
				(unless last-reduced-p
				  (setq face-i (- face-i 1)
					last-reduced-p nil)))
			      (setq cur-face (nth (mod face-i len) face-list))
			      (setq tmp (ergoemacs-status--if ifs cur-face (plist-get plist :pad)))
			      (if (and tmp (stringp tmp) (string= (format-mode-line tmp) ""))
				  (if (or (eq dir 'center) (plist-get plist :last-p) last-reduced-p) nil
				    (setq face-i (- face-i 1))
				    nil)
				(setq last-reduced-p nil) 
				(push tmp ret)
				(unless (eq cur-face last-face)
				  (cond
				   ((eq dir 'left)
				    (push (ergoemacs-status--sep dir last-face cur-face) ret))
				   ((eq dir 'right)
				    (push (ergoemacs-status--sep dir cur-face last-face) ret))))
				(setq last-face cur-face))
			      ret)))
			mode-line-list))
	  cur-face (car (last face-list)))
    (unless (eq last-face cur-face)
      (setq final (append final
			  (list (cond
				 ((eq dir 'left)
				  (ergoemacs-status--sep dir last-face cur-face))
				 ((eq dir 'right)
				  (ergoemacs-status--sep dir cur-face last-face)))))))
    (dolist (elt (reverse final))
      (when elt
	(push elt tmp)))
    (setq final tmp)    
    (when (and final (eq dir 'center))
      (setq final (append (list (ergoemacs-status--sep 'left (car (last face-list)) (nth 0 face-list)))
			  final
			  (list (ergoemacs-status--sep 'right (nth 0 face-list) (car (last face-list)))))))
    (when (eq dir 'right)
      (setq final (reverse final)))
    (or final "")))

(defcustom ergoemacs-status-extra-width 0
  "Extra width to add."
  :type 'integer
  :group 'ergoemacs-status)

(defcustom ergoemacs-status-width-multiplier 1.0
  "Multiplier for width."
  :type 'number
  :group 'ergoemacs-status)

(defvar ergoemacs-status--pixel-width-p nil
  "Determines if the mode line tries to calculate width")

(defun ergoemacs-status--eval-width (&optional what)
  (if ergoemacs-status--pixel-width-p
      (ergoemacs-status--eval-width-pixels what)
    (ergoemacs-status--eval-width-col what)))

(defun ergoemacs-status--eval-string-width-pixels (str)
  "Get string width in pixels."
  (with-current-buffer (get-buffer-create " *ergoemacs-eval-width*")
	(delete-region (point-min) (point-max))
	(insert str)
	(car (window-text-pixel-size nil (point-min) (point-max)))))

(defun ergoemacs-status--eval-width-pixels (&optional what)
  "Get the width of the display in pixels."
  (ergoemacs-status--eval-width-col what t))

(defun ergoemacs-status--eval-width-col-string (str &optional pixels-p)
  "Figure out the column width of STR."
  (apply
   '+
   (mapcar (lambda(x)
	     (let ((display (get-text-property 0 'display x)))
	       (if display
		   (car (image-size display pixels-p))
		 (if pixels-p
		     (ergoemacs-status--eval-string-width-pixels x)
		   (string-width x)))))
	   (ergoemacs-status--property-substrings str 'display))))

(defun ergoemacs-status--eval-width-col (&optional what pixels-p)
  "Eval width of WHAT, which is formated with `format-mode-line'.
When WHAT is nil, return the width of the window"
  (or (and what (ergoemacs-status--eval-width-col-string (format-mode-line what pixels-p)))
      (if pixels-p
	  (let ((width (ergoemacs-status--eval-width-col))
		(cw (frame-char-width)))
	    (* cw width))
	(let ((width (window-width))
	      ;; (cw (frame-char-width))
	      
	      tmp)
	  (when (setq tmp (window-margins))
	    (setq width (apply '+ width (list (or (car tmp) 0) (or (cdr tmp) 0)))))
	  (setq width (* ergoemacs-status-width-multiplier (+ width ergoemacs-status-extra-width)))
	  (setq ergoemacs-stats--ignore-eval-p t)
	  (unwind-protect
	      (setq width (- width (ergoemacs-status--eval-width-col-string (format-mode-line mode-line-format) pixels-p)))
	    (setq ergoemacs-stats--ignore-eval-p nil))))))

(defvar ergoemacs-status-max-reduction 4)

(defvar ergoemacs-stats--ignore-eval-p nil
  "Determine if the evaluate will complete.")
(defun ergoemacs-status--eval ()
  (if ergoemacs-stats--ignore-eval-p ""
    ;; This will dynamically grow/fill areas
    (setq mode-icons-read-only-space nil
	  mode-icons-show-mode-name t
	  mode-icons-eol-text t
	  mode-name (or (and (fboundp #'mode-icons-get-mode-icon)
			     (mode-icons-get-mode-icon (or mode-icons-cached-mode-name mode-name)))
			mode-name))
    (let* ((active (or (and (fboundp #'powerline-selected-window-active) (powerline-selected-window-active)) t))
	   (mode-line (if active 'mode-line 'mode-line-inactive))
	   (face1 (if active 'powerline-active1 'powerline-inactive1))
	   (face2 (if active 'powerline-active2 'powerline-inactive2))
	   (mode-icons-read-only-space nil)
	   (mode-icons-show-mode-name t)
	   lhs rhs center
	   wlhs wrhs wcenter
	   available
	   (reduce-level 1))
      (setq ergoemacs-status--minor-modes-available nil
	    lhs (ergoemacs-status--eval-lhs mode-line face1 face2)
	    rhs (ergoemacs-status--eval-rhs mode-line face1 face2)
	    center (ergoemacs-status--eval-center mode-line face1 face2)
	    wlhs (ergoemacs-status--eval-width lhs)
	    wrhs (ergoemacs-status--eval-width rhs)
	    wcenter (ergoemacs-status--eval-width center))
      (when (> (+ wlhs wrhs wcenter) (ergoemacs-status--eval-width))
	(setq mode-icons-read-only-space nil
	      mode-icons-show-mode-name nil
	      mode-icons-eol-text nil
	      mode-name (or (and (fboundp #'mode-icons-get-mode-icon)
				 (mode-icons-get-mode-icon (or mode-icons-cached-mode-name mode-name)))
			    mode-name)
	      lhs (ergoemacs-status--eval-lhs mode-line face1 face2)
	      rhs (ergoemacs-status--eval-rhs mode-line face1 face2)
	      center (ergoemacs-status--eval-center mode-line face1 face2)
	      wlhs (ergoemacs-status--eval-width lhs)
	      wrhs (ergoemacs-status--eval-width rhs)
	      wcenter (ergoemacs-status--eval-width center))
	(while (and (<= reduce-level ergoemacs-status-max-reduction)
		    (> (+ wlhs wrhs wcenter) (ergoemacs-status--eval-width)))
	  (setq mode-icons-read-only-space nil
		mode-icons-show-mode-name nil
		mode-icons-eol-text nil
		lhs (ergoemacs-status--minor-modes-available mode-line face1 face2)
		lhs (ergoemacs-status--eval-lhs mode-line face1 face2 reduce-level)
		rhs (ergoemacs-status--eval-rhs mode-line face1 face2 reduce-level)
		center (ergoemacs-status--eval-center mode-line face1 face2 reduce-level)
		wlhs (ergoemacs-status--eval-width lhs)
		wrhs (ergoemacs-status--eval-width rhs)
		wcenter (ergoemacs-status--eval-width center)
		reduce-level (+ reduce-level 1))))
      (setq available (/ (- (ergoemacs-status--eval-width) (+ wlhs wrhs wcenter)) 2))
      (if ergoemacs-status--pixel-width-p
	  (setq available (list available)))
      ;; (message "a: %s (%3.1f %3.1f %3.1f; %3.1f)" available wlhs wrhs wcenter (ergoemacs-status--eval-width))
      (list lhs
	    (propertize " " 'display `((space :width ,available))
			'face face1)
	    center
	    (propertize " " 'display `((space :width ,available))
			'face face1)
	    rhs))))

(defun ergoemacs-status--variable-pitch (&optional frame)
  (dolist (face '(mode-line mode-line-inactive
			    powerline-active1
			    powerline-inactive1
			    powerline-active2 powerline-inactive2))
    (set-face-attribute face frame
			  :family (face-attribute 'variable-pitch :family)
			  :foundry (face-attribute 'variable-pitch :foundry)
			  :height 125)))

(defun ergoemacs-status-format (&optional restore)
  "Setup `ergoemacs-status' `mode-line-format'."
  (if restore
      (progn
	(set-default 'mode-line-format
		     ergoemacs-old-mode-line-format)
	;; FIXME -- restore old in all buffers.
	)
    (unless ergoemacs-old-mode-line-format
      (setq ergoemacs-old-mode-line-format mode-line-format))

    (setq-default mode-line-format
		  `("%e"
		    (:eval (ergoemacs-status--eval))))
    ;; FIXME -- Apply to all buffers.
    (setq mode-line-format `("%e"
			     (:eval (ergoemacs-status--eval))))
    (force-mode-line-update)))

(defvar ergoemacs-status-turn-off-mode-icons nil)
(define-minor-mode ergoemacs-status-mode
  "Ergoemacs status mode."
  :global t
  (if ergoemacs-status-mode
      (progn
	(if mode-icons-mode
	    (setq ergoemacs-status-turn-off-mode-icons nil)
	  (setq ergoemacs-status-turn-off-mode-icons t)
	  (mode-icons-mode 1))
	(ergoemacs-status-format))
    (when ergoemacs-status-turn-off-mode-icons
      (mode-icons-mode -1))
    (ergoemacs-status-format t)))

;; (ergoemacs-advice mouse-menu-major-mode-map ()
;;   "Modify to list major modes for `ergoemacs-status'."
;;   :type :replace
;;   (ergoemacs-status--major-mode-menu-map))

;; (ergoemacs-advice mode-line-next-buffer (event)
;;   "Modify mode-line-next-buffer for `ergoemacs-status'"
;;   :type :around
;;   (if ergoemacs-status
;;       (ergoemacs-status-next-buffer event)
;;     ad-do-it))

;; (ergoemacs-advice mode-line-previous-buffer (event)
;;   "Modify mode-line-next-buffer for `ergoemacs-status'"
;;   :type :around
;;   (if ergoemacs-status
;;       (ergoemacs-status-previous-buffer event)
;;     ad-do-it))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-status.el ends here

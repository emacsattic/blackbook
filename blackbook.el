;;; blackbook.el -- edit aliases file in an user-friendly way

;; Copyright (c) 1997 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@srce.hr>
;; Keywords: mail, abbrev, extenstions
;; Version: 0.4

;; This file is not yet part of any Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: not in FSF

;;; Commentary:

;; Although the format of the mailrc file (that Emacs packages use to
;; read mail aliases from) is very simple, the question "How do I add
;; an alias to Emacs?" has popped up one time too many on comp.emacs.
;;
;; Blackbook package will parse the `~/.mailrc' file, and present the
;; aliases in a user-friendly way, using the facilities provided by
;; the widget library written by Per Abrahamsen.  In a way, this is
;; similar to Pine's and Netscape's `addressbook' features, hence the
;; name.


;;; Code:

(require 'cl)

(require 'widget)
(require 'wid-edit)

(defvar blackbook-mailrc-file
  (or (and (boundp 'mail-abbrev-mailrc-file)
	   mail-abbrev-mailrc-file)
      (getenv "MAILRC")
      (expand-file-name "~/.mailrc")))

(defvar blackbook-aliases-widget nil)
(defvar blackbook-file-widget nil)
(defvar blackbook-file-type nil)


;;; Functions used by widgets

(defun blackbook-import-choice-callback (widget &rest ignore)
  (let ((val (widget-value widget)))
    (widget-value-set
     blackbook-file-widget
     (expand-file-name
      (cdr (assq val '((mail . "~/.mailrc")
		       (mutt . "~/.muttrc")
		       (elm . "~/.elm/aliases.text")
		       (pine . "~/.addressbook"))))))
    (setq blackbook-file-type val))
  (widget-setup))

(defun blackbook-merge-callback (&rest ignore)
  (interactive)
  (if (null (widget-get blackbook-aliases-widget 'dirty))
      (message "(No aliases need to be saved)")
    (let ((invalid (widget-apply blackbook-aliases-widget
				 :validate)))
      (when invalid
	(error (widget-get invalid :error))))
    (blackbook-save-mailrc blackbook-mailrc-file
			      (widget-value blackbook-aliases-widget)
			      (blackbook-read blackbook-mailrc-file))
    (widget-put blackbook-aliases-widget 'dirty nil)))

(defun blackbook-save-callback (&rest ignore)
  (interactive)
  (if (null (widget-get blackbook-aliases-widget 'dirty))
      (message "(No aliases need to be saved)")
    (let ((invalid (widget-apply blackbook-aliases-widget
				 :validate)))
      (when invalid
	(error (widget-get invalid :error))))
    (blackbook-save-mailrc blackbook-mailrc-file
			      (widget-value blackbook-aliases-widget))
    (widget-put blackbook-aliases-widget 'dirty nil)))

(defun blackbook-import-callback (&rest ignore)
  (when (or (not (widget-get blackbook-aliases-widget 'dirty))
	    (yes-or-no-p
	     "Are you sure you want to discard changes? "))
    (let ((file (widget-value blackbook-file-widget))
	  aliases)
      (message "Parsing %s..." file)
      (setq aliases (blackbook-read file blackbook-file-type))
      (message "Refreshing widget...")
      (widget-value-set blackbook-aliases-widget aliases))
    (message "Refreshing buffer...")
    (widget-setup)
    (message "")
    (widget-put blackbook-aliases-widget 'dirty t)))

(defun blackbook-clear-callback (&rest ignore)
  (when (or (not (widget-get blackbook-aliases-widget 'dirty))
	    (yes-or-no-p
	     "Are you sure you want to clear the aliases? "))
    (widget-value-set blackbook-aliases-widget nil)
    (widget-setup)
    (widget-put blackbook-aliases-widget 'dirty t)))

(defun blackbook-activate-callback (&rest ignore)
  (if (not (widget-get blackbook-aliases-widget 'dirty))
      ;; argh!
      (if (fboundp 'build-mail-aliases)
	  (build-mail-aliases)
	(build-mail-abbrevs))
    (error "You must save options first")))
 
(defun blackbook-done-callback (&rest ignore)
  (interactive)
  (if (or (null (widget-value blackbook-aliases-widget))
	  (not (widget-get blackbook-aliases-widget 'dirty))
	  (yes-or-no-p
	   "There are unsaved changes.  Are you sure you want to exit? "))
      (kill-buffer (current-buffer))))

;; group must contain at least one member
(defun blackbook-validate-group (widget)
  (or (widget-children-validate widget)
      (if (null (cdr (widget-value widget)))
	  (prog1 widget
	    (widget-put widget
			:error "Empty groups not allowed"))
	nil)))

;; allow nothing but word and symbols constituents in alias/group names
(defun blackbook-validate-alias (widget)
  (let ((val (widget-value widget)))
    (if (or (zerop (length val))
	    (string-match "[^0-9a-zA-Z._-]" val))
      (prog1 widget
	(widget-put widget
		    :error (format "%s: invalid alias/group name" val)))
    nil)))

;; on the other hand, addresses can contain anything but newlines
(defun blackbook-validate-address (widget)
  (let ((val (widget-value widget)))
    (if (or (zerop (length val))
	    (string-match "\n" (widget-value widget)))
	(prog1 widget
	  (widget-put widget :error "Invalid email address"))
      nil)))


;;; Buffer setup

(defun blackbook-setup-buffer ()
  (message "Creating blackbook buffer...")
  (kill-buffer (get-buffer-create "*Blackbook*"))
  (switch-to-buffer (get-buffer-create "*Blackbook*"))
  ;; Short documentation.
  (widget-insert "This is the Blackbook buffer.
Push RET or click mouse-2 on the word ")
  (widget-create 'info-link 
		 :tag "help"
		 :help-echo "Read the online help."
		 "(blackbook)Editing Mail Aliases")
  (widget-insert " for more information.\n\n")
  ;; File choices.
  (setq blackbook-file-type nil)
  (widget-create 'choice
		 :tag "Aliases Type"
		 :help-echo "Choose file format"
		 :format "%[%v%]"
		 :value 'mail
		 :notify 'blackbook-import-choice-callback
		 '(const :tag "Emacs" :format "%t" mail)
		 '(const :tag "Mutt" :format "%t" mutt)
		 '(const :tag "Elm" :format "%t" elm)
		 '(const :tag "Pine" :format "%t" pine))
  (widget-insert " aliases ")
  (setq
   blackbook-file-widget
   (widget-create 'file
		  :tag "file"
		  :format "%[%t%]: %v"
		  :help-echo "File to read aliases from"
		  :value blackbook-mailrc-file))
  (widget-insert "\n")
  (widget-create 'file
		 :tag "Save To"
		 :format "%[%t%]: %v"
		 :help-echo "File to save aliases to"
		 :notify (lambda (widget &rest ignore)
			   (setq blackbook-mailrc-file (widget-value widget)))
		 :value blackbook-mailrc-file)
  (widget-insert "\n")
  ;; Buttons.
  (widget-create 'push-button
		 :tag "Import"
		 :help-echo "Import settings from file"
		 :action 'blackbook-import-callback)
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Merge & Save"
		 :help-echo "Merge with aliases file and save"
		 :action 'blackbook-merge-callback)
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Save"
		 :help-echo "Save aliases, replacing previous ones"
		 :action 'blackbook-save-callback)
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Clear"
		 :help-echo "Clear alias list"
		 :action 'blackbook-clear-callback)
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Activate"
		 :help-echo "Use saved aliases in mail"
		 :action 'blackbook-activate-callback)
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Done"
		 :help-echo "Exit Blackbook"
		 :action 'blackbook-done-callback)
  (widget-insert "\n\n")
  ;; Main widget with all the aliases.
  (setq
   blackbook-aliases-widget
   (widget-create
    'editable-list
    :offset 3
    :entry-format "%i %d\n%v"
    :value ()
    :notify (lambda (widget &rest ignore)
	      (widget-put widget 'dirty t))
    '(choice
      :tag "Address Type"
      :help-echo "Choose between alias and group"
      :value ("" . "")
      (cons :tag "Mail Alias"
	    :value ("" . "")
	    :format "%t\n%v\n"
	    (editable-field :tag "Alias"
			    :format "%[%t%]: %v"
			    :help-echo "Alias to use for person"
			    :validate blackbook-validate-alias)
	    (editable-field :tag "Address"
			    :format "%[%t%]: %v"
			    :help-echo "Email address of person"
			    :validate blackbook-validate-address))
      (cons :tag "Group of Users"
	    :value ("")
	    :format "%t\n%v\n"
	    :validate blackbook-validate-group
	    (editable-field :tag "Group"
			    :format "%[%t%]: %v"
			    :help-echo "Alias to use for group"
			    :validate blackbook-validate-alias)
	    (editable-list :tag "Members"
			   :format "%t:\n%v%i\n"
			   :extra-offset 3
			   :value ("")
			   (editable-field
			    :tag "Address"
			    :format "%[%t%]: %v"
			    :help-echo "Email address of group member"
			    :validate blackbook-validate-address))))))
  ;; keyboard
  (use-local-map (make-sparse-keymap))
  (set-keymap-parent (current-local-map) widget-keymap)
  (local-set-key "q" 'blackbook-done-callback)
  (local-set-key "\C-x\C-s" 'blackbook-save-callback)
  ;; & the rest...
  (widget-setup)
  (widget-put blackbook-aliases-widget 'dirty nil)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (forward-line 2)
  (message "Creating blackbook buffer... done"))


;;; Helper functions for alias parsers

;; Remove continuation lines \LFD from the buffer.
(defun blackbook-nuke-continuation ()
  (condition-case nil
      (progn
	(goto-char (point-min))
	(while (not (eobp))
	  (end-of-line)
	  (cond ((eq (char-after (1- (point))) ?\\)
		 (progn (delete-char -1) (delete-char 1) (insert " ")))
		(t
		 (forward-line 1)))))
    (error (error "Syntax error when removing `\\'"))))

;; If X is a string or a list with more than one element, return it
;; unchanged, otherwise return the CAR of the list.
(defsubst blackbook-collapse (x)
  (if (listp x)
      (if (cdr x) x (car x))
    x))

;; Add LIST to ALIASES
(defmacro blackbook-add-to-aliases (aliases name list)
  `(let ((found (assoc ,name ,aliases)))
     (if found
	 (setcdr found (blackbook-collapse ,list))
       (push (cons ,name (blackbook-collapse ,list)) ,aliases))))

;; Parse STR into pieces, where separate pieces can be delimited using
;; quotes.  Uses the Lisp reader to read the quoted parts.
(defsubst blackbook-parse-string (str)
  (let ((beg 0) (end 0)
	word res)
    (while (string-match ",?[ \t]*\\([^ ,\t\n\]+\\)" str end)
      (setq beg (match-beginning 1)
	    end (match-end 1)
	    word (substring str beg end))
      (when (eq ?\" (aref word 0))
	(let ((sexp (condition-case nil
			(read-from-string str beg)
		      (error (error)))))
	  (setq word (car sexp)
		end (cdr sexp))))
      (push word res))
    (nreverse res)))


;;; Alias parsers
;;
;; The parser for aliases.  The parser should return an alist whose
;; CAR is an string, and whose CDR is either an address, or a list of
;; addresses..
;;
;;   example return: (("one" . "some@address") ("two" "hmm" "himm"))
;;
;; This means that the defined aliases were "one" and "two"
;; (in that order).
;;
;; You can use the helper macro `blackbook-add-to-aliases' to add an
;; alias list to aliases.  Don't forget to call `nreverse' before
;; exiting, though.


;; The mailrc-parsing stuff is loosely based on the one from
;; mail-abbrevs.  This is less efficient, but much more
;; straight-forward.  Either way, its speed is not an issue while the
;; widget functions are as slow as they are.
(defun blackbook-parse-mail ()
  (let ((case-fold-search nil)
	(aliases nil))
    (blackbook-nuke-continuation)
    ;; Look for aliases
    (goto-char (point-min))
    (while (re-search-forward
	    "^[ \t]*\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+[^ \t\n]+"
	    nil t)
      (beginning-of-line)
      (cond ((looking-at "source[ \t]+\\([^ \t\n]+\\).*$")
	     (let ((file (substitute-in-file-name (match-string 1))))
	       ;; insert the new stuff
	       (delete-region (point) (match-end 0))
	       (insert-file-contents file)))
	    (t
	     (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
	     (let* ((name (buffer-substring
			   (match-beginning 1) (match-end 1)))
		    (expn (buffer-substring
			   (progn (skip-chars-forward " \t") (point))
			   (progn (end-of-line) (point))))
		    (list (condition-case nil
			      (blackbook-parse-string expn)
			    (error (error "Parse error near alias `%s'"
					  name)))))
	       (if list
		   (blackbook-add-to-aliases aliases name list)
		 (error "Empty alias `%s'" name))))))
    (nreverse aliases)))

;; This is very similar to the above, but different and with less
;; error-checking
(defun blackbook-parse-mutt ()
  (let ((case-fold-search nil)
	(aliases nil))
    (blackbook-nuke-continuation)
    ;; Look for aliases
    (goto-char (point-min))
    (while (re-search-forward
	    "^[ \t]*\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+[^ \t\n]+"
	    nil t)
      (beginning-of-line)
      (cond ((looking-at "source[ \t]+\\([^ \t\n]+\\).*$")
	     (let ((file (substitute-in-file-name (match-string 1))))
	       ;; insert the new stuff
	       (delete-region (point) (match-end 0))
	       (insert-file-contents file)))
	    (t
	     (condition-case nil
		 (re-search-forward "[ \t]+\\([^ \t\n]+\\)[ \t]+\\(.+\\)$")
	       (error (error "Syntax error")))
	     (let ((name (match-string 1))
		   (list (split-string (match-string 2) ",[ \t]*")))
	       (if list
		   (blackbook-add-to-aliases aliases name list)
		 (error "Empty alias `%s'" name))))))
    (nreverse aliases)))

;; Emacs 19.34 doesn't have split-string, which makes this kludge
;; necessary.
(if (fboundp 'split-string)
    (fset 'blackbook-split-string 'split-string)
  (defun blackbook-split-string (str pattern)
    (let ((l nil) (old 0) (new 0))
      (while (setq new (string-match pattern str old))
	(push (substring str old new) l)
	(setq old (1+ new)))
      (push (substring str old) l)
      (nreverse l))))

;; Parse elm's aliases.text
(defun blackbook-parse-elm ()
  (let ((case-fold-search nil)
	(aliases nil))
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at
	   "^[ \t]*\\([^ \t\n]+\\)[ \t]*=[^=\n]*=[ \t]*\\([^ \t\n]*\\)[ \t]*$")
	  (let ((name (match-string 1))
		(l (blackbook-split-string (match-string 2) ",")))
	    (blackbook-add-to-aliases aliases name l)))
      (forward-line 1))
    (nreverse aliases)))

;; Parse pine's .addressbook
(defun blackbook-parse-pine ()
  (let ((case-fold-search nil)
	(aliases nil))
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "^\\([^\t\n]+\\)\t[^\t\n]+\t\\([^\t\n]+\\)")
	  (let ((name (match-string 1))
		(addrs (match-string 2)))
	    (blackbook-add-to-aliases
	     aliases name
	     (if (eq (aref addrs 0) ?\()
		 (blackbook-split-string
		  (substring addrs 1 (1- (length addrs)))
		  ",")
	       addrs))))
      (forward-line 1))
    (nreverse aliases)))

(defun blackbook-read (file &optional type)
  "Read the mail aliases from FILE.
The optional argument TYPE can be a symbol `mail', `mutt', `elm',
or `pine'."
  (let (aliases)
    (when (file-exists-p file)
      (save-excursion
	(unwind-protect
	    (let ((buffer (generate-new-buffer " *aliases*")))
	      (buffer-disable-undo buffer)
	      (set-buffer buffer)
	      (insert-file-contents file)
	      (setq aliases
		    (cond ((or (null type)
			       (eq type 'mail))
			   (blackbook-parse-mail))
			  ((eq type 'mutt)
			   (blackbook-parse-mutt))
			  ((eq type 'elm)
			   (blackbook-parse-elm))
			  ((eq type 'pine)
			   (blackbook-parse-pine))
			  (t
			   (error "Unknown type %s" type)))))
	  (kill-buffer " *aliases*"))))
    aliases))


;;; Utility functions

(defun blackbook-resolve-alias (str aliases &optional so-far)
  "Resolve STR.
Returns a list of addresses, or nil, meaning that the alias is not defined."
  (when (member str so-far)
    (error "Alias loop detected: %s" str))
  (push str so-far)
  (let ((found (assoc str aliases)))
    (cond ((null found)
	   nil)
	  ((stringp (cdr found))
	   (cdr found))
	  (t
	   (let (res tmp)
	     (pop found)
	     (while found
	       (setq tmp (blackbook-resolve-alias (car found) aliases so-far))
	       (cond ((stringp tmp)
		      (setq tmp (list tmp)))
		     ((null tmp)
		      (setq tmp (list (car found)))))
	       (setq res (nconc res tmp))
	       (pop found))
	     (if (cdr res)
		 res
	       (car res)))))))


;;; Saving aliases

;; Quote the string if it contains spaces or stuff.
(defsubst blackbook-quote-maybe (str)
  (if (not (string-match "[, \t\"]" str))
      str
    (prin1-to-string str)))

;; Merge two sets of aliases
(defun blackbook-merge-aliases (old new)
  (setq old (nreverse old))
  (let (found)
    (while new
      (setq found (assoc (caar new) old))
      (if found
	  (setf (cdr found) (cdar new))
	(push (car new) old))
      (pop new)))
  (reverse old))

;; The actual function
(defun blackbook-save-mailrc (file aliases &optional merge)
  ;; if the aliases are to be merged, we must first read the existing
  ;; ones.
  (when merge
    (setq aliases (blackbook-merge-aliases merge aliases)))
  (save-excursion
    (unwind-protect
	(let ((buf (get-buffer-create " *saving aliases*")))
	  (buffer-disable-undo buf)
	  (set-buffer buf)
	  (erase-buffer)
	  (condition-case nil
	      (insert-file-contents file)
	    (error nil))
	  ;; Insert final newline, if none.
	  (unless (or (zerop (buffer-size))
		      (eq (char-after (1- (point-max))) ?\n))
	    (goto-char (point-max))
	    (insert ?\n))
	  ;; handle \LFD continuation lines
	  (goto-char (point-min))
	  (while (not (eobp))
	    (end-of-line)
	    (cond ((eq (char-after (1- (point))) ?\\)
		   (progn (delete-char -1) (delete-char 1) (insert ?\ )))
		  (t
		   (forward-char 1))))
	  ;; Kill all the alias/group/source entries.
	  (goto-char (point-min))
	  (let ((case-fold-search nil))
	    (while (re-search-forward
		    "^[ \t]*\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+[^ \t\n]+"
		    nil t)
	      (delete-region (match-beginning 0)
			     (progn (forward-line 1) (point)))))
	  (goto-char (point-max))
	  (while aliases
	    (if (stringp (cdar aliases))
		(insert "alias " (blackbook-quote-maybe (caar aliases))
			" " (blackbook-quote-maybe (cdar aliases)))
	      (insert "group " (blackbook-quote-maybe (caar aliases))
		      (mapconcat 'blackbook-quote-maybe (cdar aliases) " ")))
	    (insert ?\n)
	    (pop aliases))
	  (write-region (point-min) (point-max) file))
      (kill-buffer " *saving aliases*"))))



;;;###autoload
(defun blackbook ()
  "Edit the mail aliases."
  (interactive)
  (blackbook-setup-buffer))

(provide 'blackbook)

;;; blackbook.el ends here

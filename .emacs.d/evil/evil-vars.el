;;;; Settings and variables

;;; Setters

(defun evil-set-toggle-key (key)
  "Set `evil-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'evil-toggle-key)
                      evil-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (when (and (boundp 'evil-motion-state-map)
                 (keymapp evil-motion-state-map))
        (define-key evil-motion-state-map key 'evil-emacs-state)
        (define-key evil-motion-state-map old-key nil))
      (when (and (boundp 'evil-emacs-state-map)
                 (keymapp evil-emacs-state-map))
        (define-key evil-emacs-state-map key 'evil-exit-emacs-state)
        (define-key evil-emacs-state-map old-key nil)))))

;;; Customization group

(defgroup evil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'evil-)

(defcustom evil-auto-indent t
  "Whether to auto-indent when entering Insert state."
  :type  'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-auto-indent)

(defcustom evil-shift-width 4
  "The offset used by \\<evil-normal-state-map>\\[evil-shift-right] \
and \\[evil-shift-left]."
  :type 'integer
  :group 'evil)
(make-variable-buffer-local 'evil-shift-width)

(defcustom evil-default-cursor
  (list (or (frame-parameter nil 'cursor-color) "black") t)
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'evil)

(defcustom evil-repeat-move-cursor t
  "Whether \"\\<evil-normal-state-map>\\[evil-repeat]\" \
moves the cursor."
  :type 'boolean
  :group 'evil)

(defcustom evil-cross-lines nil
  "Whether motions may cross newlines."
  :type 'boolean
  :group 'evil)

(defcustom evil-move-cursor-back t
  "Whether the cursor is moved backwards when exiting Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-mode-line-format 'after
  "The position of the mode line tag.
`before' means before the mode list, `after' means after it,
and nil means no mode line tag."
  :type 'symbol
  :group 'evil)

(defcustom evil-word "[:word:]_"
  "The characters to be considered as a word.
This should be a regexp set without the enclosing []."
  :type 'string
  :group 'evil)
(make-variable-buffer-local 'evil-word)

(defcustom evil-want-fine-undo nil
  "Whether actions like \"cw\" are undone in several steps."
  :type 'boolean
  :group 'evil)

(defcustom evil-regexp-search t
  "Whether to use regular expressions for searching."
  :type  'boolean
  :group 'evil)

(defcustom evil-search-wrap t
  "Whether search wraps around."
  :type  'boolean
  :group 'evil)

(defcustom evil-flash-delay 2
  "Time in seconds to flash search matches."
  :type  'number
  :group 'evil)

(defcustom evil-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'evil)

(defcustom evil-esc-delay 0
  "Time in seconds to wait for another key after ESC."
  :type 'number
  :group 'evil)

(defcustom evil-show-paren-range 0
  "The minimal distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'evil)

(defcustom evil-want-C-i-jump t
  "Whether \"C-i\" jumps forward like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-u-scroll nil
  "Whether \"C-u\" scrolls like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-delete t
  "Whether \"C-w\" deletes a word in Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-in-emacs-state nil
  "Whether \"C-w\" prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-next-func
  (lambda (arg)
    (let ((dabbrev-search-these-buffers-only (list (current-buffer)))
          dabbrev-case-distinction)
      (condition-case nil
          (if (eq last-command this-command)
              (dabbrev-expand nil)
            (dabbrev-expand (- (abs (or arg 1)))))
        (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-previous-func
  (lambda (arg)
    (let ((dabbrev-search-these-buffers-only (list (current-buffer)))
          dabbrev-case-distinction)
      (dabbrev-expand arg)))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-line-func
  (lambda (arg)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line
             try-expand-line-all-buffers)))
      (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next-line]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-line-func
  evil-complete-next-line-func
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous-line]."
  :type 'function
  :group 'evil)

(defcustom evil-lookup-func 'woman
  "Lookup function used by \
\"\\<evil-motion-state-map>\\[evil-lookup]\"."
  :type 'function
  :group 'evil)

(defcustom evil-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'evil
  :set (lambda (sym value)
         (evil-set-toggle-key value)
         (set-default sym value)))

(defcustom evil-default-state 'normal
  "The default state.
This is the state a mode comes up in when it is not listed
in `evil-emacs-state-modes', `evil-insert-state-modes' or
`evil-motion-state-modes'. The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and
`emacs'."
  :type  'symbol
  :group 'evil)

(defcustom evil-emacs-state-modes
  '(bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    efs-mode
    Electric-buffer-menu-mode
    ert-results-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    mh-folder-mode
    monky-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    rcirc-mode
    recentf-dialog-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    term-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    gud-mode
    inferior-emacs-lisp-mode
    inferior-python-mode
    internal-ange-ftp-mode
    prolog-inferior-mode
    shell-mode
    slime-repl-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    command-history-mode
    compilation-mode
    help-mode
    Info-mode
    speedbar-mode
    undo-tree-visualizer-mode
    view-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-overriding-maps
  '((Buffer-menu-mode-map . "buff-menu")
    (comint-mode-map . comint)
    (compilation-mode-map . compile)
    (speedbar-key-map . speedbar)
    (speedbar-file-key-map . speedbar)
    (speedbar-buffers-key-map . speedbar))
  "Keymaps that should override global Evil maps.
Entries have the form (MAP-VAR . EVAL-AFTER), where MAP-VAR is
a keymap variable and EVAL-AFTER is the file or package defining it
\(ref. `eval-after-load')."
  :type '(alist :key-type symbol :value-type (choice symbol string))
  :group 'evil)

(defcustom evil-intercept-maps
  '((edebug-mode-map . edebug))
  "Keymaps that should override all Evil maps.
Entries have the form (MAP-VAR . EVAL-AFTER), where MAP-VAR is
a keymap variable and EVAL-AFTER is the file or package defining it
\(ref. `eval-after-load')."
  :type '(alist :key-type symbol :value-type (choice symbol string))
  :group 'evil)

(defcustom evil-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    digit-argument
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    redo
    right-char
    scroll-down
    scroll-up
    undo
    undo-tree-redo
    undo-tree-undo
    universal-argument
    universal-argument-minus
    universal-argument-more
    universal-argument-other-key
    up-list)
  "Non-Evil commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'evil)

(defface evil-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
  "Face for the info message in ex mode."
  :group 'evil)

;; Searching
(defcustom evil-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'evil)

(defcustom evil-ex-search-case 'smart
  "The case behaviour of the search command."
  :type '(radio (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'evil)

(defcustom evil-ex-substitute-case nil
  "The case behaviour of the search command."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'evil)

(defcustom evil-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'evil)

(defface evil-ex-search '((t :inherit isearch))
  "Face for interactive search."
  :group 'evil)

(defface evil-ex-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'evil)

(defface evil-ex-substitute '((((supports :underline))
                               :underline t
                               :foreground "red"))
  "Face for interactive replacement text."
  :group 'evil)

;;; Variables

(defvar evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")
(make-variable-buffer-local 'evil-state)

;; these may be used inside `evil-define-state'
(defvar evil-next-state nil
  "The Evil state being switched to.")
(make-variable-buffer-local 'evil-next-state)

(defvar evil-previous-state nil
  "The Evil state being switched from.")
(make-variable-buffer-local 'evil-previous-state)

(defvar evil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(make-variable-buffer-local 'evil-mode-line-tag)
(put 'evil-mode-line-tag 'risky-local-variable t)

(defvar evil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-state-properties nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(defvar evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-command-properties nil
  "Specifications made by `evil-define-command'.")

(defvar evil-transient-vars '(cua-mode transient-mark-mode)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(defvar evil-locked-display nil
  "If non-nil, state changes are invisible.
Don't set this directly; use the macro
`evil-with-locked-display' instead.")
(make-variable-buffer-local 'evil-locked-display)

(defvar evil-type-properties nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(defvar evil-motion-marker nil
  "Marker for storing the starting position of a motion.")
(make-variable-buffer-local 'evil-motion-marker)

(defvar evil-this-type nil
  "Current motion type.")
(make-variable-buffer-local 'evil-this-type)

(defvar evil-this-register nil
  "Current register.")
(make-variable-buffer-local 'evil-this-register)

(defvar evil-this-macro nil
  "Current macro register.")
(make-variable-buffer-local 'evil-this-macro)

(defvar evil-this-operator nil
  "Current operator.")
(make-variable-buffer-local 'evil-this-operator)

(defvar evil-this-motion nil
  "Current motion.")
(make-variable-buffer-local 'evil-this-motion)

(defvar evil-this-motion-count nil
  "Current motion count.")
(make-variable-buffer-local 'evil-this-motion-count)

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar evil-inhibit-operator-value nil
  "This variable is used to transfer the value
of `evil-inhibit-operator' from one local scope to another.")

;; used by `evil-execute-in-normal-state'
(defvar evil-old-move-cursor-back nil
  "Old value of `evil-move-cursor-back'.")

(defvar evil-operator-range-motion nil
  "Motion of `evil-operator-range'.")

(defvar evil-operator-range-beginning nil
  "Beginning of `evil-operator-range'.")

(defvar evil-operator-range-end nil
  "End of `evil-operator-range'.")

(defvar evil-operator-range-type nil
  "Type of `evil-operator-range'.")

(defvar evil-markers-alist
  '((?\( . evil-backward-sentence)
    (?\) . evil-forward-sentence)
    (?{ . evil-backward-paragraph)
    (?} . evil-forward-paragraph)
    (?' . evil-jump-backward)
    (?` . evil-jump-backward)
    (?< . evil-visual-beginning)
    (?> . evil-visual-end))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")
(make-variable-buffer-local 'evil-markers-alist)

(defvar evil-jump-list nil
  "Jump list.")
(make-variable-buffer-local 'evil-jump-list)

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap evil-suppress-map t)

;; TODO: customize size of ring
(defvar evil-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar evil-repeat-types
  '((t . evil-repeat-keystrokes)
    (change . evil-repeat-changes)
    (motion . evil-repeat-motion)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar evil-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar evil-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar evil-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar evil-repeat-info nil
  "Information accumulated during current repeat.")

(defvar evil-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar evil-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar evil-repeat-keys nil
  "The keys that invoked the current command.")

(defvar evil-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(defvar evil-insert-count nil
  "The explicit count passed to an command starting Insert state.")
(make-variable-buffer-local 'evil-insert-count)

(defvar evil-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number of function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")
(make-variable-buffer-local 'evil-insert-vcount)

(defvar evil-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(defvar evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")
(make-variable-buffer-local 'evil-insert-lines)

(defvar evil-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")
(make-variable-buffer-local 'evil-insert-repeat-info)

(defvar evil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")
(make-variable-buffer-local 'evil-replace-alist)

(defvar evil-echo-area-message nil
  "Previous value of `current-message'.")
(make-variable-buffer-local 'evil-echo-area-message)

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar evil-last-paste nil
  "Information about the latest paste.
This should be a list (CMD POINT BEG END) where CMD is the last
paste-command (either `evil-paste-before' or `evil-paste-after'),
POINT is the position of point before the paste,
BEG end END are the region of the inserted text.")

(defvar evil-paste-count nil
  "The count argument of the current paste command.")

(defvar evil-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(defvar evil-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")
(make-variable-buffer-local 'evil-undo-list-pointer)

(defvar evil-flash-timer nil
  "Timer for flashing search results.")

(defvar evil-search-prompt nil
  "String to use for search prompt.")

(defvar evil-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar evil-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(defvar evil-input-method nil
  "Input method used in Insert state and Emacs state.")
(make-variable-buffer-local 'evil-input-method)

(defvar evil-symbol-counter 0
  "Counter used by `evil-generate-symbol'.")

;;; Visual state

(defvar evil-visual-beginning nil
  "The beginning of the Visual selection, a marker.")
(make-variable-buffer-local 'evil-visual-beginning)

(defvar evil-visual-end nil
  "The end of the Visual selection, a marker.")
(make-variable-buffer-local 'evil-visual-end)

(defvar evil-visual-mark nil
  "The position of mark in Visual state, a marker.")
(make-variable-buffer-local 'evil-visual-mark)

(defvar evil-visual-point nil
  "The position of point in Visual state, a marker.")
(make-variable-buffer-local 'evil-visual-point)

(defvar evil-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `evil-define-visual-selection'.")
(make-variable-buffer-local 'evil-visual-selection)

(defvar evil-visual-type nil
  "The type of the Visual selection.
This is a type as defined by `evil-define-type'.")
(make-variable-buffer-local 'evil-visual-type)

;; we could infer the direction by comparing `evil-visual-mark'
;; and `evil-visual-point', but destructive operations may
;; displace the markers
(defvar evil-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `evil-visual-direction'.")
(make-variable-buffer-local 'evil-visual-direction)

(defvar evil-visual-properties nil
  "Property list of miscellaneous Visual properties.")
(make-variable-buffer-local 'evil-visual-properties)

(defvar evil-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")
(make-variable-buffer-local 'evil-visual-region-expanded)

(defvar evil-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `evil-visual-block-overlays'.")
(make-variable-buffer-local 'evil-visual-overlay)

(defvar evil-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")
(make-variable-buffer-local 'evil-visual-block-overlays)

(defvar evil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

;;; ex-mode

(defvar evil-ex-minibuffer nil
  "The currently active ex minibuffer.")

(defvar evil-ex-current-buffer nil
  "The buffer from which the current ex-mode has been started.")

(defvar evil-ex-last-cmd nil
  "The previously executed command.")

(defvar evil-ex-current-cmd nil
  "The currently parsed command.")

(defvar evil-ex-current-cmd-begin nil
  "The begin-position of the currently parsed command.")

(defvar evil-ex-current-cmd-end nil
  "The end-position of the currently parsed command.")

(defvar evil-ex-current-cmd-force nil
  "The force argument of the currently parsed command.")

(defvar evil-ex-current-arg nil
  "The currently parsed argument.")

(defvar evil-ex-current-range nil
  "The currenty parsed range.")

(defvar evil-ex-history nil
  "History of ex-commands.")

(defvar evil-ex-keymap (make-sparse-keymap)
  "Keymap used in ex-mode.")
(set-keymap-parent evil-ex-keymap minibuffer-local-completion-map)
(define-key evil-ex-keymap (kbd "SPC") #'self-insert-command)

(defvar evil-ex-commands nil
  "An alist of command-bindings to functions.")

(defvar evil-ex-current-arg-handler nil
  "Currently active argument handler depending on current command.")

(defvar evil-ex-arg-types-alist nil
  "An alist of defined argument handlers.")

;; Searching
(defvar evil-ex-search-history nil
  "The history for the search command.")

(defvar evil-ex-search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar evil-ex-search-count nil
  "The count if the current search.")

(defvar evil-ex-search-start-point nil
  "The point where the search started.")

(defvar evil-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar evil-ex-search-pattern nil
  "The actual search pattern.")

(defvar evil-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar evil-ex-search-match-end nil
  "The end position of the last match.")

(defvar evil-ex-substitute-pattern nil
  "The actual replacement.")

(defvar evil-ex-substitute-replacement nil
  "The actual replacement.")

;; The lazy-highlighting framework.
(defvar evil-ex-active-highlights-alist nil
  "An alist of currently active highlights.")
(make-variable-buffer-local 'evil-ex-active-highlights-alist)

(defvar evil-ex-hl-update-timer nil
  "Time used for updating highlights.")
(make-variable-buffer-local 'evil-ex-hl-update-timer)

(defvar evil-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")
(set-keymap-parent evil-ex-search-keymap minibuffer-local-map)

(defconst evil-version "0.1"
  "The current version of Evil")

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here

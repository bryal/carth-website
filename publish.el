(defun carth-org-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n"
           (coding-system-get org-html-coding-system 'mime-charset))
   (format "<title>%s</title>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (format "<meta name=\"author\" content=\"%s\">\n"
           (org-export-data (plist-get info :author) info))
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   (plist-get info :html-head)
   "\n</head>\n"
   "<body>\n"
   "<header>\n"
   (let ((link-home (org-trim (plist-get info :html-link-home))))
     (when (not (string= link-home ""))
       (concat "<nav id=\"sitenav\">\n"
               (format (plist-get info :html-home/up-format)
                       link-home)
               "</nav>\n")))
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (let ((subtitle (plist-get info :subtitle)))
     (if subtitle
         (format "<p class=\"subtitle\">%s</p>\n" (org-export-data subtitle info))
       ""))
   "</header>\n"
   contents
   (org-html--build-pre/postamble 'postamble info)
   "</body>\n"
   "</html>\n"))

(defun carth-org-html-inner-template (contents info)
  (concat
   "<main>\n"
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   "<article>\n"
   contents
   (org-html-footnote-section info)
   "</article>\n"
   "</main>\n"))

(defun carth-org-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information.

Like org-html-headline, but makes the section number a link to
the section for easy sharing."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (ids (delq nil
                      (progn
                        (list (org-element-property :CUSTOM_ID headline)
                              (org-export-get-reference headline info)
                              (org-element-property :ID headline)))))
           (preferred-id (car ids))
           (extra-ids
	    (mapconcat
	     (lambda (id)
	       (org-html--anchor
		(if (org-uuidgen-p id) (concat "ID-" id) id)
		nil nil info))
	     (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (org-html-format-list-item
              contents (if numberedp 'ordered 'unordered)
	      nil info nil
              (concat (org-html--anchor preferred-id nil nil info)
                      extra-ids
                      full-text)) "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (concat "outline-container-"
			  (org-export-get-reference headline info))
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (let* ((section-number-s (mapconcat #'number-to-string numbers "."))
                         )
                    (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                            level
                            preferred-id
                            extra-ids
                            (concat
                             (and numberedp
                                  ;; Link the section number to the section for easy sharing
                                  (format
                                   "<a class=\"section-number-%d\" href=\"#%s\">%s</a> "
                                   level
                                   preferred-id
                                   section-number-s))
                             full-text)
                            level))
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(org-export-define-derived-backend 'carth-html 'html
  :translate-alist '((template . carth-org-html-template)
                     (inner-template . carth-org-html-inner-template)
                     (headline . carth-org-html-headline))
  :options-alist
  '((:html-divs nil nil '((postamble "footer" "postamble")))))

(defun org-publish-to-carth-html (plist filename pub-dir)
  (let ((org-html-htmlize-output-type 'css))
    (org-publish-org-to 'carth-html filename
                        ".html"
                        plist pub-dir)))

(let* ((dir (file-name-directory load-file-name))
       (root "/")
       (projects `(("carth"
                    :base-directory ,dir
                    :base-extension "org"
                    :publishing-directory ,dir
                    :publishing-function org-publish-to-carth-html

                    :author "JoJo"
                    :email "jo@jo.zone"
                    :language "en"
                    :headline-levels 4
                    :with-footnotes t
                    :with-latex t
                    :with-tables t
                    :with-toc t
                    :with-todo-keywords nil
                    :exclude-tags ("noexport")

                    :html-head ,(format "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />" (concat root "css/style.css"))
                    :html-home/up-format
                    ,(concat "<a href=\"%s\">Home</a>"
                             "<a href=\"" root "reference.html\">Documentation</a>"
                             "<a href=\"https://gitlab.com/JoJoZ/carth/-/releases\">Downloads</a>"
                             "<a href=\"https://gitlab.com/JoJoZ/carth\">Repo</a>"
                             "<a href=\"https://gitlab.com/JoJoZ/carth-website\">Site repo</a>")
                    :html-postamble "<p class=\"author\">Author: %a (%e)</p><p class=\"date\">Last updated: %T</p><p class=\"creator\">%c</p>"
                    :html-link-home ,root
                    ))))
  (setq org-publish-project-alist
        (append projects org-publish-project-alist)))

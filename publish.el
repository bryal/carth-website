(defun carth-org-publish-to-html (plist filename pub-dir)
  (let ((org-html-htmlize-output-type 'css))
    (jojo-org-publish-to-html plist filename pub-dir)))

(let* ((dir (file-name-directory load-file-name))
       (root "/")
       (projects `(("carth"
                    :base-directory ,dir
                    :base-extension "org"
                    :publishing-directory ,dir
                    :publishing-function carth-org-publish-to-html

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
                    ,(concat "<a href=\"%s\"><img src='" root "img/logo_512.png' /></a>"
                             "<a href=\"" root "reference.html\">Documentation</a>"
                             "<a href=\"https://github.com/bryal/carth/releases\">Releases</a>"
                             "<a href=\"https://github.com/bryal/carth\">Source</a>"
                             )
                    :html-postamble "<p class=\"author\">Author: %a (%e)</p><p class=\"date\">Last updated: %T</p><p class=\"creator\">%c</p>"
                    :html-link-home ,root
                    ))))
  (setq org-publish-project-alist
        (append projects org-publish-project-alist)))

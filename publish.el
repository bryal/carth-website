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
                    :with-toc 2
                    :with-todo-keywords nil
                    :exclude-tags ("noexport")

                    :html-head ,(concat (format "<link rel=\"shortcut icon\" href=\"%s\"/>" (concat root "img/logo_128.png"))
                                        (format "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />" (concat root "css/style.css"))
                                        (format "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />" (concat root "css/font-Alegreya.css"))
                                        (format "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />" (concat root "css/font-Iosevka.css")))
                    :html-home/up-format
                    ,(concat "<a href=\"%s\"><img src='" root "img/logo_512.png' alt='Carth logo' title='Home'/></a>"
                             "<a href=\"" root "guide.html\">Guide</a>"
                             "<a href=\"" root "reference.html\">Reference</a>"
                             "<a href=\"https://github.com/bryal/carth/releases\">Releases</a>"
                             "<a href=\"https://github.com/bryal/carth\">Source</a>"
                             )
                    :html-postamble "<p class=\"author\">Author: %a (%e)</p><p class=\"date\">Last updated: %C</p><p class=\"creator\">%c</p>"
                    :html-link-home ,root
                    ))))
  (setq org-publish-project-alist
        (append projects org-publish-project-alist)))

p1_doc.html: FORCE
	pandoc --self-contained --css=blog.css --filter pandoc-citeproc --toc p1_doc.md >$@

clean: FORCE
	rm -f p1_doc.html

FORCE:

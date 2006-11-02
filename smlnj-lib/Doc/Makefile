# Generated makefile
#

MLDOC_BIN = /n/stew/home/jhr/SGML/ML-Doc/bin

PROOF = $(MLDOC_BIN)/proof.sh
EXTRACT_INFO = $(MLDOC_BIN)/extract-info.sh
MERGE_INFO = $(MLDOC_BIN)/merge-info.sh
HTML_GEN = $(MLDOC_BIN)/html-gen.sh
HTML_INDEX = $(MLDOC_BIN)/html-index.sh
HTML_TOC = $(MLDOC_BIN)/html-toc.sh

SRC_DIR = ML-Doc
PROOF_DIR = Proof
INFO_DIR = Info
HTML_DIR = HTML

CONFIG = Config.cfg CATALOG
INFO = $(CONFIG) $(INFO_DIR)/Master.info

HTML_FILES =	$(HTML_DIR)/index-all.html \
	$(HTML_DIR)/toc.html \
	$(HTML_DIR)/array-qsort-fn.html \
	$(HTML_DIR)/array-qsort.html \
	$(HTML_DIR)/array-sort.html \
	$(HTML_DIR)/binary-map-fn.html \
	$(HTML_DIR)/binary-set-fn.html \
	$(HTML_DIR)/bit-array.html \
	$(HTML_DIR)/bit-vector.html \
	$(HTML_DIR)/bsearch.html \
	$(HTML_DIR)/char-map.html \
	$(HTML_DIR)/dynamic-array.html \
	$(HTML_DIR)/engine-sig.html \
	$(HTML_DIR)/fifo.html \
	$(HTML_DIR)/intro-part.html \
	$(HTML_DIR)/iterate.html \
	$(HTML_DIR)/lib-base.html \
	$(HTML_DIR)/list-format.html \
	$(HTML_DIR)/list-map-fn.html \
	$(HTML_DIR)/list-set-fn.html \
	$(HTML_DIR)/list-xprod.html \
	$(HTML_DIR)/listsort.html \
	$(HTML_DIR)/match-tree.html \
	$(HTML_DIR)/mono-array-sort.html \
	$(HTML_DIR)/mono-dynamic-array.html \
	$(HTML_DIR)/ord-key.html \
	$(HTML_DIR)/ord-map.html \
	$(HTML_DIR)/ord-set.html \
	$(HTML_DIR)/parser-sig.html \
	$(HTML_DIR)/queue.html \
	$(HTML_DIR)/rand.html \
	$(HTML_DIR)/random.html \
	$(HTML_DIR)/regexp-fn.html \
	$(HTML_DIR)/regexp-intro.html \
	$(HTML_DIR)/regexp-lib-part.html \
	$(HTML_DIR)/regexp-sig.html \
	$(HTML_DIR)/smlnj-lib.html \
	$(HTML_DIR)/splay-map-fn.html \
	$(HTML_DIR)/splay-set-fn.html \
	$(HTML_DIR)/splaytree.html \
	$(HTML_DIR)/syntax.html \
	$(HTML_DIR)/util-lib-part.html

INFO_FILES =	$(INFO_DIR)/array-qsort-fn.info \
	$(INFO_DIR)/array-qsort.info \
	$(INFO_DIR)/array-sort.info \
	$(INFO_DIR)/binary-map-fn.info \
	$(INFO_DIR)/binary-set-fn.info \
	$(INFO_DIR)/bit-array.info \
	$(INFO_DIR)/bit-vector.info \
	$(INFO_DIR)/bsearch.info \
	$(INFO_DIR)/char-map.info \
	$(INFO_DIR)/dynamic-array.info \
	$(INFO_DIR)/engine-sig.info \
	$(INFO_DIR)/fifo.info \
	$(INFO_DIR)/intro-part.info \
	$(INFO_DIR)/iterate.info \
	$(INFO_DIR)/lib-base.info \
	$(INFO_DIR)/list-format.info \
	$(INFO_DIR)/list-map-fn.info \
	$(INFO_DIR)/list-set-fn.info \
	$(INFO_DIR)/list-xprod.info \
	$(INFO_DIR)/listsort.info \
	$(INFO_DIR)/match-tree.info \
	$(INFO_DIR)/mono-array-sort.info \
	$(INFO_DIR)/mono-dynamic-array.info \
	$(INFO_DIR)/ord-key.info \
	$(INFO_DIR)/ord-map.info \
	$(INFO_DIR)/ord-set.info \
	$(INFO_DIR)/parser-sig.info \
	$(INFO_DIR)/queue.info \
	$(INFO_DIR)/rand.info \
	$(INFO_DIR)/random.info \
	$(INFO_DIR)/regexp-fn.info \
	$(INFO_DIR)/regexp-intro.info \
	$(INFO_DIR)/regexp-lib-part.info \
	$(INFO_DIR)/regexp-sig.info \
	$(INFO_DIR)/smlnj-lib.info \
	$(INFO_DIR)/splay-map-fn.info \
	$(INFO_DIR)/splay-set-fn.info \
	$(INFO_DIR)/splaytree.info \
	$(INFO_DIR)/syntax.info \
	$(INFO_DIR)/util-lib-part.info

PROOF_FILES =	$(PROOF_DIR)/array-qsort-fn.tex \
	$(PROOF_DIR)/array-qsort.tex \
	$(PROOF_DIR)/array-sort.tex \
	$(PROOF_DIR)/binary-map-fn.tex \
	$(PROOF_DIR)/binary-set-fn.tex \
	$(PROOF_DIR)/bit-array.tex \
	$(PROOF_DIR)/bit-vector.tex \
	$(PROOF_DIR)/bsearch.tex \
	$(PROOF_DIR)/char-map.tex \
	$(PROOF_DIR)/dynamic-array.tex \
	$(PROOF_DIR)/engine-sig.tex \
	$(PROOF_DIR)/fifo.tex \
	$(PROOF_DIR)/intro-part.tex \
	$(PROOF_DIR)/iterate.tex \
	$(PROOF_DIR)/lib-base.tex \
	$(PROOF_DIR)/list-format.tex \
	$(PROOF_DIR)/list-map-fn.tex \
	$(PROOF_DIR)/list-set-fn.tex \
	$(PROOF_DIR)/list-xprod.tex \
	$(PROOF_DIR)/listsort.tex \
	$(PROOF_DIR)/match-tree.tex \
	$(PROOF_DIR)/mono-array-sort.tex \
	$(PROOF_DIR)/mono-dynamic-array.tex \
	$(PROOF_DIR)/ord-key.tex \
	$(PROOF_DIR)/ord-map.tex \
	$(PROOF_DIR)/ord-set.tex \
	$(PROOF_DIR)/parser-sig.tex \
	$(PROOF_DIR)/queue.tex \
	$(PROOF_DIR)/rand.tex \
	$(PROOF_DIR)/random.tex \
	$(PROOF_DIR)/regexp-fn.tex \
	$(PROOF_DIR)/regexp-intro.tex \
	$(PROOF_DIR)/regexp-lib-part.tex \
	$(PROOF_DIR)/regexp-sig.tex \
	$(PROOF_DIR)/smlnj-lib.tex \
	$(PROOF_DIR)/splay-map-fn.tex \
	$(PROOF_DIR)/splay-set-fn.tex \
	$(PROOF_DIR)/splaytree.tex \
	$(PROOF_DIR)/syntax.tex \
	$(PROOF_DIR)/util-lib-part.tex

HTML:	$(HTML_FILES)

$(HTML_DIR)/toc.html:	$(INFO)
	$(HTML_TOC)

$(HTML_DIR)/index-all.html:	$(INFO)
	$(HTML_INDEX) -all

$(HTML_DIR)/array-qsort-fn.html:	$(SRC_DIR)/array-qsort-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/array-qsort-fn.mldoc

$(HTML_DIR)/array-qsort.html:	$(SRC_DIR)/array-qsort.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/array-qsort.mldoc

$(HTML_DIR)/array-sort.html:	$(SRC_DIR)/array-sort.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/array-sort.mldoc

$(HTML_DIR)/binary-map-fn.html:	$(SRC_DIR)/binary-map-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/binary-map-fn.mldoc

$(HTML_DIR)/binary-set-fn.html:	$(SRC_DIR)/binary-set-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/binary-set-fn.mldoc

$(HTML_DIR)/bit-array.html:	$(SRC_DIR)/bit-array.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/bit-array.mldoc

$(HTML_DIR)/bit-vector.html:	$(SRC_DIR)/bit-vector.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/bit-vector.mldoc

$(HTML_DIR)/bsearch.html:	$(SRC_DIR)/bsearch.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/bsearch.mldoc

$(HTML_DIR)/char-map.html:	$(SRC_DIR)/char-map.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/char-map.mldoc

$(HTML_DIR)/dynamic-array.html:	$(SRC_DIR)/dynamic-array.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/dynamic-array.mldoc

$(HTML_DIR)/engine-sig.html:	$(SRC_DIR)/engine-sig.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/engine-sig.mldoc

$(HTML_DIR)/fifo.html:	$(SRC_DIR)/fifo.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/fifo.mldoc

$(HTML_DIR)/intro-part.html:	$(SRC_DIR)/intro-part.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/intro-part.mldoc

$(HTML_DIR)/iterate.html:	$(SRC_DIR)/iterate.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/iterate.mldoc

$(HTML_DIR)/lib-base.html:	$(SRC_DIR)/lib-base.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/lib-base.mldoc

$(HTML_DIR)/list-format.html:	$(SRC_DIR)/list-format.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/list-format.mldoc

$(HTML_DIR)/list-map-fn.html:	$(SRC_DIR)/list-map-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/list-map-fn.mldoc

$(HTML_DIR)/list-set-fn.html:	$(SRC_DIR)/list-set-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/list-set-fn.mldoc

$(HTML_DIR)/list-xprod.html:	$(SRC_DIR)/list-xprod.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/list-xprod.mldoc

$(HTML_DIR)/listsort.html:	$(SRC_DIR)/listsort.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/listsort.mldoc

$(HTML_DIR)/match-tree.html:	$(SRC_DIR)/match-tree.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/match-tree.mldoc

$(HTML_DIR)/mono-array-sort.html:	$(SRC_DIR)/mono-array-sort.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/mono-array-sort.mldoc

$(HTML_DIR)/mono-dynamic-array.html:	$(SRC_DIR)/mono-dynamic-array.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/mono-dynamic-array.mldoc

$(HTML_DIR)/ord-key.html:	$(SRC_DIR)/ord-key.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/ord-key.mldoc

$(HTML_DIR)/ord-map.html:	$(SRC_DIR)/ord-map.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/ord-map.mldoc

$(HTML_DIR)/ord-set.html:	$(SRC_DIR)/ord-set.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/ord-set.mldoc

$(HTML_DIR)/parser-sig.html:	$(SRC_DIR)/parser-sig.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/parser-sig.mldoc

$(HTML_DIR)/queue.html:	$(SRC_DIR)/queue.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/queue.mldoc

$(HTML_DIR)/rand.html:	$(SRC_DIR)/rand.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/rand.mldoc

$(HTML_DIR)/random.html:	$(SRC_DIR)/random.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/random.mldoc

$(HTML_DIR)/regexp-fn.html:	$(SRC_DIR)/regexp-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/regexp-fn.mldoc

$(HTML_DIR)/regexp-intro.html:	$(SRC_DIR)/regexp-intro.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/regexp-intro.mldoc

$(HTML_DIR)/regexp-lib-part.html:	$(SRC_DIR)/regexp-lib-part.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/regexp-lib-part.mldoc

$(HTML_DIR)/regexp-sig.html:	$(SRC_DIR)/regexp-sig.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/regexp-sig.mldoc

$(HTML_DIR)/smlnj-lib.html:	$(SRC_DIR)/smlnj-lib.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/smlnj-lib.mldoc

$(HTML_DIR)/splay-map-fn.html:	$(SRC_DIR)/splay-map-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/splay-map-fn.mldoc

$(HTML_DIR)/splay-set-fn.html:	$(SRC_DIR)/splay-set-fn.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/splay-set-fn.mldoc

$(HTML_DIR)/splaytree.html:	$(SRC_DIR)/splaytree.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/splaytree.mldoc

$(HTML_DIR)/syntax.html:	$(SRC_DIR)/syntax.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/syntax.mldoc

$(HTML_DIR)/util-lib-part.html:	$(SRC_DIR)/util-lib-part.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/util-lib-part.mldoc

$(INFO_DIR)/Master.info:	$(INFO_FILES)
	$(MERGE_INFO) $(INFO_FILES)

$(INFO_DIR)/array-qsort-fn.info:	$(SRC_DIR)/array-qsort-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/array-qsort-fn.mldoc

$(INFO_DIR)/array-qsort.info:	$(SRC_DIR)/array-qsort.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/array-qsort.mldoc

$(INFO_DIR)/array-sort.info:	$(SRC_DIR)/array-sort.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/array-sort.mldoc

$(INFO_DIR)/binary-map-fn.info:	$(SRC_DIR)/binary-map-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/binary-map-fn.mldoc

$(INFO_DIR)/binary-set-fn.info:	$(SRC_DIR)/binary-set-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/binary-set-fn.mldoc

$(INFO_DIR)/bit-array.info:	$(SRC_DIR)/bit-array.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/bit-array.mldoc

$(INFO_DIR)/bit-vector.info:	$(SRC_DIR)/bit-vector.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/bit-vector.mldoc

$(INFO_DIR)/bsearch.info:	$(SRC_DIR)/bsearch.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/bsearch.mldoc

$(INFO_DIR)/char-map.info:	$(SRC_DIR)/char-map.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/char-map.mldoc

$(INFO_DIR)/dynamic-array.info:	$(SRC_DIR)/dynamic-array.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/dynamic-array.mldoc

$(INFO_DIR)/engine-sig.info:	$(SRC_DIR)/engine-sig.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/engine-sig.mldoc

$(INFO_DIR)/fifo.info:	$(SRC_DIR)/fifo.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/fifo.mldoc

$(INFO_DIR)/intro-part.info:	$(SRC_DIR)/intro-part.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/intro-part.mldoc

$(INFO_DIR)/iterate.info:	$(SRC_DIR)/iterate.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/iterate.mldoc

$(INFO_DIR)/lib-base.info:	$(SRC_DIR)/lib-base.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/lib-base.mldoc

$(INFO_DIR)/list-format.info:	$(SRC_DIR)/list-format.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/list-format.mldoc

$(INFO_DIR)/list-map-fn.info:	$(SRC_DIR)/list-map-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/list-map-fn.mldoc

$(INFO_DIR)/list-set-fn.info:	$(SRC_DIR)/list-set-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/list-set-fn.mldoc

$(INFO_DIR)/list-xprod.info:	$(SRC_DIR)/list-xprod.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/list-xprod.mldoc

$(INFO_DIR)/listsort.info:	$(SRC_DIR)/listsort.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/listsort.mldoc

$(INFO_DIR)/match-tree.info:	$(SRC_DIR)/match-tree.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/match-tree.mldoc

$(INFO_DIR)/mono-array-sort.info:	$(SRC_DIR)/mono-array-sort.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/mono-array-sort.mldoc

$(INFO_DIR)/mono-dynamic-array.info:	$(SRC_DIR)/mono-dynamic-array.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/mono-dynamic-array.mldoc

$(INFO_DIR)/ord-key.info:	$(SRC_DIR)/ord-key.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/ord-key.mldoc

$(INFO_DIR)/ord-map.info:	$(SRC_DIR)/ord-map.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/ord-map.mldoc

$(INFO_DIR)/ord-set.info:	$(SRC_DIR)/ord-set.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/ord-set.mldoc

$(INFO_DIR)/parser-sig.info:	$(SRC_DIR)/parser-sig.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/parser-sig.mldoc

$(INFO_DIR)/queue.info:	$(SRC_DIR)/queue.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/queue.mldoc

$(INFO_DIR)/rand.info:	$(SRC_DIR)/rand.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/rand.mldoc

$(INFO_DIR)/random.info:	$(SRC_DIR)/random.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/random.mldoc

$(INFO_DIR)/regexp-fn.info:	$(SRC_DIR)/regexp-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/regexp-fn.mldoc

$(INFO_DIR)/regexp-intro.info:	$(SRC_DIR)/regexp-intro.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/regexp-intro.mldoc

$(INFO_DIR)/regexp-lib-part.info:	$(SRC_DIR)/regexp-lib-part.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/regexp-lib-part.mldoc

$(INFO_DIR)/regexp-sig.info:	$(SRC_DIR)/regexp-sig.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/regexp-sig.mldoc

$(INFO_DIR)/smlnj-lib.info:	$(SRC_DIR)/smlnj-lib.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/smlnj-lib.mldoc

$(INFO_DIR)/splay-map-fn.info:	$(SRC_DIR)/splay-map-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/splay-map-fn.mldoc

$(INFO_DIR)/splay-set-fn.info:	$(SRC_DIR)/splay-set-fn.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/splay-set-fn.mldoc

$(INFO_DIR)/splaytree.info:	$(SRC_DIR)/splaytree.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/splaytree.mldoc

$(INFO_DIR)/syntax.info:	$(SRC_DIR)/syntax.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/syntax.mldoc

$(INFO_DIR)/util-lib-part.info:	$(SRC_DIR)/util-lib-part.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/util-lib-part.mldoc

Proof:	$(PROOF_FILES)

$(PROOF_DIR)/array-qsort-fn.tex:	$(SRC_DIR)/array-qsort-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/array-qsort-fn.mldoc

$(PROOF_DIR)/array-qsort.tex:	$(SRC_DIR)/array-qsort.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/array-qsort.mldoc

$(PROOF_DIR)/array-sort.tex:	$(SRC_DIR)/array-sort.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/array-sort.mldoc

$(PROOF_DIR)/binary-map-fn.tex:	$(SRC_DIR)/binary-map-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/binary-map-fn.mldoc

$(PROOF_DIR)/binary-set-fn.tex:	$(SRC_DIR)/binary-set-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/binary-set-fn.mldoc

$(PROOF_DIR)/bit-array.tex:	$(SRC_DIR)/bit-array.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/bit-array.mldoc

$(PROOF_DIR)/bit-vector.tex:	$(SRC_DIR)/bit-vector.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/bit-vector.mldoc

$(PROOF_DIR)/bsearch.tex:	$(SRC_DIR)/bsearch.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/bsearch.mldoc

$(PROOF_DIR)/char-map.tex:	$(SRC_DIR)/char-map.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/char-map.mldoc

$(PROOF_DIR)/dynamic-array.tex:	$(SRC_DIR)/dynamic-array.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/dynamic-array.mldoc

$(PROOF_DIR)/engine-sig.tex:	$(SRC_DIR)/engine-sig.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/engine-sig.mldoc

$(PROOF_DIR)/fifo.tex:	$(SRC_DIR)/fifo.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/fifo.mldoc

$(PROOF_DIR)/intro-part.tex:	$(SRC_DIR)/intro-part.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/intro-part.mldoc

$(PROOF_DIR)/iterate.tex:	$(SRC_DIR)/iterate.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/iterate.mldoc

$(PROOF_DIR)/lib-base.tex:	$(SRC_DIR)/lib-base.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/lib-base.mldoc

$(PROOF_DIR)/list-format.tex:	$(SRC_DIR)/list-format.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/list-format.mldoc

$(PROOF_DIR)/list-map-fn.tex:	$(SRC_DIR)/list-map-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/list-map-fn.mldoc

$(PROOF_DIR)/list-set-fn.tex:	$(SRC_DIR)/list-set-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/list-set-fn.mldoc

$(PROOF_DIR)/list-xprod.tex:	$(SRC_DIR)/list-xprod.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/list-xprod.mldoc

$(PROOF_DIR)/listsort.tex:	$(SRC_DIR)/listsort.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/listsort.mldoc

$(PROOF_DIR)/match-tree.tex:	$(SRC_DIR)/match-tree.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/match-tree.mldoc

$(PROOF_DIR)/mono-array-sort.tex:	$(SRC_DIR)/mono-array-sort.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/mono-array-sort.mldoc

$(PROOF_DIR)/mono-dynamic-array.tex:	$(SRC_DIR)/mono-dynamic-array.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/mono-dynamic-array.mldoc

$(PROOF_DIR)/ord-key.tex:	$(SRC_DIR)/ord-key.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/ord-key.mldoc

$(PROOF_DIR)/ord-map.tex:	$(SRC_DIR)/ord-map.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/ord-map.mldoc

$(PROOF_DIR)/ord-set.tex:	$(SRC_DIR)/ord-set.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/ord-set.mldoc

$(PROOF_DIR)/parser-sig.tex:	$(SRC_DIR)/parser-sig.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/parser-sig.mldoc

$(PROOF_DIR)/queue.tex:	$(SRC_DIR)/queue.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/queue.mldoc

$(PROOF_DIR)/rand.tex:	$(SRC_DIR)/rand.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/rand.mldoc

$(PROOF_DIR)/random.tex:	$(SRC_DIR)/random.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/random.mldoc

$(PROOF_DIR)/regexp-fn.tex:	$(SRC_DIR)/regexp-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/regexp-fn.mldoc

$(PROOF_DIR)/regexp-intro.tex:	$(SRC_DIR)/regexp-intro.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/regexp-intro.mldoc

$(PROOF_DIR)/regexp-lib-part.tex:	$(SRC_DIR)/regexp-lib-part.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/regexp-lib-part.mldoc

$(PROOF_DIR)/regexp-sig.tex:	$(SRC_DIR)/regexp-sig.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/regexp-sig.mldoc

$(PROOF_DIR)/smlnj-lib.tex:	$(SRC_DIR)/smlnj-lib.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/smlnj-lib.mldoc

$(PROOF_DIR)/splay-map-fn.tex:	$(SRC_DIR)/splay-map-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/splay-map-fn.mldoc

$(PROOF_DIR)/splay-set-fn.tex:	$(SRC_DIR)/splay-set-fn.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/splay-set-fn.mldoc

$(PROOF_DIR)/splaytree.tex:	$(SRC_DIR)/splaytree.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/splaytree.mldoc

$(PROOF_DIR)/syntax.tex:	$(SRC_DIR)/syntax.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/syntax.mldoc

$(PROOF_DIR)/util-lib-part.tex:	$(SRC_DIR)/util-lib-part.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/util-lib-part.mldoc

clean-info:
	rm -f $(INFO_DIR)/Master.info $(INFO_FILES)

clean-proof:
	rm -f $(PROOF_FILES)
	rm -f $(PROOF_DIR)/*.aux
	rm -f $(PROOF_DIR)/*.bbl
	rm -f $(PROOF_DIR)/*.blg
	rm -f $(PROOF_DIR)/*.dvi
	rm -f $(PROOF_DIR)/*.idx
	rm -f $(PROOF_DIR)/*.ind
	rm -f $(PROOF_DIR)/*.log
	rm -f $(PROOF_DIR)/*.ps
	rm -f $(PROOF_DIR)/*.toc

clean-html:
	rm -f $(HTML_FILES)

clean:	clean-info clean-proof clean-html


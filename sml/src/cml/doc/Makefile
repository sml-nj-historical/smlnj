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
	$(HTML_DIR)/refman.html \
	$(HTML_DIR)/basics.html \
	$(HTML_DIR)/porting.html \
	$(HTML_DIR)/core-cml.html \
	$(HTML_DIR)/cml.html \
	$(HTML_DIR)/mailbox.html \
	$(HTML_DIR)/sync-var.html \
	$(HTML_DIR)/os.html \
	$(HTML_DIR)/os-process.html \
	$(HTML_DIR)/os-io.html \
	$(HTML_DIR)/cml-lib.html \
	$(HTML_DIR)/multicast.html \
	$(HTML_DIR)/trace-cml.html

INFO_FILES =	$(INFO_DIR)/refman.info \
	$(INFO_DIR)/basics.info \
	$(INFO_DIR)/porting.info \
	$(INFO_DIR)/core-cml.info \
	$(INFO_DIR)/cml.info \
	$(INFO_DIR)/mailbox.info \
	$(INFO_DIR)/sync-var.info \
	$(INFO_DIR)/os.info \
	$(INFO_DIR)/os-process.info \
	$(INFO_DIR)/os-io.info \
	$(INFO_DIR)/cml-lib.info \
	$(INFO_DIR)/multicast.info \
	$(INFO_DIR)/trace-cml.info

PROOF_FILES =	$(PROOF_DIR)/refman.tex \
	$(PROOF_DIR)/basics.tex \
	$(PROOF_DIR)/porting.tex \
	$(PROOF_DIR)/core-cml.tex \
	$(PROOF_DIR)/cml.tex \
	$(PROOF_DIR)/mailbox.tex \
	$(PROOF_DIR)/sync-var.tex \
	$(PROOF_DIR)/os.tex \
	$(PROOF_DIR)/os-process.tex \
	$(PROOF_DIR)/os-io.tex \
	$(PROOF_DIR)/cml-lib.tex \
	$(PROOF_DIR)/multicast.tex \
	$(PROOF_DIR)/trace-cml.tex

HTML:	$(HTML_FILES)

$(HTML_DIR)/toc.html:	$(INFO)
	$(HTML_TOC)

$(HTML_DIR)/index-all.html:	$(INFO)
	$(HTML_INDEX) -all

$(HTML_DIR)/refman.html:	$(SRC_DIR)/refman.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/refman.mldoc

$(HTML_DIR)/basics.html:	$(SRC_DIR)/basics.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/basics.mldoc

$(HTML_DIR)/porting.html:	$(SRC_DIR)/porting.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/porting.mldoc

$(HTML_DIR)/core-cml.html:	$(SRC_DIR)/core-cml.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/core-cml.mldoc

$(HTML_DIR)/cml.html:	$(SRC_DIR)/cml.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/cml.mldoc

$(HTML_DIR)/mailbox.html:	$(SRC_DIR)/mailbox.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/mailbox.mldoc

$(HTML_DIR)/sync-var.html:	$(SRC_DIR)/sync-var.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/sync-var.mldoc

$(HTML_DIR)/os.html:	$(SRC_DIR)/os.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/os.mldoc

$(HTML_DIR)/os-process.html:	$(SRC_DIR)/os-process.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/os-process.mldoc

$(HTML_DIR)/os-io.html:	$(SRC_DIR)/os-io.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/os-io.mldoc

$(HTML_DIR)/cml-lib.html:	$(SRC_DIR)/cml-lib.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/cml-lib.mldoc

$(HTML_DIR)/multicast.html:	$(SRC_DIR)/multicast.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/multicast.mldoc

$(HTML_DIR)/trace-cml.html:	$(SRC_DIR)/trace-cml.mldoc $(INFO)
	$(HTML_GEN) $(SRC_DIR)/trace-cml.mldoc

$(INFO_DIR)/Master.info:	$(INFO_FILES)
	$(MERGE_INFO) $(INFO_FILES)

$(INFO_DIR)/refman.info:	$(SRC_DIR)/refman.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/refman.mldoc

$(INFO_DIR)/basics.info:	$(SRC_DIR)/basics.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/basics.mldoc

$(INFO_DIR)/porting.info:	$(SRC_DIR)/porting.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/porting.mldoc

$(INFO_DIR)/core-cml.info:	$(SRC_DIR)/core-cml.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/core-cml.mldoc

$(INFO_DIR)/cml.info:	$(SRC_DIR)/cml.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/cml.mldoc

$(INFO_DIR)/mailbox.info:	$(SRC_DIR)/mailbox.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/mailbox.mldoc

$(INFO_DIR)/sync-var.info:	$(SRC_DIR)/sync-var.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/sync-var.mldoc

$(INFO_DIR)/os.info:	$(SRC_DIR)/os.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/os.mldoc

$(INFO_DIR)/os-process.info:	$(SRC_DIR)/os-process.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/os-process.mldoc

$(INFO_DIR)/os-io.info:	$(SRC_DIR)/os-io.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/os-io.mldoc

$(INFO_DIR)/cml-lib.info:	$(SRC_DIR)/cml-lib.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/cml-lib.mldoc

$(INFO_DIR)/multicast.info:	$(SRC_DIR)/multicast.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/multicast.mldoc

$(INFO_DIR)/trace-cml.info:	$(SRC_DIR)/trace-cml.mldoc $(CONFIG)
	$(EXTRACT_INFO) $(SRC_DIR)/trace-cml.mldoc

Proof:	$(ProofFiles)

$(PROOF_DIR)/refman.tex:	$(SRC_DIR)/refman.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/refman.mldoc

$(PROOF_DIR)/basics.tex:	$(SRC_DIR)/basics.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/basics.mldoc

$(PROOF_DIR)/porting.tex:	$(SRC_DIR)/porting.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/porting.mldoc

$(PROOF_DIR)/core-cml.tex:	$(SRC_DIR)/core-cml.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/core-cml.mldoc

$(PROOF_DIR)/cml.tex:	$(SRC_DIR)/cml.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/cml.mldoc

$(PROOF_DIR)/mailbox.tex:	$(SRC_DIR)/mailbox.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/mailbox.mldoc

$(PROOF_DIR)/sync-var.tex:	$(SRC_DIR)/sync-var.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/sync-var.mldoc

$(PROOF_DIR)/os.tex:	$(SRC_DIR)/os.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/os.mldoc

$(PROOF_DIR)/os-process.tex:	$(SRC_DIR)/os-process.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/os-process.mldoc

$(PROOF_DIR)/os-io.tex:	$(SRC_DIR)/os-io.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/os-io.mldoc

$(PROOF_DIR)/cml-lib.tex:	$(SRC_DIR)/cml-lib.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/cml-lib.mldoc

$(PROOF_DIR)/multicast.tex:	$(SRC_DIR)/multicast.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/multicast.mldoc

$(PROOF_DIR)/trace-cml.tex:	$(SRC_DIR)/trace-cml.mldoc $(INFO)
	$(PROOF) $(SRC_DIR)/trace-cml.mldoc

clean:
	rm -f $(PROOF_FILES)
	rm -f $(INFO_DIR)/Master.info $(INFO_FILES)
	rm -f $(HTML_FILES)

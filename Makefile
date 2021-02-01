# Makefile for Tootsville
#
# Targets:
#
# deps — install prerequisites needed from the build-deps file
#
# bin — compile the deployment files (CGI script, compiled JS/CSS, &c)
#
# doc — extract and build documentation in various formats
#
# doc-publish — push docs to Goethe
#
# test — run automated unit tests
#
# ui-test — run Selenium-based unit tests
#
# deploy — send code to production server, compile and test
#
# 
#
# Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021 The Corporation
# for Inter-World Tourism and Adventuring (ciwta.org).
#
# This program is  Free Software: you can redistribute  it and/or modify
# it  under the  terms  of  the GNU  Affero  General  Public License  as
# published by  the Free  Software Foundation; either  version 3  of the
# License, or (at your option) any later version.
#
# This program  is distributed in the  hope that it will  be useful, but
# WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
# MERCHANTABILITY  or FITNESS  FOR  A PARTICULAR  PURPOSE.  See the  GNU
# Affero General Public License for more details.
#
# You  should have  received a  copy of  the GNU  Affero General  Public
# License     along    with     this     program.     If    not,     see
# <https://www.gnu.org/licenses/>.
#
# You can reach CIWTA at https://ciwta.org/, or write to us at:
#
# PO Box 23095
#
# Oakland Park, FL 33307-3095
#
# USA
#
# 

all:	bin doc

clean:
	-rm -rf Tootsville Tootsville.new \
		doc/* bin/buildapp
	-rm -rf $$HOME/.cache/common-lisp/*/$$(pwd)
	find . -name \\*.fasl -exec rm {} \;
	find . -name dumper\\*.lisp -exec rm {} \;
	find . -name \\*~ -exec rm {} \;
	find . -name \\*.orig -exec rm {} \;
	-rm -f TODO.org TODO.scorecard	
	git gc --auto || true

modules:	.gitmodules $(shell  grep '\[submodule "' .gitmodules | \
			cut -d '"' -f 2 | sed -e 's,^,../.git/modules/,g' -e 's,$$,/config,g')
	if [ -d ../.git ]; then cd .. ; git submodule update --init; fi

../.git/modules/%/config:	.gitmodules
	(cd .. ; git submodule update --init )

#TAGS:	$(shell find . -name \\*.lisp)
#	ctags --languages=lisp -e -R -f TAGS

deploy:	bin test deploy-servers

VERSION=$(shell grep :version tootsville.asd | cut -d '"' -f 2)
doc-publish:	doc
	rsync -rv -essh doc/* goethet@goethe.tootsville.org:goethe.tootsville.org/devel/docs/Tootsville/$(VERSION)/

bin:	Tootsville

~/quicklisp/setup.lisp:	src/setup.lisp
	sbcl --non-interactive \
		--load src/setup.lisp \
		--quit

../.gitmodules:
	>> ../.gitmodules

bin/buildapp:
	if which buildapp; \
	then \
		ln -s $$(which buildapp) -f bin/buildapp; \
	else \
		sbcl --non-interactive \
			--load ~/quicklisp/setup.lisp \
			--eval '(ql:quickload :buildapp)' \
			--eval '(buildapp:build-buildapp "bin/buildapp")'; \
	fi

Tootsville:	Tootsville.new
	./Tootsville.new check
	mv --backup=simple Tootsville.new Tootsville

Tootsville.new:	deps quicklisp-manifest.tmp bin/buildapp
	bin/buildapp --output ../Tootsville.new \
		--manifest-file quicklisp-manifest.tmp \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--entry Tootsville:entry

quicklisp-manifest.tmp:	tootsville.asd \
		$(shell find . -name \\*.lisp \
			-and -not -path \\**/.\\* \
			-or -name \\*.asd \
			-and -not -path \\**/.\\*)
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.tmp")'
	for asd in src/lib/*/*.asd ; \
	do \
		echo $$(pwd)/$$asd >> quicklisp-manifest.tmp ;\
	done
	echo $$(pwd) >> quicklisp-manifest.tmp

../tootsville.org/dist/doc.texi:
	$(MAKE) -C ../tootsville.org dist/doc.texi

doc/Tootsville.texi:	Tootsville ../tootsville.org/dist/doc.texi
	./Tootsville write-docs

install:	tootsville.service
	chcon system_u:object_r:bin_t:s0 /home/pil/tootsville.net/Tootsville
	cp tootsville.service --backup=simple -f /usr/lib/systemd/system/
	restorecon /usr/lib/systemd/system/tootsville.service
	cp 55-tootsville.conf -f /etc/rsyslog.conf/ || echo "No rsyslogd"
	systemctl reload-daemon
	systemctl restart tootsville

####################

deps:	.deps~

.deps~:	build/build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.deps~

####################

test:	./Tootsville
	./Tootsville check

#################### vars

# To target alternate clusters:
#	make CLUSTER=qa
#	make CLUSTER=.
CLUSTER:=test
ifeq ($(CLUSTER),.)
clusternet=tootsville.net
else
clusternet=$(CLUSTER).tootsville.net
endif

GAMEHOSTS=game1
BALANCERS=balancer1 balancer2

LOCAL_USERNAME=$(shell whoami)
REVISION=$(shell git log -n 1 --pretty=format:"%H")
REALNAME:=$(shell if which finger &>/dev/null ;\
	then \
	    REALNAME=$$(finger $$LOCAL_USERNAME | perl -ne 'if (/Name: (.*)[\t\n]/){print $$1; exit}')  ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(grep ^$$LOCAL_USERNAME: /etc/passwd | cut -d: -f5 | cut -d, -f1) ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(whoami) ;\
	fi ;\
	echo $$REALNAME)

# Rollbar
ACCESS_TOKEN=7c28543f4257495694b50fe59acb2ada

#################### servers

servers:	./Tootsville

#################### doc

doc:	server-doc

server-doc: \
	doc/Tootsville.pdf \
	doc/Tootsville.html.tar.gz \
	doc/Tootsville.html.d/index.html \
	doc/Tootsville.txt \
	doc/index.html

doc/index.html:	src/doc/index.html
	mv src/doc/index.html doc/index.html  

doc-install-info:	doc/Tootsville.info
	install-info doc/Tootsville.info /usr/local/share/info/dir


doc/Tootsville.html.tar.gz:	doc/Tootsville.html.tar
	gzip -9 -c < $< > $@

doc/Tootsville.html.tar.Z:	doc/Tootsville.html.tar
	compress -9 -c < $< > $@

doc/Tootsville.html.tar.bz2:	doc/Tootsville.html.tar
	bzip2 -9 -c < $< > $@

doc/Tootsville.html.tar.xz:	doc/Tootsville.html.tar
	xz -9 -c < $< > $@

doc/Tootsville.html.tar:	doc/Tootsville.html.d/index.html
	cd doc; tar cf Tootsville.html.tar Tootsville.html.d

doc/Tootsville.html.zip:	doc/Tootsville.html.d/index.html
	cd doc; zip -9 Tootsville.html.zip Tootsville.html.d

doc/Tootsville.html.d/doc-style.css:	src/doc/doc-style.css
	mkdir -p doc/Tootsville.html.d/
	cp $< $@

doc/Tootsville.html.d/index.html:	doc/Tootsville.texi doc/Tootsville.html.d/doc-style.css
	emacsclient -e '(with-current-buffer (find-file "doc/Tootsville.texi") (texinfo-all-menus-update) (texinfo-every-node-update) (texinfo-master-menu t) (save-buffer) (kill-buffer))'
	perl texi-to-html

doc/Tootsville.ps:	doc/Tootsville.pdf
	cd doc; pdf2ps Tootsville.pdf

doc/Tootsville.pdf:	doc/Tootsville.texi
	cd doc; PDFLATEX=xelatex makeinfo --pdf Tootsville.texi 

doc/Tootsville.txt:	doc/Tootsville.texi
	cd doc; makeinfo --plaintext -o Tootsville.txt Tootsville.texi

doc/Tootsville.info:	doc/Tootsville.texi
	emacsclient -e '(with-current-buffer (find-file "doc/Tootsville.texi") (texinfo-all-menus-update) (texinfo-every-node-update) (texinfo-master-menu t) (save-buffer) (kill-buffer))'
	cd doc; makeinfo -o Tootsville.info Tootsville.texi

doc/doc.css:	build/doc.less

all-docs: \
	doc/Tootsville.html.tar.gz	\
	doc/Tootsville.html.tar.Z	\
	doc/Tootsville.html.tar.bz2	\
	doc/Tootsville.html.tar.xz	\
	doc/Tootsville.html.zip	\
	doc/Tootsville.ps	\
	doc/Tootsville.pdf	\
	doc/Tootsville.txt 	\
	doc/Tootsville.info

#################### TODO

TODO.org:	$(shell find . -name \\*.lisp -o -name \\*.css -o -name \\*.js -o -name \\*.org -o \
		   -name \\*.texi -o -name \\*.asd -o -name \\*.txt -o -name \\*.html) \
		 README.org
	-mv TODO.org TODO.org~ 2>/dev/null
	echo '* TODO-type notes found $$(date +%Y-%m-%d)' > TODO.org
	echo '' >> TODO.org
	echo '** FIXME Actual bugs!' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn FIXME servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** TODO To be done ASAP' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn TODO servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** XXX Might Be Nice to do someday' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn XXX servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** ☠☠☠ Bruce-Robert should examine this' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn ☠☠☠: servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org

TODO.scorecard:	$(shell find . \( -name \\*.lisp -o -name \\*.asd \) -and -not -name .\\*) \
	README.org
	echo -n 'TOOTS_FIXME=' > TODO.scorecard
	git grep FIXME *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_TODO=' >> TODO.scorecard
	git grep TODO *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_XXX=' >> TODO.scorecard
	git grep XXX *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_BRP=' >> TODO.scorecard
	git grep ☠☠☠ *.lisp *.asd src/
	 | wc -l >> TODO.scorecard

#################### dev-test

devel-test:	devel-serve

devel-serve:	./Tootsville
	./Tootsville server < /dev/null

#################### deploy

deploy-servers:	predeploy-servers
	for host in $(GAMEHOSTS) ; \
	do \
		echo " » Deploy $$host.$(clusternet)" ;\
		ssh root@$$host.$(clusternet) dnf -y update ;\
                    scp ~/.config/Tootsville/Tootsville.config.lisp $$host.$(clusternet):.config/Tootsville ;\
		ssh $$host.$(clusternet) make -k -C ~pil/tootsville.net Tootsville ;\
		ssh root@$$host.$(clusternet) make -k -C ~pil/tootsville.net install ;\
		ssh root@$$host.$(clusternet) systemctl daemon-reload ;\
		ssh root@$$host.$(clusternet) systemctl restart tootsville ;\
		ssh root@$$host.$(clusternet) ~pil/tootsville.net/bin/make-secure ;\
		VERSION=$(shell ssh $$host.$(clusternet) Tootsville version-info version) ;\
		curl https://api.rollbar.com/api/1/deploy/ \
		     -F access_token=$(ACCESS_TOKEN) \
		     -F environment=$$host.$(clusternet) \
		     -F framework=gmake \
		     -F notifier.name=gmake \
		     -F revision=$(REVISION) \
		     -F comment="v $(VERSION)" \
		     -F uuid=$(uuidgen) \
		     -F local_username=$(LOCAL_USERNAME) ;\
		echo "Deployment successful" ;\
	done

predeploy:	no-fixmes predeploy-servers

connectivity:
	echo " » Test connectivity"
	for host in $(GAMEHOSTS); do \
	   ssh $$host.$(clusternet) sbcl --no-userinit --quit | grep 'This is SBCL'; \
	done

no-fixmes:	TODO.scorecard
	TOOT_TODO=$$(grep TODO TODO.scorecard | wc -l) ;\
	TOOT_FIXME=$$(grep FIXME TODO.scorecard | wc -l) ;\
	if [[ $$TOOT_FIXME -gt 0 ]] ;\
	then \
			clear ;\
			echo "There are $$TOOT_FIXME FIXME comments!" ;\
			if [[ "$(CLUSTER)" = . ]] ;\
			then \
				echo " ✗ Refusing to deploy to Production with FIXME notes" ;\
				exit 8 ;\
			fi ;\
			echo "" ;\
			echo "$(REALNAME), are you sure you want to deploy to $(clusterorg)" ;\
			echo "when there are $$TOOT_FIXME FIXME notes and $$TOOT_TODO TODOs?" ;\
			echo "" ;\
			read -p "Deploy anyway? (Y/N) ⇒ " -n 1 yorn ;\
			if [[ "$${yorn}" = "y" ]] ;\
			then \
				echo "" ;\
				echo "Overridden" ;\
				echo "" ;\
				echo "Override accepted. Good luck …" ;\
				break ;\
			elif [[ "$${yorn}" = "n" ]] ;\
			then \
				echo "" ;\
				echo "That seems wise. Exiting." ;\
				echo "" ;\
				exit 8 ;\
			else \
				echo "Only y or n work. Treading $$yorn as No." ; \
				exit 8; \
			fi ;\
	fi

predeploy-servers:	servers quicklisp-update-servers
	for host in $(GAMEHOSTS) ;\
	do \
		echo " » Pre-deploy $$host.$(clusternet)" ;\
		rsync -essh --exclude '*~' --exclude 'Tootsville' --delete -zar * .??* \
			$$host.$(clusternet):tootsville.net/ ;\
		ssh $$host.$(clusternet) rm tootsville.net/*~ ;\
		ssh $$host.$(clusternet) make -C tootsville.net clean || exit 6 ;\
		ssh $$host.$(clusternet) sbcl --non-interactive \
			--load '~/quicklisp/setup.lisp' \
			--eval "'(ql:update-all-dists :prompt nil)'" || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.net Tootsville || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.net test || exit 6 ;\
	done

quicklisp-update-servers:
	for host in $(GAMEHOSTS) ; \
	do \
		echo " » Ensure latest Quicklisp on $$host.$(clusternet)" ;\
	    ssh $$host.$(clusternet) \
	        sbcl --non-interactive \
	        --no-inform \
	        --eval "'(ql:update-client)'" \
	        --eval "'(ql:update-all-dists :prompt nil)'" \
	        --quit ;\
	done

remotes:
	if ! git remote -v | grep -q github &>/dev/null ;\
	then \
		git remote add github git@github.com:adventuring/tootsville.net ;\
	fi
	if ! git remote -v | grep -q gitlab &>/dev/null ;\
	then \
		git remote add gitlab git@gitlab.com:adventuring/tootsville.net ;\
	fi
	if ! git remote -v | grep -q goethe &>/dev/null ;\
	then \
		git remote add goethe goethe.Tootsville.org:devel/git/tootsville.net ;\
	fi

bump-next-version:
	git status | grep modified: | grep ./tootsville.asd && exit 9 || :
	perl -pne 's/:version "(\d+\.\d+)\.(\d+)"/ ":version \"$$1." . (1+ $$2) . "\"" /e' -i ./tootsville.asd
	git add ./tootsville.asd
	git commit -m "bump version number for next build"

git-tag-deployment:
	VERSION=$$(./Tootsville version-info version) ;\
	now=$$(date +%Y-%m-%d) ;\
	msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	if git rev-parse v$$VERSION &>/dev/null ;\
	then \
	    echo "Previous tag v$$VERSION found, adding v$$VERSION-$$now" ;\
	    if git rev-parse v$$VERSION-$$now &>/dev/null ;\
	    then \
	        now=$$(date +%Y-%m-%d.%H%M) ;\
	        msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	        echo " - I meant v$$VERSION-$$now" ;\
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.net: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    else \
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.net: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    fi ;\
	else \
	    echo "First deploy of v$$VERSION, tagging" ;\
	    git submodule foreach git tag -a tootsville-v$$VERSION -m "for Tootsville.net: $$msg" ;\
	    git tag -a v$$VERSION -m "$$msg" ;\
	fi

	git push --tags origin ||:
	git submodule foreach --recursive 'git push --tags origin ||:'
	git push --tags github ||:
	git submodule foreach --recursive 'git push --tags github ||:'
	# git push --tags gitlab ||:
	# git submodule foreach --recursive 'git push --tags gitlab ||:'
	# git push --tags goethe ||:
	# git submodule foreach --recursive 'git push --tags goethe ||:'

	$(MAKE) bump-next-version

#################### deploy-docs

deploy-docs:
	make doc-publish
	scp ../tootsville.org/dist/htaccess.all/goethe.tootsville.net.htaccess goethe.tootsville.org:goethe.tootsville.org/.htaccess
	scp ../tootsville.org/www/favicon.??? goethe.tootsville.org:goethe.tootsville.org/
	rsync -essh -zar ../tootsville.org/www/error goethe.tootsville.org:goethe.tootsville.org/

####################

TAGS:	$(shell find . -type f -name \\*.lisp)
	etags --declarations $(shell find . -type f -name *.lisp) Makefile

